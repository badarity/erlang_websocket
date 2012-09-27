%% 
%% Basic implementation of the WebSocket API:
%% http://dev.w3.org/html5/websockets/
%% However, it's not completely compliant with the WebSocket spec.
%% Specifically it doesn't handle the case where 'length' is included
%% in the TCP packet, SSL is not supported, and you don't pass a 'ws://type url to it.
%%
%% It also defines a behaviour to implement for client implementations.
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(websocket_client).

-behaviour(gen_server).

%% API
-export([start/4,start/5,write/2,close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Ready States
-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSED,2).
-define(HANDSHAKE,3).

%% Behaviour definition
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},{ws_onmessage,2},{ws_onopen,1},
     {ws_onclose,1},{ws_oninfo,2}];
behaviour_info(_) ->
    undefined.

-record(state, {socket, readystate = undefined,
                headers = [], callback, callback_state, buffer = []}).

start(Host,Port,Mod,Args) ->
  start(Host,Port,"/",Mod,Args).
  
start(Host,Port,Path,Mod,Args) ->
    gen_server:start_link(?MODULE, [{Host,Port,Path,Mod,Args}], []).

init(Args) ->
    [{Host,Port,Path,Mod,CArgs}] = Args,
    {ok, Sock} = gen_tcp:connect(Host,Port,[binary,{packet, http},{active,true}]),
    Req = initial_request(Host,Path),
    ok = gen_tcp:send(Sock,Req),
    {ok, CState} = Mod:init(CArgs),
    
    {ok,#state{socket=Sock,callback=Mod,callback_state = CState}}.

%% Write to the server
write(Pid, Data) ->
    gen_server:cast(Pid,{send,Data}).

%% Close the socket
close(Pid) ->
    gen_server:cast(Pid,close).

handle_cast({send,Data}, State) ->
    gen_tcp:send(State#state.socket,[0] ++ Data ++ [255]),
    {noreply, State};

handle_cast(close,State) ->
    State1 = callback_onclose(State),
    gen_tcp:close(State1#state.socket),
    State2 = State1#state{readystate=?CLOSED},
    {stop,normal,State2}.

%% Start handshake
handle_info({http,Socket,{http_response,{1,1},101,"WebSocket Protocol Handshake"}}, State) ->
    State1 = State#state{readystate=?CONNECTING,socket=Socket},
    {noreply, State1};

%% Extract the headers
handle_info({http,Socket,{http_header, _, Name, _, Value}},State) ->
    case State#state.readystate of
	?CONNECTING ->
	    H = [{Name,Value} | State#state.headers],
	    State1 = State#state{headers=H,socket=Socket},
	    {noreply,State1};
	undefined ->
	    %% Bad state should have received response first
	    {stop,error,State}
    end;

%% Once we have all the headers check for the 'Upgrade' flag 
handle_info({http,Socket,http_eoh},State) ->
    %% Validate headers, set state, change packet type back to raw
     case State#state.readystate of
	?CONNECTING ->
	     Headers = State#state.headers,
	     case proplists:get_value('Upgrade',Headers) of
		 "WebSocket" ->
		     inet:setopts(Socket, [{packet, raw}]),
		     State1 = State#state{readystate=?HANDSHAKE,socket=Socket},
		     {noreply,callback_onopen(State1)};
		 _Any  ->
		     {stop,error,State}
	     end;
	undefined ->
	    %% Bad state should have received response first
	    {stop,error,State}
    end;

%% Handshake complete, handle packets
handle_info({tcp, _Socket, <<_:128, Rest/binary>>},
            #state{readystate = ?HANDSHAKE} = State) ->
    {Msgs, BufferedState} = unframe(binary_to_list(Rest), State),
    OpenState = BufferedState#state{readystate = ?OPEN},
    {noreply, callback_send_messages(Msgs, OpenState)};

handle_info({tcp, _Socket, Data},State) ->
    case State#state.readystate of
	?OPEN ->
	    {Msgs, BufferedState} = unframe(binary_to_list(Data), State),
	    {noreply, callback_send_messages(Msgs, BufferedState)};
	_Any ->
	    {stop,error,State}
    end;

handle_info({tcp_closed, _Socket},State) ->
    {stop,normal,callback_onclose(State)};

handle_info({tcp_error, _Socket, _Reason},State) ->
    {stop,tcp_error,State};

handle_info(Something, State) ->
    {noreply, callback_oninfo(Something, State)}.
  
handle_call(_Request,_From,State) ->
    {reply,ok,State}.

terminate(Reason, _State) ->
    error_logger:info_msg("Websocket Client Terminated ~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

callback_send_messages(Msgs, State) ->
    Sender = fun(Msg, PrevState) -> callback_onmessage(Msg, PrevState) end,
    lists:foldl(Sender, State, Msgs).

callback_onopen(State) ->
    callback_default_call(ws_onopen, [], State).

callback_onmessage(Msg, State) ->
    callback_default_call(ws_onmessage, [Msg], State).

callback_oninfo(Msg, State) ->
    callback_default_call(ws_oninfo, [Msg], State).

callback_onclose(State) ->
    callback_closed_reply(callback_apply(ws_onclose, [], State), State).

callback_closed_reply({ok, _CState} = Rep, State) ->
    callback_reply(Rep, State).

callback_default_call(Fun, Args, State) ->
    callback_reply(callback_apply(Fun, Args, State), State).

callback_apply(Fun, Args, #state{callback = Mod, callback_state = State}) ->
    apply(Mod, Fun, Args ++ [State]).

callback_reply({ok, CState}, State) ->
    State#state{callback_state = CState};
callback_reply({reply, Msg, CState}, State) ->
    write(self(), Msg),
    State#state{callback_state = CState};
callback_reply({close, CState}, State) ->
    close(self()),
    State#state{callback_state = CState}.

initial_request(Host,Path) ->
    "GET "++ Path ++" HTTP/1.1\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" ++ 
	"Host: " ++ Host ++ ":8081\r\n" ++
	"Origin: http://" ++ Host ++ ":8081/\r\n" ++
    "Sec-WebSocket-Key1: " ++ sec_key() ++ "\r\n" ++
    "Sec-WebSocket-Key2: " ++ sec_key() ++ "\r\n" ++
    "\r\n" ++ binary_to_list(<<45:64>>). %% HARDCODE

sec_key() ->
    "18x 6]8vM;54 *(5:  {   U1]8  z [  8". %% HARDCODE

unframe(Msg, #state{buffer = []} = State) ->
    unframe_result(unframe2(Msg, [], []), State);

unframe(Msg, #state{buffer = Buf} = State) ->
    unframe_result(unframe1(Msg, [], Buf), State).

unframe_result({Msgs, Buffer}, State) ->
    {lists:reverse(Msgs), State#state{buffer = Buffer}}.

unframe1([255 | Msg], Msgs, ByteAcc) ->
    unframe2(Msg, [lists:reverse(ByteAcc) | Msgs], []);
unframe1([Byte | Msg], Msgs, ByteAcc) ->
    unframe1(Msg, Msgs, [Byte | ByteAcc]);
unframe1([], Msgs, ByteAcc) ->
    {Msgs, ByteAcc}.

unframe2([0 | Msg], Msgs, ByteAcc) ->
    unframe1(Msg, Msgs, ByteAcc);
unframe2([], Msgs, ByteAcc) ->
    {Msgs, ByteAcc}.

