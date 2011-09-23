h2. Erlang WebSocket Server and Client


h3. WebSocket Server

The server is a simple wrapper around the mochiweb_socket_server to allow "WebSocket":http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-66 connections.  With this wrapper, you can write WebSocket connection loops just like you'd write a normal mochiweb application.  Here's an example (included with the source code):

<pre>
 <code>
-module(basic_websocket).

-export([start/1, stop/0, loop/1]).

start(Options) ->
    Loop = fun (WebSocket) ->
                   ?MODULE:loop(WebSocket)
           end,
    mochiweb_websocket:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_websocket:stop(?MODULE).


loop(WebSocket) ->
    %% Get the data sent from the client
    Data = WebSocket:get_data(),
    
    %% For this example...
    %% Of course you could handle JSON or another format of your choice
    case Data of
	%% On initial connect we get this message
	"client-connected" ->
	    WebSocket:send("You are connected!");
	%% Other messages go here
	Other ->
	    Msg = "You Said: " ++ Other,
	    WebSocket:send(Msg)
    end.
 </code>
</pre> 

h3. WebSocket Client

This is a pure Erlang WebSocket client you can use to call a WebSocket Server from Erlang code. For simplicity, the code defines an Erlang Behaviour you implement for your client. The Behaviour defines the functions outlined in the WebSocket API spec.  Here's an example of a simple client:

<pre>
 <code>

-module(sample).
-behaviour(websocket_client).
-export([start/0]).
%% websocket specific callbacks
-export([ws_onmessage/1,ws_onopen/0,ws_onclose/0,ws_close/0,ws_send/1]).

ws_send(Data) ->
    websocket_client:write(Data).

ws_start() ->
    websocket_client:start("localhost",8002,?MODULE).

%% Handle incoming messages here
ws_onmessage(Data) ->
    io:format("Got some data:: ~p~n",[Data]).

ws_onclose() ->
    io:format("Connection closed~n").

ws_onopen() ->
    io:format("Connection open~n"),
    send("client-connected").

ws_close() ->
    websocket_client:close().

 </code>
</pre>

Here's an example of using the client from an erl shell:

<pre>
 <code>
> sample:ws_start().
> sample:ws_send("Hello there").
> sample:ws_close().
 </code>
</pre>

Check out the examples directory. 

h3. Requirements:
 
 # Erlang 12.5 or greater
 # Google Chrome Browser developer channel release 4.0.249.0


h3. Getting Started:
 
 # download the code
 # CD into the erlang_websocket directory
 # run make

h3. Run the example:

 # CD into the examples/basic directory
 # run the 'start.sh' script
 # Point the Chrome Browser to 'http://localhost:8000'


    
 
