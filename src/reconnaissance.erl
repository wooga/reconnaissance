-module(reconnaissance).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, discover/1, discover/2]).

-record(state, {port, send_socket, receive_socket, encode_cb, handle_cb, nodes = []}).

-define(ADDRESS,  {224, 0, 0, 1}).

%% Public API

start() ->
    application:start(reconnaissance).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

discover(Server) ->
    discover(500, Server).

discover(WaitTime, Server) ->
    gen_server:cast(discover, Server),
    timer:sleep(WaitTime),
    gen_server:call(Server, nodes).

%% Callbacks

init([]) ->
    process_flag(trap_exit, true),
    % TODO CONFIG PORT
    Port          = 9876,
    ReceiveSocket = receive_socket(Port),
    SendSocket    = send_socket(),
    EncodeCB      = fun() -> <<"hello">> end,
    HandleCB      = fun(Packet) -> io:format("GOT: ~p", [Packet]) end,
    {ok, #state{
            port           = Port,
            send_socket    = SendSocket,
            receive_socket = ReceiveSocket,
            encode_cb      = EncodeCB,
            handle_cb      = HandleCB}}.

handle_call(nodes, _From, State) ->
    {reply, State#state.nodes, State}.

handle_cast(discover, State) ->
    Message = State#state.encode_cb,
    ok = gen_udp:send(State#state.send_socket,
          ?ADDRESS,
          State#state.port,
          Message()),
    {noreply, State#state{ nodes = [] }}.

handle_info({udp, Socket, IP, InPortNo, Packet}, State = #state{
            receive_socket = Socket,
            handle_cb      = HandleCB,
            nodes          = Nodes }) ->

    NodeData = HandleCB(IP, InPortNo, Packet),
    NodesNew = [NodeData | Nodes],
    {noreply, State#state{ nodes = NodesNew }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_socket() ->
    {ok, SendSocket} = gen_udp:open(0, [{ip, {0, 0, 0, 0}}]),
    SendSocket.

receive_socket(Port) ->
    Opts = [
        {active, true},
        {ip, ?ADDRESS},
        {add_membership, {?ADDRESS, {0, 0, 0, 0}}},
        {reuseaddr, true},
        binary],
    {ok, ReceiveSocket} = gen_udp:open(Port, Opts),
    ReceiveSocket.
