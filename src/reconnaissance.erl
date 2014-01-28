-module(reconnaissance).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/2, discover/0, discover/1, discover/2]).

-record(state, {port, send_socket, receive_socket, response_port, callback_module, nodes = []}).

-define(ADDRESS,  {224, 0, 0, 1}).
%% Public API

start() ->
    application:start(reconnaissance).

start_link(Port, CallbackModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, CallbackModule], []).

discover() ->
    discover(?MODULE).

discover(Server) ->
    discover(500, Server).

discover(WaitTime, Server) ->
    gen_server:cast(Server, discover),
    timer:sleep(WaitTime),
    gen_server:call(Server, nodes).

%% Callbacks

init([Port, CallbackModule]) ->
    process_flag(trap_exit, true),
    ReceiveSocket  = receive_socket(Port),
    SendSocket     = send_socket(),
    {ok, #state{
            port            = Port,
            send_socket     = SendSocket,
            receive_socket  = ReceiveSocket,
            callback_module = CallbackModule}}.


handle_call(nodes, _From, State) ->
    {reply, State#state.nodes, State}.


handle_cast(discover, State = #state{ callback_module = CallbackModule }) ->
    Request = CallbackModule:request(),
    Prefix  = <<"REQ">>,
    ok = gen_udp:send(State#state.send_socket,
          ?ADDRESS,
          State#state.port,
          << Prefix/binary, Request/binary >>),
    {noreply, State#state{ nodes = [] }}.


handle_info({udp, _, IP, InPortNo, << "REQ", Payload/binary >>}, State = #state{
    callback_module = CallbackModule}) ->

    Response        = CallbackModule:response(IP, InPortNo, Payload),
    Prefix          = <<"RSP">>,
    ResponsePort    = send_response(IP, InPortNo, << Prefix/binary, Response/binary >>),
    {noreply, State#state{ response_port = ResponsePort }};

handle_info({udp, _, IP, Port, << "RSP", Payload/binary >>}, State = #state{
    callback_module = CallbackModule,
    nodes           = Nodes,
    response_port   = ResponsePort }) ->

    NodesNew = case originates_from_self(IP, Port, ResponsePort) of
        true ->
            Nodes;
        false ->
            Response = CallbackModule:handle_response(IP, Port, Payload),
            [Response | Nodes]
    end,
    {noreply, State#state{ nodes = NodesNew }}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_socket() ->
    {ok, SendSocket} = gen_udp:open(0, [{ip, {0, 0, 0, 0}}, binary]),
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


send_response(Host, Port, Response) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    gen_udp:send(Socket, Host, Port, Response),
    {ok, OutPort} = inet:port(Socket),
    gen_udp:close(Socket),
    OutPort.


originates_from_self(IP, InAndOutPort, InAndOutPort) ->
    lists:member(IP, local_ips());
originates_from_self(_, _, _) ->
    false.


local_ips() ->
    {ok, Addr} = inet:getifaddrs(),
    IPs = lists:flatten(
        lists:map(fun({_, Flags}) ->
            lists:map(fun({addr, IP}) -> IP; (_) -> undefined end, Flags) end,
        Addr)),
    lists:filter(fun(undefined) -> false; (_) -> true end, IPs).
