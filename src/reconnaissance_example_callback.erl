-module(reconnaissance_example_callback).

-compile(export_all).

% Creates the payload that is sent as multicast packet
request() ->
    <<"hello">>.

% Request is the payload from the request() function above
% The output is the response payload
response(_IP, _Port, Request) ->
    list_to_binary(atom_to_list(node())).

% Called when the response from discoverd nodes is received
% Output will be stored in the node list
handle_response(IP, Port, Response) ->
    {IP, Port, Response}.
