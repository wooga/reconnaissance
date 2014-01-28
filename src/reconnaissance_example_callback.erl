-module(reconnaissance_example_callback).

-compile(export_all).

request() ->
    <<"hello">>.

response(_IP, _Port, Request) ->
    list_to_binary(atom_to_list(node())).

handle_response(IP, Port, Response) ->
    {IP, Port, Response}.

