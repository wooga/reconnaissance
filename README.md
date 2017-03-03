reconnaissance
=== 

reconnaissance is a small Erlang application for node discovery in a local network.

# Building

```
git clone git@github.com:wooga/reconnaissance.git
cd reconnaissance
./rebar com
```

# Usage

reconnaissance comes preconfigured with a callback module that sends the host, port and node name:

When started twice with `erl -pz ebin -s reconnaissance` you can use `reconnaissance:discover().` to find other nodes also having reconaissance running:

```
reconnaissance:discover().
[{{172,19,255,43},63616,<<"nonode@nohost">>}]
```

Here is the build-in callback module:

```
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
```

The API calls are:

1. `reconnaissance:discover/0` meaning "find other nodes, wait half a second for responses and give me the list"
2. `reconnaissance:discover/1` use a server name other than `reconnaissance`
3. `reconnaissance:discover/2` also specify the time to wait in ms.

Et voilà: reconnaissance!

# Configuration

Use in the environment:
```
{reconnaissance, [
    {port, 31337},
    {callback_module, reconnaissance_example_callback}
]}
```

Make sure multicast is routed propery by the OS:

```sudo route add 224.0.0.1 10.8.0.3```
