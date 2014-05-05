%% Office scanner which scans the subnet for ARP lookups.
-module(ouroffice_scanner_nmap).

-behaviour(ouroffice_scanner).

-export([scan/1]).

-include_lib("xmerl/include/xmerl.hrl").

scan(Opts) ->
    Subnet = proplists:get_value_env(subnet, Opts, "192.168.10.0/24"),
    Output = os:cmd("sudo nmap -oX - -sP " ++ Subnet),
    {RootElem, _} = xmerl_scan:string(Output),
    file:write_file("/tmp/network.xml", Output),
    Hosts0 = [hostinfo(H) || H <- xmerl_xpath:string("//host[status[@state=\"up\"]]", RootElem)],
    lists:filter(fun(undefined) -> false; (_) -> true end, Hosts0).

hostinfo(HostElem) ->
    [StatusElem] = xmerl_xpath:string("status", HostElem),
    case xml_attrib(reason, StatusElem) =:= <<"arp-response">> andalso xml_attrib(state, StatusElem) =:= <<"up">> of
        true ->
            %% up
            [IpElem] = xmerl_xpath:string("address[@addrtype=\"ipv4\"]", HostElem),
            [MacElem] = xmerl_xpath:string("address[@addrtype=\"mac\"]", HostElem),
            [{addr, xml_attrib(addr, IpElem)},
             {mac, z_convert:to_binary(z_string:to_lower(xml_attrib(addr, MacElem)))}];
        false ->
            %% down
            undefined
    end.

xml_attrib(Name, #xmlElement{attributes=Attrs}) ->
    case lists:filter(fun(#xmlAttribute{name=Nm}) -> Nm =:= Name end, Attrs) of
        [] -> undefined;
        [#xmlAttribute{value=Value}|_] ->
            list_to_binary(Value)
    end.
    
