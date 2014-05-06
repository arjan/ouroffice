%% Office scanner which uses the unify API to get a list of IP
%% addresses + clients, then does a ping on those.
-module(ouroffice_scanner_unify).

-behaviour(ouroffice_scanner).

-export([scan/1]).

-include_lib("xmerl/include/xmerl.hrl").

priv_dir(Mod) ->
    Ebin = filename:dirname(code:which(Mod)),
    filename:join(filename:dirname(Ebin), "priv").

scan(Opts) ->
    Host = proplists:get_value(host, Opts, "unify"),
    User = proplists:get_value(user, Opts, "admin"),
    Password = proplists:get_value(password, Opts, "admin"),
    Cmd = priv_dir(?MODULE) ++ "/unify-clients -c " ++ Host ++ " -u " ++ User ++ " -p " ++ Password,
    Output = os:cmd(Cmd),
    {All, HostString} = lists:mapfoldl(
                          fun(Line, Hosts) ->
                                  case string:tokens(Line, "\t") of
                                      [Mac, Addr] ->                                      
                                          {
                                        [{addr, z_convert:to_binary(Addr)},
                                         {mac, z_convert:to_binary(Mac)}],
                                        Addr ++ " " ++ Hosts};
                                      _ ->
                                          {undefined, Hosts}
                                  end
                          end,
                          [],
                          string:tokens(Output, "\n")),
    OutputXml = os:cmd("sudo nmap -oX - -sP " ++ HostString),
    {RootElem, _} = xmerl_scan:string(OutputXml),
    file:write_file("/tmp/unifyhosts.xml", OutputXml),
    UpHosts = [ip(H) || H <- xmerl_xpath:string("//host[status[@state=\"up\"]]", RootElem)],
    lists:filter(fun(undefined) -> false;
                    ([{addr,A}|_]) ->
                         lists:member(A, UpHosts)
                 end,
                 All).

ip(undefined) ->
    undefined;
ip(HostElem) ->
    [IpElem] = xmerl_xpath:string("address[@addrtype=\"ipv4\"]", HostElem),
    z_convert:to_binary(xml_attrib(addr, IpElem)).

xml_attrib(Name, #xmlElement{attributes=Attrs}) ->
    case lists:filter(fun(#xmlAttribute{name=Nm}) -> Nm =:= Name end, Attrs) of
        [] -> undefined;
        [#xmlAttribute{value=Value}|_] ->
            list_to_binary(Value)
    end.
    
