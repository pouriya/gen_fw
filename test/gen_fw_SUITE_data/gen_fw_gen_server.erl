-module(gen_fw_gen_server).

-include("gen_fw.hrl").

-export([start_link/0
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,fw_handle_message/2]).








start_link() ->
    gen_server:start_link(?MODULE, undefined, [{debug_options, [trace]}]).


init(_) ->
    {ok, undefined}.


handle_call(Msg, _, S) ->
    gen_fw_SUITE:reply(Msg),
    {reply, ok, S}.


handle_cast(Msg, S) ->
    gen_fw_SUITE:reply(Msg),
    {noreply, S}.


handle_info(Msg, S) ->
    gen_fw_SUITE:reply(Msg),
    {noreply, S}.


terminate(_, _) ->
    ok.


code_change(_, S, _) ->
    {ok, S}.


fw_handle_message(Type, Msg) ->
    io:format("Type: ~p~nMsg: ~p", [Type, Msg]),
    case Type of
        system ->
            ?ALLOW_TAG;
        cast ->
            gen_fw_SUITE:reply(Msg),
            ?DYNAMIC_TAG;
        call ->
            gen_fw_SUITE:reply(erlang:element(2, Msg)),
            ?DENY_TAG;
        message ->
            gen_fw_SUITE:reply(Msg),
            exit(oops)
    end.