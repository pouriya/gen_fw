-module(gen_fw_SUITE).

-include_lib("eunit/include/eunit.hrl").

-define(NAME, gen_fw_process).
-define(REGISTER_NAME, {local, ?NAME}).
-define(CALLBACK, gen_fw_gen_server).
-define(DEF_RULES, []).

-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export([reply/1]).

-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1]).

-include("gen_fw.hrl").


all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
        || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-9)].


init_per_suite(Config) ->
    Config.


end_per_suite(Config) ->
    Config.


init_per_testcase(_, Config) ->
    erlang:process_flag(trap_exit, true),
    Config.


end_per_testcase(_, _) ->
    catch erlang:exit(erlang:whereis(?NAME), kill),
    ok.


reply({From, Tag}) ->
    From ! {Tag, done},
    ok.


'1'(_) ->
    Result = gen_fw:start_link({?CALLBACK, start_link, []}, ?DEF_RULES),
    ?assertMatch({ok, _}, Result),
    {ok, Pid} = Result,
    ?assert(erlang:is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlang:exit(Pid, kill),

    Result2 = gen_fw:start({?CALLBACK, start_link, []}, ?DEF_RULES),
    ?assertMatch({ok, _}, Result2),
    {ok, Pid2} = Result2,
    ?assert(erlang:is_pid(Pid2)),
    ?assert(erlang:is_process_alive(Pid2)),
    erlang:exit(Pid2, kill),

    Result3 = gen_fw:start_link(?REGISTER_NAME, {?CALLBACK, start_link, []}, ?DEF_RULES),
    ?assertMatch({ok, _}, Result3),
    {ok, Pid3} = Result3,
    ?assertEqual(Pid3, erlang:whereis(?NAME)),
    ?assert(erlang:is_pid(Pid3)),
    ?assert(erlang:is_process_alive(Pid3)),
    erlang:exit(Pid3, kill),
    timer:sleep(1000),

    Result4 = gen_fw:start(?REGISTER_NAME, {?CALLBACK, start_link, []}, ?DEF_RULES),
    ?assertMatch({ok, _}, Result4),
    {ok, Pid4} = Result4,
    ?assertEqual(Pid4, erlang:whereis(?NAME)),
    ?assert(erlang:is_pid(Pid4)),
    ?assert(erlang:is_process_alive(Pid4)),
    erlang:exit(Pid4, kill).


'2'(_) ->
    ?assertEqual({ok, {?ALLOW_TAG, ?DENY_TAG, ?DYNAMIC_TAG, ?DENY_TAG}}
                ,gen_fw:check_rules([{system, ?DENY_TAG}
                                    ,{message, ?DYNAMIC_TAG}
                                    ,{call, ?DENY_TAG}
                                    ,{cast, ?ALLOW_TAG}])),
    ?assertMatch({ok, {?DYNAMIC_TAG, _, _, _}}
                ,gen_fw:check_rules([{system, ?DENY_TAG}
                                    ,{cast, ?ALLOW_TAG}
                                    ,{message, ?DYNAMIC_TAG}
                                    ,{cast, ?DENY_TAG}
                                    ,{call, ?DENY_TAG}
                                    ,{cast, ?DYNAMIC_TAG}])),
    ?assertMatch({ok, {?ALLOW_TAG, ?ALLOW_TAG, ?ALLOW_TAG, ?ALLOW_TAG}}, gen_fw:check_rules([])),

    ?assertEqual({ok, {?DYNAMIC_TAG, ?DYNAMIC_TAG, ?DYNAMIC_TAG, ?DYNAMIC_TAG}}
                ,gen_fw:check_rules([{all, ?DYNAMIC_TAG}])),

    ?assertMatch({error, {value, [{rules, _}|_]}}, gen_fw:check_rules({})),
    ?assertMatch({error, {value, [{rule, 3.14}|_]}}, gen_fw:check_rules([3.14])).


'3'(_) ->
    gen_fw:start(?REGISTER_NAME, {?CALLBACK, start_link, []}, []),
    Ref = erlang:make_ref(),
    From = erlang:self(),
    erlang:spawn(fun() -> gen_server:call(?NAME, {From, Ref}) end),
    receive
        {Ref, done} ->
            ok
    end,

    gen_server:cast(?NAME, {From, Ref}),
    receive
        {Ref, done} ->
            ok
    end,

    ?NAME ! {From, Ref},
    receive
        {Ref, done} ->
            ok
    end,

    _ = sys:get_state(?NAME).


'4'(_) ->
    gen_fw:start(?REGISTER_NAME, {?CALLBACK, start_link, []}, [{all, ?DENY_TAG}]),
    Ref = erlang:make_ref(),
    From = erlang:self(),
    try gen_server:call(?NAME, {From, Ref}, 2000) of
        _ ->
            exit(break)
    catch
        _:_ ->
            ok
    end,
    receive
        {Ref, done} ->
            exit(break)
    after 1000 ->
        ok
    end,

    gen_server:cast(?NAME, {From, Ref}),
    receive
        {Ref, done} ->
            exit(break)
    after 1000 ->
        ok
    end,

    ?NAME ! {From, Ref},
    receive
        {Ref, done} ->
            exit(break)
    after 1000 ->
        ok
    end,

    try sys:get_state(?NAME, 2000) of
        _ ->
            exit(break)
    catch
        _:_ ->
            ok
    end.


'5'(_) ->
    gen_fw:start(?REGISTER_NAME, {?CALLBACK, start_link, []}, [{all, ?DYNAMIC_TAG}]),
    Ref = erlang:make_ref(),
    From = erlang:self(),
    try gen_server:call(?NAME, {From, Ref}, 2000) of
        _ ->
            exit(break)
    catch
        _:_ ->
            ok
    end,
    receive
        {Ref, done} ->
            ok
    end,

    gen_server:cast(?NAME, {From, Ref}),
    receive
        {Ref, done} ->
            ok
    end,
    receive
        {Ref, done} ->
            exit(break)
    after 1000 ->
        ok
    end,


    ?NAME ! {From, Ref},
    receive
        {Ref, done} ->
            ok
    end,
    receive
        {Ref, done} ->
            exit(break)
    after 1000 ->
        ok
    end,

    try sys:get_state(?NAME, 2000) of
        _ ->
            ok
    catch
        _:_ ->
            exit(break)
    end.







