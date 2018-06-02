%%% ------------------------------------------------------------------------------------------------
%%% "gen_fw" is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.6.2
%% @doc
%%           Erlang/Elixir Generic firewall process.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(gen_fw).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/2
        ,start_link/3
        ,start/2
        ,start/3
        ,check_rules/1]).

%% 'proc_lib' callbacks:
-export([init/5
        ,init/6]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {module, object, cast, call, message, system}).

-define(DEF_CALLBACK_FUNCTION, fw_handle_message).

-define(is_mfa(X, Y, Z), (erlang:is_atom(X) andalso erlang:is_atom(Y) andalso erlang:is_list(Z))).

-include("gen_fw.hrl").

%% -------------------------------------------------------------------------------------------------
%% Types:

-type register_name() :: {'local', atom()}.
-type fw() :: register_name() | pid().
-type rules() :: [] | [rule()].
-type  rule() :: {'cast', action()}
               | {'call', action()}
               | {'message', action()}
               | {'system', action()}
               | {'all', action()}.
-type   action() :: 'allow' | 'deny' | 'dynamic'.

-type start_return() :: {'ok', pid()} | 'ignore' | {'error', term()}.

-export_type([register_name/0
             ,fw/0
             ,rules/0
             ,rule/0
             ,action/0
             ,start_return/0]).

%% -------------------------------------------------------------------------------------------------
%% Behaviour info:

-callback
fw_handle_message(Type, Info) ->
    allow | deny
when
    Type :: 'cast' | 'call' | 'message' |'system',
    Info :: {From, term()} | term(),
    From :: {pid(), reference()}.

-optional_callbacks([fw_handle_message/2]).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(mfa(), rules()) ->
    start_return().
start_link({Mod, Func, Args}, Rules) when ?is_mfa(Mod, Func, Args) andalso erlang:is_list(Rules) ->
    proc_lib:start_link(?MODULE, init, [Mod, Func, Args, Rules, erlang:self()]).


-spec
start_link(register_name(), mfa(), rules()) ->
    start_return().
start_link({local, Name}, {Mod, Func, Args}, Rules) when erlang:is_atom(Name) andalso
                                                         ?is_mfa(Mod, Func, Args) andalso
                                                         erlang:is_list(Rules) ->
    proc_lib:start_link(?MODULE, init, [Name, Mod, Func, Args, Rules, erlang:self()]).


-spec
start(mfa(), rules()) ->
    start_return().
start({Mod, Func, Args}, Rules) when ?is_mfa(Mod, Func, Args) andalso erlang:is_list(Rules) ->
    proc_lib:start(?MODULE, init, [Mod, Func, Args, Rules, erlang:self()]).


-spec
start(register_name(), mfa(), rules()) ->
    start_return().
start({local, Name}, {Mod, Func, Args}, Rules) when erlang:is_atom(Name) andalso
                                                    ?is_mfa(Mod, Func, Args) andalso
                                                    erlang:is_list(Rules) ->
    proc_lib:start(?MODULE, init, [Name, Mod, Func, Args, Rules, erlang:self()]).


-spec
check_rules(rules()) ->
    {'ok', {Cast :: action(), Call :: action(), Msg :: action(), Sys :: action()}} |
    {'error', term()}.
check_rules(Rules) when erlang:is_list(Rules) ->
    format_rules(Rules, #{});
check_rules(Other) ->
    {error, {value, [{rules, Other}]}}.

%% -------------------------------------------------------------------------------------------------
%% 'proc_lib' callbacks:

init(Name, Mod, Func, Args, Rules, Parent) ->
    try erlang:register(Name, erlang:self()) of
        _ ->
            init(Mod, Func, Args, Rules, Parent)
    catch
        _:_ ->
            Rsn = {already_registered, erlang:whereis(Name)},
            proc_lib:init_ack(Parent, {error, Rsn}),
            erlang:exit(Rsn)
    end.


init(Mod, Func, Args, Rules, Parent) ->
    erlang:process_flag(trap_exit, true),
    case check_rules(Rules) of
        {ok, {Cast, Call, Msg, Sys}} ->
            Result =
                try erlang:apply(Mod, Func, Args) of
                    {ok, Object}=Ok when erlang:is_pid(Object) orelse erlang:is_port(Object) ->
                        Ok;
                    ignore ->
                        {ok, ignore};
                    {error, _}=Err ->
                        Err;
                    Other ->
                        {error, {return, [{value, Other}
                                         ,{module, Mod}
                                         ,{function, Func}
                                         ,{arguments, Args}]}}
                catch
                    _:Rsn ->
                        {error, {crash, [{reason, Rsn}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{module, Mod}
                                        ,{function, Func}
                                        ,{arguments, Args}]}}
                end,

            Ack =
                case Result of
                    {ok, ignore} ->
                        ignore;
                    {ok, _} ->
                        {ok, erlang:self()};
                    _ -> % {error, _}
                        Result
                end,
            proc_lib:init_ack(Parent, Ack),

            case Result of
                {ok, ignore} ->
                    erlang:exit(normal);
                {ok, Object2} ->
                    MonitorType =
                        if
                            erlang:is_pid(Object2) ->
                                process;
                            true -> % port
                                port
                        end,
                    erlang:monitor(MonitorType, Object2),
                    loop(Parent
                        ,#?S{module = Mod
                            ,object = Object2
                            ,cast = Cast
                            ,call = Call
                            ,message = Msg
                            ,system = Sys});
                {error, Rsn2} ->
                    erlang:exit(Rsn2)
            end;
        {error, Rsn}=Err ->
            proc_lib:init_ack(Parent, Err),
            erlang:exit(Rsn)
    end.

%% -------------------------------------------------------------------------------------------------
%% Internals:

format_rules([{cast, Action}|Rest], Ret) when ?is_action(Action) ->
    format_rules(Rest, Ret#{cast => Action});
format_rules([{call, Action}|Rest], Ret) when ?is_action(Action) ->
    format_rules(Rest, Ret#{call => Action});
format_rules([{system, Action}|Rest], Ret) when ?is_action(Action) ->
    format_rules(Rest, Ret#{system => Action});
format_rules([{message, Action}|Rest], Ret) when ?is_action(Action) ->
    format_rules(Rest, Ret#{message => Action});


format_rules([{all, Action}|Rest], Ret) when ?is_action(Action) ->
    format_rules(Rest, Ret#{call => Action, cast => Action, message => Action, system => Action});

format_rules([Elem|_], _) ->
    {error, {value, [{rule, Elem}]}};

format_rules(_, Ret) -> % ([], Ret)
    Cast =
        case maps:get(cast, Ret, ?ALLOW_TAG) of
            CastRules when erlang:is_list(CastRules) ->
                lists:reverse(CastRules);
            CastRule ->
                CastRule
        end,
    Call =
        case maps:get(call, Ret, ?ALLOW_TAG) of
            CallRules when erlang:is_list(CallRules) ->
                lists:reverse(CallRules);
            CallRule ->
                CallRule
        end,
    Msg =
        case maps:get(message, Ret, ?ALLOW_TAG) of
            MsgRules when erlang:is_list(MsgRules) ->
                lists:reverse(MsgRules);
            MsgRule ->
                MsgRule
        end,
    Sys =
        case maps:get(system, Ret, ?ALLOW_TAG) of
            SysRules when erlang:is_list(SysRules) ->
                lists:reverse(SysRules);
            SysRule ->
                SysRule
        end,
    {ok, {Cast, Call, Msg, Sys}}.


loop(Parent, S) ->
    S2 =
        case (receive Msg -> Msg end) of
            {'EXIT', Parent, Rsn} -> % Parent == Parent
                erlang:exit(Rsn);
            Msg2 ->
                % Currently it does not change State:
                process_message(Msg2, S)
        end,
    loop(Parent, S2).


process_message({'$gen_cast', Cast}=Msg, #?S{cast = Rule, module = Mod}=S) ->
    case process_rule(Rule, Mod, cast, Cast) of
        ?ALLOW_TAG ->
            S#?S.object ! Msg,
            ok;
        _ -> % ?DENY_TAG
            ok
    end,
    S;
process_message({'$gen_call', From, Call}=Msg, #?S{call = Rule, module = Mod}=S) ->
    case process_rule(Rule, Mod, call, {From, Call}) of
        ?ALLOW_TAG ->
            S#?S.object ! Msg,
            ok;
        _ -> % ?DENY_TAG
            ok
    end,
    S;
process_message({system, From, SysMsg}=Msg, #?S{system = Rule, module = Mod}=S) ->
    case process_rule(Rule, Mod, system, {From, SysMsg}) of
        ?ALLOW_TAG ->
            S#?S.object ! Msg,
            ok;
        _ -> % ?DENY_TAG
            ok
    end,
    S;
process_message({'DOWN', _, _, Object, Rsn}, #?S{object = Object}) ->
    erlang:exit(Rsn);
process_message({'EXIT', Object, Rsn}, #?S{object = Object}) ->
    erlang:exit(Rsn);
process_message(Msg, #?S{message = Rule, module = Mod}=S) ->
    case process_rule(Rule, Mod, message, Msg) of
        ?ALLOW_TAG ->
            S#?S.object ! Msg,
            ok;
        _ -> % ?DENY_TAG
            ok
    end,
    S.


process_rule(Rule, Mod, Type, Info) when Rule == ?DYNAMIC_TAG  ->
    case erlang:function_exported(Mod, ?DEF_CALLBACK_FUNCTION, 2) of
        true ->
            try Mod:?DEF_CALLBACK_FUNCTION(Type, Info) of
                Result when ?is_action(Result) andalso Result /= ?DYNAMIC_TAG ->
                    Result;
                _ ->
                    ?DENY_TAG
            catch
                _:_ ->
                    ?DENY_TAG
            end;
        false ->
            ?DENY_TAG
    end;
process_rule(Rule, _, _, _) when ?is_action(Rule) ->
    Rule;
process_rule(_, _, _, _) ->
    ?DENY_TAG.