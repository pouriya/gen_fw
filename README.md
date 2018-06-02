![gen_fw travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/gen_fw.png?branch=master)

# gen_fw
Generic firewall process for Erlang/Elixir. It's an Erlang process between client processes and server process/port and prevents receiving some unwanted Erlang messages from server process/port.

## When to use **gen_fw**?
When writing new server process, It's better to receive all messages and handle unwanted messages in that process instead of using `gen_fw`. But when you are using some generic behaviour or someone else's library and you don't want your client processes send everything (even wrongly) to server process/port, `gen_fw` is your friend. In below example i will show you how to prevent client process call `supervisor:terminate/2` successfully for your supervisor.  

## How to use?
To starting main process you have to give its `MFA` to `gen_fw` and `gen_fw` will start itself and main process in order. Note that if you want to register main process, you have to **give** its register name to `gen_fw` and `gen_fw` will register itself.  
API functions for starting linked `gen_fw` process:  
```erlang
gen_fw:start_link({Mod, Func, Args}=_MFA, Rules).
gen_fw:start_link({local, Name}, {Mod, Func, Args}=_MFA, Rules).
```
API functions for starting stand-alone `gen_fw` process:  
```erlang
gen_fw:start({Mod, Func, Args}=_MFA, Rules).
gen_fw:start({local, Name}, {Mod, Func, Args}=_MFA, Rules).
```

#### Rules
```erlang
-type rules() :: [] | [rule()].
-type  rule() :: {'cast', action()}
               | {'call', action()}
               | {'message', action()}
               | {'system', action()}
               | {'all', action()}.
-type   action() :: 'allow' | 'deny' | 'dynamic'.
```
For example:  
```erlang

start_link_fw() ->
	gen_fw:start_link({?MODULE, start_link, []}
                     ,[{cast, deny}, {message, allow}, {system, allow}, {call, dynamic}]).

start_link() ->
	gen_server:start_link(?MODULE, init_arg, []).
```
Call to `start_link_fw/0` means start my process using `?MODULE:start_link()` which start a `gen_server` and:  
* Prevent receiving `cast` messages (Means that `?MODULE:handle_cast/2` never runs)  
* Allow receiving messages (Means that `?MODULE:handle_info/2` may run)  
* Allow receiving system message (Means you can use `sys:get_state/1`, `sys:get_status/1`, ...)    
* Use callback function `?MODULE:fw_handle_message/2` and this function will tell you that i want to receive calls or not (Means that `gen_server` may receive some calls and `?MODULE:handle_call/3` may run)  

If function `?MODULE:fw_handle_message/2` is not exported or if it did not yield `allow` or `deny` or if it crashed, `gen_fw` denies message silently.  

Note that instead of writing:
```erlang
gen_fw:start_link({?MODULE, start_link, []}, [{cast, deny}, {message, deny}, {system, deny}, {call, allow}]).
```
you can just write:
```erlang
gen_fw:start_link({?MODULE, start_link, []}, [{all, deny}, {call, allow}]).
```

## Example
I wrote a childless supervisor in module [my_sup](https://github.com/pouriya-jahanbakhsh/gen_fw/blob/master/examples/my_sup.erl). Let's start supervisor directly first:  
```erlang
1> {ok, Sup} = supervisor:start_link(my_sup, init_arg).
{ok,<0.60.0>}

2> Sup ! hello.
hello
 
=ERROR REPORT==== 2-Jun-2018::13:49:28 ===
Supervisor received unexpected message: hello

3> supervisor:which_children(Sup).
[]

4> supervisor:terminate_child(Sup, foo).
{error,not_found}

%% Crash :-S
5> gen_server:cast(Sup, oops).

=ERROR REPORT==== 2-Jun-2018::13:50:20 ===
** Generic server <0.60.0> terminating 
** Last message in was {'$gen_cast',oops}
** When Server state == {state,{<0.60.0>,my_sup},
                               one_for_one,[],undefined,1,3,[],0,my_sup,
                               init_arg}
** Reason for termination == 
** {function_clause,
       [{supervisor,handle_cast,
            [oops,
             {state,
                 {<0.60.0>,my_sup},
                 one_for_one,[],undefined,1,3,[],0,my_sup,init_arg}],
            [{file,"supervisor.erl"},{line,585}]},
        {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,601}]},
        {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,667}]},
        {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,247}]}]}
ok
** exception error: no function clause matching supervisor:handle_cast(oops,
                                                                       {state,{<0.60.0>,my_sup},
                                                                              one_for_one,[],undefined,1,3,[],0,my_sup,init_arg}) (supervisor.erl, line 585)
     in function  gen_server:try_dispatch/4 (gen_server.erl, line 601)
     in call from gen_server:handle_msg/5 (gen_server.erl, line 667)
     in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 247)
```
Now i want to use `gen_fw`. I don't want to receive `cast`s and `message`s. Also i just want allow `supervisor:count_children/1` call and want to reply an error message to callers of other API functions directly from `gen_fw`. First read the [my_sup code](https://github.com/pouriya-jahanbakhsh/gen_fw/blob/master/examples/my_sup.erl):  
```erlang
1> {ok, Sup} = my_sup:start_link([{cast, deny}, {message, deny}, {call, dynamic}]).
{ok,<0.60.0>}

%% supervisor process does not receive this message and does not call error_logger
2> Sup ! hello.
hello

3> supervisor:which_children(Sup).
{error,permission_denied}

4> supervisor:terminate_child(Sup, foo).
{error,permission_denied}

5> supervisor:count_children(Sup). 
[{specs,0},{active,0},{supervisors,0},{workers,0}]

%% supervisor process does not receive this message, then there is no crash
6> gen_server:cast(Sup, oops).
ok
```

### License
**`BSD 3-Clause`**


### Author
**`pouriya.jahanbakhsh@gmail.com`**


### Hex
[**`18.6.2`**](https://hex.pm/packages/gen_fw)
