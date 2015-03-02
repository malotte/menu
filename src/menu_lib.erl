%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Malotte Westma Lonne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Generic menu system.
%%%
%%% Created : Jan 2015 by Malotte W Lonne
%%% @end
%%%-------------------------------------------------------------------
-module(menu_lib).

-include_lib("lager/include/log.hrl").

-export([spec/0, 
	 example/0, 
	 start/0, start/2]).

-define(i2a(I), list_to_atom(integer_to_list(I))).
-define(a2i(A), list_to_integer(atom_to_list(A))).

start() ->
    {menu, Spec} = spec(),
    Config = example(),
    start(Spec, Config).

start(Spec, Config) ->
    Db = load([], Spec, Config, []),
    lager:debug("menu: db ~p", [Db]),
    Output = fun({list_item, Key, N, Value}) ->
		     io:format("~p~p ~p~n", [Key, [N], Value]);
		({Key, Value}) when is_list(Key)->
		     list2dot(Key, Value);
		({Key, Value}) ->
		     io:format("~p ~p~n",[Key, Value]);
		(Key) ->
		     io:format("~p~n",[Key])
	     end,
    Input = fun() ->
		    io:get_line(">")
	    end,

    menu_loop([Spec], Db, [], Output, Input).

list2dot([Key], Value) ->
    %% Last, no dot
    io:format("~p ~p ~n",[Key, Value]);
list2dot([Key | Rest], Value) ->
    io:format("~p.",[Key]),
    list2dot(Rest, Value).
    
load(_KeyStack, Spec, [], Acc) ->
    lists:reverse(Acc);
load(KeyStack, Spec, [{Key, Value} | Rest], Acc) when is_list(Value)->
    case is_string(Value) of
	true -> 
	    load(KeyStack, Spec, Rest, [{lists:reverse([Key|KeyStack]), Value} | Acc]);
	false ->
	    Acc1 = load([Key| KeyStack], Spec, Value, []),
	    lager:debug("load: acc1 ~p", [Acc1]),
	    load(KeyStack, Spec, Rest, lists:reverse(Acc1) ++ Acc)
    end;
load(KeyStack, Spec, [{Key, Value} | Rest], Acc)->
    load(KeyStack, Spec, Rest, [{lists:reverse([Key|KeyStack]), Value} | Acc]).
		  
    

menu_loop([], _Db, _KeyStack, _Output, _Input) ->
    %% Backed out
    ok;
menu_loop(SpecStack, Db, KeyStack, Output, Input) ->
    case menu(hd(SpecStack), hd(SpecStack), Db, KeyStack, Output, Input) of
	repeat -> 
	    menu_loop(SpecStack, Db, KeyStack, Output, Input);
	{continue, NewSpec, NewKeyStack} -> 
	    menu_loop(push(NewSpec, SpecStack), Db, NewKeyStack, Output, Input);
	{back, NewDb, NewKeyStack} -> 
	    menu_loop(SpecStack, NewDb, NewKeyStack, Output, Input);
	back -> 
	    menu_loop(pop(SpecStack), Db, pop(KeyStack), Output, Input);
	{error, Reason} -> 
	    Output(Reason),
	    menu_loop(SpecStack, Db, KeyStack, Output, Input);
	show -> 
	    show(Db, KeyStack, Output),
	    menu_loop(SpecStack, Db, KeyStack, Output, Input);
	exit -> 
	    ok
    end.

push(New, Old) -> [New| Old].

pop([]) -> [];
pop([_Last| Prev]) -> Prev.

menu([], Spec, Db, KeyStack, _Output, Input) ->
    Choice = Input(),
    case Choice of
	"..\n" -> back;
	".\n" -> repeat;
	"?\n" -> repeat;
	"show\n" -> show;
	"exit\n" -> exit;
	"exit.\n" -> exit;
	_ -> scan_input(Choice, Spec, Db, KeyStack)
    end;
menu([{leaf_list = T, Key, _TS} | List], Spec, Db, KeyStack, Output, Input) ->
    Output({Key, T}),
    menu(List, Spec, Db, KeyStack, Output, Input);    
menu([{list_item, N, _TS} | List], Spec, Db, KeyStack, Output, Input) ->
    FullKey = lists:reverse(KeyStack),
    {FullKey, Value} = lists:keyfind(FullKey, 1, Db), %% Must exist ??
    Output({list_item, hd(KeyStack), N, lists:nth(N, Value)}),
    menu(List, Spec, Db, KeyStack, Output, Input);    
menu([{leaf_list = Type, Key, _TS} | List], Spec, Db, KeyStack, Output, Input) ->
    %%lager:debug("menu: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    FullKey = lists:reverse(push(Key,KeyStack)),
    %%lager:debug("menu: ~p", [FullKey]),
    case lists:keyfind(FullKey, 1, Db) of
	false ->  
	    Output({Key, Type});
	{FullKey, Value} ->
	    output_leaf_list(Key, Value, 1, Output)
    end,
    menu(List, Spec, Db, KeyStack, Output, Input);
menu([{Type, Key, _TS} | List], Spec, Db, KeyStack, Output, Input) ->
    %%lager:debug("menu: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    FullKey = lists:reverse(push(Key,KeyStack)),
    %%lager:debug("menu: ~p", [FullKey]),
    case lists:keyfind(FullKey, 1, Db) of
	false ->  Output({Key, Type});
	{FullKey, Value} -> Output({Key, Value})
    end,
    menu(List, Spec, Db, KeyStack, Output, Input).

output_leaf_list(_Key, [], _N, _Output) ->
    ok;
output_leaf_list(Key, [Value | Rest], N, Output) ->
    Output({list_item, Key, N, Value}),
    output_leaf_list(Key, Rest, N+1, Output).


scan_input(Choice, Spec, Db, KeyStack) ->
    case string:tokens(Choice, [$ , $\n]) of
	["set", Key, Value] -> 
	    search_spec(format(Key), [Value], Spec, Db, KeyStack);
	["unset", Key] -> 
	    search_spec(format(Key), [], Spec, Db, KeyStack);
	[Key] -> 
	    search_spec(format(Key), [], Spec, Db, KeyStack);
	_ -> repeat
    end.

format(String) when is_list(String) ->
    try list_to_integer(String) of
	I -> I
    catch 
	error:_E -> 
	    list_to_atom(String)
    end.
	    
search_spec(Key, [], Spec, Db, KeyStack) ->
    lager:debug("search_spec: ~p = ~p, ~p, ~p, ~p", 
		[Key, [], Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, _TS} -> {error, value_required};
	{menu, Key, NewSpec} -> {continue, NewSpec, push(Key, KeyStack)};
	{leaf_list, Key, TS} -> handle_leaf_list(Key, TS, Spec, Db, KeyStack);
	false -> {error, illegal_choice}
    end;
search_spec(Key, [Value], Spec, Db, KeyStack) ->
    lager:debug("search_spec: ~p = ~p, ~p, ~p, ~p", 
		[Key, Value, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{list_item, Key, TS} -> verify_ts(Key, Value, TS, Db, push(Key, KeyStack));
	{item, Key, TS} -> verify_ts(Key, Value, TS, Db, push(Key, KeyStack));
	{menu, Key, _NewSpec} -> {error, superfluous_value};
	false -> {error, illegal_choice}
    end.

handle_leaf_list(Key, TS, Spec, Db, KeyStack) ->
    FullKey = lists:reverse(push(Key,KeyStack)),
    lager:debug("leaf_list: ~p", [FullKey]),
    case lists:keyfind(FullKey, 1, Db) of
	false ->  
	    {error, hmm};
	{FullKey, Value} when is_list(Value) -> 
	    {continue, leaf_list_spec(Key, Value, TS, 1, []), push(Key, KeyStack)}
    end.
    
leaf_list_spec(_Key, [], _TS, _N, Acc) ->
    lists:reverse(Acc);
leaf_list_spec(Key, [_Value | Rest], TS, N, Acc) ->
    leaf_list_spec(Key, Rest, TS, N+1, [{list_item, N, TS} | Acc]).
	

%% Use yang:check_type/2 instead ??
verify_ts(Key, Value, string, Db, KeyStack) when is_list(Value) ->
    change_config(Key, Value, Db, KeyStack);
verify_ts(_Key, _Value, string, _Db, _KeyStack)  ->
    {error, wrong_type};
verify_ts(Key, Value, integer, Db, KeyStack)  ->
    try list_to_integer(Value) of
	Int -> change_config(Key, Int, Db, KeyStack)
    catch
	error:_E -> {error, wrong_type}
    end.
	    
change_config(Key, Value, Db, KeyStack) when is_integer(Key) ->
    %% Leaf list case
    lager:debug("change_config: ~p = ~p, ~p", [Key,Value,KeyStack]),
    FullKey = lists:reverse(pop(KeyStack)), %% Remove integer
     lager:debug("change_config: ~p", [FullKey]),
    {back, update_list_item(Key, Value, Db, FullKey), pop(KeyStack)};  
change_config(Key, Value, Db, KeyStack) when is_atom(Key) ->
    %% Normal case
    lager:debug("change_config: ~p = ~p, ~p", [Key,Value,KeyStack]),
    FullKey = lists:reverse(KeyStack),
    lager:debug("change_config: ~p", [FullKey]),
    NewDb = lists:keyreplace(FullKey, 1, Db, {FullKey, Value}), 
    {back, NewDb, pop(KeyStack)}. 

update_list_item(N, Value, Db, FullKey) ->
    lager:debug("update_list_item: ~p = ~p, ~p, ~p", [N, Value, Db, FullKey]),
    {FullKey, List} = lists:keyfind(FullKey, 1, Db),
    %%{StartAndN, End} = lists:split(N, List),
    {Start, [Remove | End]} = lists:split(N-1, List),
    NewList = Start ++ [Value] ++ End,
    lists:keyreplace(FullKey, 1, Db, {FullKey, NewList}).
			       
show(Db, KeyStack, Output) ->
    lager:debug("show: ~p, ~p.", [Db, KeyStack]),
    lists:foldl(fun({Key, Value}, _Acc) ->
			case lists:prefix(lists:reverse(KeyStack), Key) of
			    true -> Output({Key, Value});
			    false -> ok
			end
		end, [], Db).
		

spec() ->
    {menu,
	[{item, number, integer},
	 {item, name, string},
	 {menu, data_port, [{item, number, integer},
			    {menu, description, [{item, number, integer},
						 {item, name, string}]}]},
	 {leaf_list, ports, integer},
	 {item, description, string},
	 {item, data_timeout, integer}]}.


example() ->
    [{number, 10},
     {name, "hej"},
     {data_port, [{number,12}, {description, [{number, 17}, {name, "bar"}]}]},
     {ports, [8080, 8081]},
     {description,  "foo"}].


verify_type(_X, undefined) -> true;
verify_type(X, anyxml) when is_list(X); is_binary(X) -> true;
verify_type(X, {enumeration, Enums}) ->
    lists:keymember(list_to_atom(X), 1, Enums);
verify_type(X, binary) ->
    try list_to_binary(X) of
	_Bin -> true
    catch error:_ -> false
    end;
verify_type(X, string) -> is_string(X);
verify_type(X, boolean) -> 
    case list_to_atom(X) of
	true ->true;
	false ->true;
	_ -> false
    end;
verify_type("", empty) -> true;	    
verify_type(_, empty) -> false;
verify_type(X, decimal64) -> 
    try list_to_float(X) of
	_F -> true
    catch error:_ -> false
    end;
verify_type(N, T) when T == uint8;
		       T == uint16;
		       T == uint32;
		       T == uint64;
		       T == int8;
		       T == int16;
		       T == int32;
		       T == int64 ->
    try verify_integer_type(list_to_integer(N), T)
    catch error:_ ->false
    end;
verify_type(_,_) -> false. 
   
verify_integer_type(N, uint8) when 0 =< N, N =< 255 -> true;
verify_integer_type(N, uin16) when 0 =< N, N =< 65535 -> true;
verify_integer_type(N, uint32) when  0 =< N, N =< 4294967295 -> true; 
verify_integer_type(N, uint64) when 0 =< N, N =< 18446744073709551615 -> true;
verify_integer_type(N, int8) when -128 =< N, N =< 127 -> true;
verify_integer_type(N, in16) when -32768 =< N, N =< 32767 -> true;
verify_integer_type(N, int32) when  -2147483648 =< N, N =< 2147483647 -> true; 
verify_integer_type(N, int64) when -9223372036854775808 =< N, 
				   N =< 9223372036854775807 -> true;
verify_integer_type(_,_) -> false. 
	     


-spec check_type(any(), atom()) -> boolean().
check_type(_X, undefined) -> true;
check_type(X, anyxml) when is_list(X); is_binary(X) -> true;
check_type(X, {enumeration,Enums}) -> lists:keymember(X, 1, Enums);
check_type(X, binary) when is_binary(X) -> true;
check_type(X, bits) when is_bitstring(X) -> true;
check_type(X, string) -> is_string(X);
check_type(X, boolean) when is_boolean(X) -> true;
check_type(X, empty) -> is_empty(X);
check_type(X, decimal64) when is_float(X) -> true;
check_type(X, integer) when is_integer(X) -> true;
check_type(N, T) when is_integer(N) ->
   if 	T == uint8, 08 =< N, N =< 255 -> true;
	T == uint16, 0 =< N, N =< 65535 -> true;
	T == uint32, 0 =< N, N =< 4294967295 -> true;
	T == uint64, 0 =< N, N =< 18446744073709551615 -> true;
	true -> false
    end.

is_empty(<<>>) -> true;
is_empty([])   -> true;
is_empty(_)    -> false.


is_string(Value) ->
    try unicode:characters_to_binary(Value) of
	Utf8 when is_binary(Utf8) -> true;
	{error,_,_} -> false
    catch
	error:_ -> false
    end.

