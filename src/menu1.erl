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
%%% @author Malotte Westman Lonne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Generic menu system.
%%%
%%% Created : Jan 2015 by Malotte W Lonne
%%% @end
%%%-------------------------------------------------------------------
-module(menu1).

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
    Db = Config, 
    lager:debug("menu: db ~p", [Db]),
    Output = fun({list_item, Key, N, Value}) ->
		     io:format("~p~p ~p~n", [Key, [N], Value]);
		({"end", Key}) ->
		     io:format("end ~p ~n", [Key]);
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

    menu_loop([Spec], [Db], [], Output, Input).

list2dot([Key], Value) ->
    %% Last, no dot
    io:format("~p ~p ~n",[Key, Value]);
list2dot([Key | Rest], Value) ->
    io:format("~p.",[Key]),
    list2dot(Rest, Value).
    

menu_loop([], _DbStack, _KeyStack, _Output, _Input) ->
    %% Backed out
    ok;
menu_loop(SpecStack, DbStack, KeyStack, Output, Input) ->
    case menu(hd(SpecStack), hd(SpecStack), hd(DbStack), KeyStack, Output, Input) of
	repeat -> 
	    menu_loop(SpecStack, DbStack, KeyStack, Output, Input);
	{continue, SubSpec, SubDb, NewKeyStack} -> 
	    menu_loop(push(SubSpec, SpecStack), 
		      push(SubDb, DbStack), 
		      NewKeyStack, Output, Input);
	{stay, NewDb} -> 
	    menu_loop(SpecStack, 
		      replace_db(NewDb, DbStack), 
		      KeyStack, Output, Input);
	back ->
	    %% The top of stack db might have been changed,
	    %% Update the one before and keep the rest (if any)
	    case DbStack of
		[SubDb, Db | Rest] ->
		    NewDb = update_db(hd(KeyStack), SubDb, Db, KeyStack),
		    menu_loop(pop(SpecStack), [NewDb| Rest],
			      pop(KeyStack), Output, Input);
		[_Db] ->
		    %% already at top level, repeat instead
		     menu_loop(SpecStack, DbStack, KeyStack, Output, Input)
	    end;
	{error, Reason} -> 
	    Output(Reason),
	    menu_loop(SpecStack, DbStack, KeyStack, Output, Input);
	show -> 
	    show(hd(DbStack), KeyStack, Output),
	    menu_loop(SpecStack, DbStack, KeyStack, Output, Input);
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
	_ -> handle_input(Choice, Spec, Db, KeyStack)
    end;
menu([{T, Key, _TS} | List], Spec, Db, KeyStack, Output, Input) 
  when T =:= menu;
       T =:= leaf_list->
    Output({Key, T}),
    menu(List, Spec, Db, KeyStack, Output, Input);    
menu([{list_items, Key, _TS} | List], Spec, Value, KeyStack, Output, Input) ->
    lager:debug("list_items: ~p, ~p, ~p", [Spec, Value, KeyStack]),
    output_leaf_list(Key, Value, 1, Output),
    %%Output({list_item, hd(KeyStack), N, lists:nth(N, Value)}),
    menu(List, Spec, Value, KeyStack, Output, Input);    
menu([{Type, Key, _TS} | List], Spec, Db, KeyStack, Output, Input) ->
    %%lager:debug("menu: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    case find_in_db(Key, Db) of
	false ->  Output({Key, Type});
	{Key, Value} -> Output({Key, Value})
    end,
    menu(List, Spec, Db, KeyStack, Output, Input).

output_leaf_list(_Key, [], _N, _Output) ->
    ok;
output_leaf_list(Key, [Value | Rest], N, Output) ->
    Output({list_item, Key, N, Value}),
    output_leaf_list(Key, Rest, N+1, Output).

handle_input(Choice, Spec, Db, KeyStack) ->
    case string:tokens(Choice, [$ , $\n]) of
	["set", Key, Value] -> 
	    set(split(Key), Value, Spec, Db, KeyStack);
	["unset", Key] -> 
	    unset(split(Key), Spec, Db, KeyStack);
	["insert", Position, Key, Value] when Position =:= "before";
					      Position =:= "after";
					      Position =:= "first";
					      Position =:= "last" -> 
	    insert(split(Key), Position, Value, Spec, Db, KeyStack);
	["insert", Position, Value] when Position =:= "first";
					 Position =:= "last" -> 
	    %% Must be on leaf_list level
	    insert([0], Position, Value, Spec, Db, KeyStack);
	["delete", Key] -> 
	    delete(split(Key), Spec, Db, KeyStack);
	[Key] ->
	    enter(format(Key), Spec, Db, KeyStack);
	_ -> repeat
    end.

enter(Key, Spec, Db, KeyStack) ->
    lager:debug("enter: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, _TS} -> {error, value_required};
	{menu, Key, SubSpec} -> handle_menu(Key, SubSpec, Db, KeyStack);
	{leaf_list, Key, TS} -> handle_leaf_list(Key, TS, Spec, Db, KeyStack);
 	false -> {error, illegal_choice}
    end.   

handle_menu(Key, SubSpec, Db, KeyStack) ->
    lager:debug("handle_menu: ~p, ~p, ~p, ~p", [Key, SubSpec, Db, KeyStack]),
    case find_in_db(Key, Db) of
	 {Key, SubDb} ->
	     {continue, SubSpec, SubDb, push(Key, KeyStack)};
	 false ->
	     {error, unknown_menu}
     end.

handle_leaf_list(Key, TS, Spec, Db, KeyStack) ->
    lager:debug("handle_leaf_list: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    case find_in_db(Key, Db) of
	false ->  
	    {error, hmm};
	{Key, Value} when is_list(Value) -> 
	    {continue, [{list_items, Key, TS}], Value, push(Key, KeyStack)}
    end.

leaf_list_spec(_Key, [], _TS, _N, Acc) ->
    lists:reverse(Acc);
leaf_list_spec(Key, [_Value | Rest], TS, N, Acc) ->
    leaf_list_spec(Key, Rest, TS, N+1, [{list_item, N, TS} | Acc]).
	

set([N], Value, [{list_items, _Key, TS}], List, KeyStack) ->
    %% list_items case
    set_list_item(N, Value, TS, List, KeyStack);

set([Key], Value, Spec, Db, KeyStack) ->
    lager:debug("set: ~p = ~p, ~p, ~p, ~p", [Key, Value, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, TS} ->
	    set_item(Key, Value, TS, Db, KeyStack);
	{list_items, _, TS} ->
	    set_list_item(Key, Value, TS, Db, KeyStack);
	_O ->
	    lager:debug("set: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end;
set([Key | Rest] = KeyList, Value, Spec, Db, KeyStack) ->
    lager:debug("set: ~p = ~p, ~p, ~p, ~p", [KeyList, Value, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{menu, Key, SubSpec} ->
	    set_menu(KeyList, Value, Spec, SubSpec, Db, KeyStack);
	{leaf_list, Key, TS} ->
	    set_leaf_list(Key, Rest, Value, TS, Spec, Db, KeyStack);	    
	_O ->
	    lager:debug("set: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end.

set_item(Key, Value, TS, Db, KeyStack) ->
    lager:debug("set: found item ~p with spec ~p", [Key, TS]),
    case verify_type(Value, TS) of
	{true, X} ->
	    {stay, update_db(Key, X, Db, KeyStack)};
	false ->
	    {error, illegal_type}
    end.

set_list_item(N, Value, TS, List, KeyStack) ->
    lager:debug("set: found list items with spec ~p", [TS]),
    case verify_type(Value, TS) of
	{true, X} ->
	   case length(List) of
	       I when I >= N ->
		    {stay, update_list(N, X, List, KeyStack)};
	       I when I < N ->
		   {error, unknown_item}
	   end;
	false ->
	    {error, illegal_type}
    end.

set_menu([Key | Rest] = KeyList, Value, Spec, SubSpec, Db, KeyStack) ->
    lager:debug("set: found ~p with spec ~p", [Key, SubSpec]),
    case find_in_db(Key, Db) of
	{Key, SubDb} -> 
	    lager:debug("set: found ~p with db ~p", [Key, SubDb]),
	    case set(Rest, Value, SubSpec, SubDb, push(Key, KeyStack)) of
		{error, _Reason} = E ->
		    E;
		{stay, NewDb} ->
		    {stay, update_db(Key, NewDb, Db, KeyStack)}
	    end;
	false -> 
	    lager:debug("set: ~p not found in db", [Key]),
	    %% Add 
	    set(KeyList, Value, Spec, 
		add_to_db(Key, SubSpec, Db, KeyStack), KeyStack)
    end.

set_leaf_list(Key, Rest, Value, TS, Spec, Db, KeyStack) ->
    lager:debug("set: found leaf list ~p with spec ~p", [Key, TS]),
    %% Rest must be [N] ??
    case verify_type(Value, TS) of
	{true, X} ->
	    case update_list_in_db(hd(Rest), X, Db, 
				   push(Key, KeyStack)) of
		{error, _Reason} = E ->
		    E;
		NewDb ->	
		    {stay, NewDb}
	    end;
	false ->
	    {error, illegal_type}
    end.


unset([Key], Spec, Db, KeyStack) ->
    lager:debug("unset: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, TS} ->
	    lager:debug("unset: found item ~p with spec ~p", [Key, TS]),
	    {stay, remove_from_db(Key, TS, Db, KeyStack)};
	_O ->
	    lager:debug("unset: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end;
unset([Key | _Rest] = KeyList, Spec, Db, KeyStack) ->
    lager:debug("unset: ~p, ~p, ~p, ~p", [KeyList, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{menu, Key, SubSpec} ->
	    unset_menu(KeyList, Spec, SubSpec, Db, KeyStack);
	_O ->
	    lager:debug("unset: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end.

unset_menu([Key | Rest], Spec, SubSpec, Db, KeyStack) ->
    lager:debug("unset: found ~p with spec ~p", [Key, SubSpec]),
    case find_in_db(Key, Db) of
	{Key, SubDb} -> 
	    lager:debug("unset: found ~p with db ~p", [Key, SubDb]),
	    case unset(Rest, SubSpec, SubDb, push(Key, KeyStack)) of
		{error, _Reason} = E ->
		    E;
		{stay, NewDb} ->
		    {stay, update_db(Key, NewDb, Db, KeyStack)}
	    end;
	false -> 
	    lager:debug("unset: ~p not found in db", [Key]),
	    {error, unknown_item}
    end.

insert([N], Position, Value, [{list_items, _Key, TS}], List, KeyStack) ->
    insert_list_item(N, Position, Value, TS, List, KeyStack);
insert([Key], Position, Value, Spec, Db, KeyStack) ->
    lager:debug("insert: ~p = ~p, ~p, ~p, ~p", 
		[Key, Value, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, TS} ->
	    insert_item(Key, Position, Value, TS, Db, KeyStack);
	{list_items, _, TS} ->
	    insert_list_item(Key, Position, Value, TS, Db, KeyStack);
	_O ->
	    lager:debug("insert: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end;
insert([Key | Rest] = KeyList, Position, Value, Spec, Db, KeyStack) ->
    lager:debug("insert: ~p = ~p, ~p, ~p, ~p", [KeyList, Value, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{menu, Key, SubSpec} ->
	    insert_menu(KeyList, Position, Value, Spec, SubSpec, Db, KeyStack);
	{leaf_list, Key, TS} ->
	    insert_leaf_list(KeyList, Position, Value, TS, Spec, Db, KeyStack);
	_O ->
	    lager:debug("insert: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end.

insert_item(Key, Position, Value, TS, Db, KeyStack) ->
    lager:debug("insert: found item ~p with spec ~p", [Key, TS]),
    case verify_type(Value, TS) of
	{true, X} ->
	    {stay, add_to_list_in_db(Key, Position, X, Db, KeyStack)};
	false ->
	    {error, illegal_type}
    end.

insert_list_item(N, Position, Value, TS, List, KeyStack) ->
    lager:debug("insert: ~p = ~p, ~p, ~p, ~p", [N, Value, TS, List, KeyStack]),
    case verify_type(Value, TS) of
	{true, X} ->
	   case length(List) of
	       I when I >= N ->
		    {stay, add_to_list(N, Position, X, List, KeyStack)};
	       I when I < N ->
		   {error, unknown_item}
	   end;
	false ->
	    {error, illegal_type}
    end.

insert_menu([Key | Rest] = KeyList, Position, Value, Spec, SubSpec, Db, KeyStack) ->
    lager:debug("insert: found ~p with spec ~p", [Key, SubSpec]),
    case find_in_db(Key, Db) of
	{Key, SubDb} -> 
	    lager:debug("insert: found ~p with db ~p", [Key, SubDb]),
	    case insert(Rest, Position, Value, SubSpec, SubDb, push(Key, KeyStack)) of
		{error, _Reason} = E ->
		    E;
		{stay, NewDb} ->
		    {stay, update_db(Key, NewDb, Db, KeyStack)}
	    end;
	false -> 
	    lager:debug("insert: ~p not found in db", [Key]),
	    %% Add 
	    insert(KeyList, Position, Value, Spec, 
		   add_to_db(Key, SubSpec, Db, KeyStack), KeyStack)
    end.

insert_leaf_list([Key | Rest], Position, Value, TS, Spec, Db, KeyStack) ->
    lager:debug("insert: found leaf list ~p with spec ~p", [Key, TS]),
    case verify_type(Value, TS) of
	{true, X} ->
	    %% Rest must be [N] ??
	    case add_to_list_in_db(hd(Rest), Position, X, Db, 
				   push(Key, KeyStack)) of
		{error, _Reason} = E ->
		    E;
		NewDb ->	
		    {stay, NewDb}
	    end;
	false ->
	    {error, illegal_type}
    end.

delete([N], [{list_items, _Key, TS}], Db, KeyStack) ->
    lager:debug("delete: ~p, ~p, ~p, ~p", [N, TS, Db, KeyStack]),
    {stay, delete_from_list(N, Db, KeyStack)};
delete([Key], Spec, Db, KeyStack) ->
   lager:debug("delete: ~p, ~p, ~p, ~p", [Key, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, TS} ->
	    lager:debug("delete: found item ~p with spec ~p", [Key, TS]),
	    {stay, remove_from_db(Key, TS, Db, KeyStack)};
	_O ->
	    lager:debug("delete: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end;
%%    lager:debug("delete: unknown list item ~p = ~p", [Key, Spec]),
%%    {error, unknown_list_item};
delete([Key | Rest] = KeyList, Spec, Db, KeyStack) ->
    lager:debug("delete: ~p, ~p, ~p, ~p", [KeyList, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{menu, Key, SubSpec} ->
	    delete_menu(KeyList, Spec, SubSpec, Db, KeyStack);
	{leaf_list, Key, TS} ->
	    lager:debug("delete: found leaf list ~p with spec ~p", [Key, TS]),
	    %% Rest must be [N] ??
	    {stay, delete_from_list_in_db(hd(Rest), Db, push(Key, KeyStack))};
	_O ->
	    lager:debug("delete: unknown item ~p = ~p", [Key, _O]),
	    {error, unknown_item}		
    end.

delete_menu([Key | Rest], Spec, SubSpec, Db, KeyStack) ->
    lager:debug("delete: found ~p with spec ~p", [Key, SubSpec]),
    case find_in_db(Key, Db) of
	{Key, SubDb} -> 
	    lager:debug("delete: found ~p with db ~p", [Key, SubDb]),
	    case delete(Rest, SubSpec, SubDb, push(Key, KeyStack)) of
		{error, _Reason} = E ->
		    E;
		{stay, NewDb} ->
		    {stay, update_db(Key, NewDb, Db, KeyStack)}
	    end;
	false -> 
	    lager:debug("delete: ~p not found in db", [Key]),
	    {error, unknown_item}
    end.

find_in_db(Key, Db) ->
    lists:keyfind(Key, 1, Db).

replace_db(New, []) ->
    New;
replace_db(New, [ Old | Rest]) ->
    lager:debug("replace_db: ~p, ~p, ~p", [New, Old, Rest]),
    [New | Rest].

update_db(Key, Value, Db, KeyStack) ->
    lager:debug("update_db:  ~p = ~p, ~p, ~p", [Key, Value, Db, KeyStack]),
    lists:keystore(Key, 1, Db, {Key, Value}).
    

add_to_db(Key, Spec, Db, KeyStack) ->
    lager:debug("add_to_db:  ~p, ~p, ~p, ~p", 
		[Key, Spec, Db, KeyStack]),
    %% Fetch defaults etc from spec??
    lists:keystore(Key, 1, Db, {Key, []}).
    
remove_from_db(Key, Spec, Db, KeyStack) ->
    lager:debug("remove_from_db:  ~p, ~p, ~p, ~p", 
		[Key, Spec, Db, KeyStack]),
    %% Replace with defaults etc from spec??
    lists:keydelete(Key, 1, Db).
 
delete_from_list(N, List, KeyStack) ->
    lager:debug("delete_from_list:  ~p, ~p, ~p", [N, List, KeyStack]),
    {Start, [_Remove | End]} = lists:split(N-1, List),
    Start ++ End.

delete_from_list_in_db(N, Db, KeyStack) ->
    lager:debug("delete_from_list_in_db:  ~p, ~p, ~p", [N, Db, KeyStack]),
    {Key, List} = find_in_db(hd(KeyStack), Db), %% Must exist ??
    {Start, [_Remove | End]} = lists:split(N-1, List),
    NewList = Start ++ End,
    lists:keyreplace(Key, 1, Db, {Key, NewList}).

add_to_list(N, Position, Value, List, KeyStack) ->
    lager:debug("add_to_list: after, ~p = ~p, ~p, ~p", 
		[N, Value, List, KeyStack]),
    N1 = case Position of
	     "after" -> N;
	     "before" -> N-1;
	     "first" -> 0;
	     "last" -> length(List)
	 end,
    {Start, End} = lists:split(N1, List),
    Start ++ [Value] ++ End.

update_list(N, Value, List, KeyStack) ->
    lager:debug("update_list:  ~p = ~p, ~p, ~p", 
		[N, Value, List, KeyStack]),
    {Start, [_Remove | End]} = lists:split(N-1, List),
    Start ++ [Value] ++ End.

add_to_list_in_db(N, Position, Value, Db, KeyStack) ->
    lager:debug("add_to_list_in_db:  ~p = ~p, ~p, ~p", 
		[N, Value, Db, KeyStack]),
    {Key, List} = find_in_db(hd(KeyStack), Db), %% Must exist ??
    N1 = case Position of
	     "after" -> N;
	     "before" -> N-1;
	     "first" -> 0;
	     "last" -> length(List)
	 end,
    case lists:split(N1, List) of
	{Start, End} ->
	    NewList = Start ++ [Value] ++ End,
	    lists:keyreplace(Key, 1, Db, {Key, NewList});
	_ ->
	    {error, unknown_item}
    end.

update_list_in_db(N, Value, Db, KeyStack) ->
    lager:debug("update_list_in_db:  ~p = ~p, ~p, ~p", 
		[N, Value, Db, KeyStack]),
    {Key, List} = find_in_db(hd(KeyStack), Db), %% Must exist ??
    case lists:split(N-1, List) of
	{Start, [_Remove | End]} ->
	    NewList = Start ++ [Value] ++ End,
	    lists:keyreplace(Key, 1, Db, {Key, NewList});
	_ ->
	    {error, unknown_item}
    end.

show(Db, KeyStack, Output) ->
    %%lager:debug("show: ~p, ~p.", [Db, KeyStack]),
    lists:foldl(fun({Key, Value}, _Acc) when is_list(Value)->
			case is_string(Value) of
			    true -> Output({Key, Value});
			    false -> Output(Key), 
				     show(Value, KeyStack, Output), 
				     Output({"end", Key})
			end;
		   ({Key, Value}, _Acc) ->
			Output({Key, Value})
		end, [], Db).
		


split(String) when is_list(String) ->
    convert(string:tokens(String, [$.]),[]).
	
convert([], Acc) ->
    lists:reverse(Acc);
convert([String | Rest], Acc) ->
    convert(Rest, [format(String) | Acc]).

format(String) when is_list(String) ->
    try list_to_integer(String) of
	I -> I
    catch 
	error:_E -> 
	    list_to_atom(String)
    end.



verify_type(X, string) -> {true, X};
verify_type(X, undefined) -> {true, X};
verify_type(X, anyxml) when is_list(X); is_binary(X) -> {true, X};
verify_type(X, {enumeration, Enums}) ->
    E = list_to_atom(X),
    case lists:keymember(E, 1, Enums) of
	true -> {true, E};
	false -> false
    end;
verify_type(X, binary) ->
    try list_to_binary(X) of
	Bin -> {true, Bin}
    catch error:_ -> false
    end;
verify_type(X, boolean) -> 
    case list_to_atom(X) of
	true ->{true, true};
	false ->{true, false};
	_ -> false
    end;
verify_type("", empty) -> {true, ""};	    
verify_type(_, empty) -> false;
verify_type(X, decimal64) -> 
    try list_to_float(X) of
	F -> {true, F}
    catch error:_ -> false
    end;
verify_type(N, integer)  ->
    try list_to_integer(N) of
	I -> {true, I}
    catch error:_ ->false
    end;
verify_type(N, T) when T == uint8;
		       T == uint16;
		       T == uint32;
		       T == uint64;
		       T == int8;
		       T == int16;
		       T == int32;
		       T == int64 ->
    try verify_integer_type(list_to_integer(N), T) of
	true -> {true, list_to_integer(N)}
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


spec() ->
    {menu,
	[{item, number, integer},
	 {item, name, string},
	 {menu, data_port, [{item, number, integer},
			    {item, speed, integer},
			    {menu, description, [{item, number, integer},
						 {item, name, string}]}]},
	 {leaf_list, ports, integer},
	 {item, description, string},
	 {item, data_timeout, integer},
	 {menu, client, [{item, name, string},
			 {item, server_key, integer},
			 {item, client_key, integer}]}]}.


example() ->
    [{number, 10},
     {name, "hej"},
     {data_port, [{number,12}, {description, [{number, 17}, {name, "bar"}]}]},
     {ports, [8080, 8081]},
     {description,  "foo"}].

