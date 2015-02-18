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

-export([spec/0, example/0, menu/0]).

menu() ->
    {menu, Spec} = spec(),
    Config = example(),
    Db = load([], Config, []),
    lager:debug("menu: db ~p", [Db]),
    Output = fun(Key) ->
		     io:format("~p~n",[Key])
	     end,
    Input = fun() ->
		    io:get_line(">")
	    end,

    menu_loop([Spec], Db, [], Output, Input).



load(_KeyStack, [], Acc) ->
    lists:reverse(Acc);
load(KeyStack, [{Key, Value} | Rest], Acc) when is_list(Value)->
    case is_string(Value) of
	true -> 
	    load(KeyStack, Rest, [{lists:reverse([Key|KeyStack]), Value} | Acc]);
	false ->
	    Acc1 = load([Key| KeyStack], Value, []),
	    lager:debug("load: acc1 ~p", [Acc1]),
	    load(KeyStack, Rest, lists:reverse(Acc1) ++ Acc)
    end;
load(KeyStack, [{Key, Value} | Rest], Acc)->
    load(KeyStack, Rest, [{lists:reverse([Key|KeyStack]), Value} | Acc]).
		  
    

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

show(Db, KeyStack, Output) ->
    lager:debug("show: ~p, ~p.", [Db, KeyStack]),
    lists:foldl(fun({Key, Value}, _Acc) ->
			case lists:prefix(lists:reverse(KeyStack), Key) of
			    true -> pretty_print(Key, Value, Output);
			    false -> ok
			end
		end, [], Db).
		
pretty_print(Key, Value, Output)->		
    Output([Key, Value]).


menu([], Spec, Db, KeyStack, _Output, Input) ->
    Choice = Input(),
    case Choice of
	"..\n" -> back;
	".\n" -> repeat;
	"?\n" -> repeat;
	"show\n" -> show;
	"exit\n" -> exit;
	_ -> scan_input(Choice, Spec, Db, KeyStack)
    end;
menu([{_Type, Key, _TS} | List], Spec, Db, KeyStack, Output, Input) ->
    Output(Key),
    menu(List, Spec, Db, KeyStack, Output, Input).


scan_input(Choice, Spec, Db, KeyStack) ->
    case string:tokens(Choice, [$ , $\n]) of
	[Key, Value] -> 
	    search_spec(list_to_atom(Key), [Value], Spec, Db, KeyStack);
	[Key] -> 
	    search_spec(list_to_atom(Key), [], Spec, Db, KeyStack);
	_ -> repeat
    end.

search_spec(Key, [], Spec, Db, KeyStack) ->
    lager:debug("search_spec: ~p = ~p, ~p, ~p, ~p", 
		[Key, [], Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, _TS} -> {error, value_required};
	{menu, Key, NewSpec} -> {continue, NewSpec, [Key | KeyStack]};
	false -> {error, illegal_choice}
    end;
search_spec(Key, [Value], Spec, Db, KeyStack) ->
    lager:debug("search_spec: ~p = ~p, ~p, ~p, ~p", 
		[Key, Value, Spec, Db, KeyStack]),
    case lists:keyfind(Key, 2, Spec) of
	{item, Key, TS} -> verify_ts(Key, Value, TS, Db, [Key | KeyStack]);
	{menu, Key, _NewSpec} -> {error, superflous_value};
	false -> {error, illegal_choice}
    end.

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
	    
change_config(Key, Value, Db, KeyStack) ->
    lager:debug("change_config: ~p = ~p, ~p", [Key,Value,KeyStack]),
    FullKey = lists:reverse(KeyStack),
    NewDb = lists:keyreplace(FullKey, 1, Db, {FullKey, Value}), 
    lager:debug("change_config: ~p", [FullKey]),
    {back, NewDb, pop(KeyStack)}. 
			       

spec() ->
    {menu,
	[{item, a, integer},
	 {item, b, string},
	 {menu, c, [{item, d, integer},
		    {menu, e, [{item, g, integer},
			       {item, h, string}]}]},
	  {item, f, string}]}.


example() ->
    [{a, 10},
     {b, "hej"},
     {c, [{d,12}, {e, [{g, 17}, {h, "bar"}]}]},
     {f,  "foo"}].


is_string(Value) ->
    try unicode:characters_to_binary(Value) of
	Utf8 when is_binary(Utf8) -> true;
	{error,_,_} -> false
    catch
	error:_ -> false
    end.

