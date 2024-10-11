-module(arithmetic).
-export([start_factorializer/0, start_adder/0, start_subtracter/0, start_multiplier/0, start_divider/0,
         factorializer/0, adder/0, subtracter/0, multiplier/0, divider/0,
         factorial_of/2, add/3, subtract/3, multiply/3, divide/3]).

%%
%% Arithmetic process functions
%%

start_factorializer() ->
    spawn(?MODULE, factorializer, []).

start_adder() ->
    spawn(?MODULE, adder, []).

start_subtracter() ->
    spawn(?MODULE, subtracter, []).

start_multiplier() ->
    spawn(?MODULE, multiplier, []).

start_divider() ->
    spawn(?MODULE, divider, []).

%%
%% Core Arithmetic Function Implementations
%%

factorializer() ->
    receive
        {Pid, N} -> Pid ! factorial_of(self(), N),
        factorializer()
    end.

adder() ->
    receive
        {Pid, A, B} -> Pid ! add(self(), A, B),
        adder()
    end.

subtracter() ->
    receive
        {Pid, A, B} -> Pid ! subtract(self(), A, B),
        subtracter()
    end.

multiplier() ->
    receive
        {Pid, A, B} -> Pid ! multiply(self(), A, B),
        multiplier()
    end.

divider() ->
    receive
        {Pid, A, B} -> Pid ! divide(self(), A, B),
        divider()
    end.

%%
%% Factorial Function with Edge Cases
%%

factorial_of(_, N) when is_integer(N), N >= 0 ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, lists:seq(1, N));
factorial_of(_, N) when is_integer(N), N < 0 ->
    {fail, N, is_negative};
factorial_of(_, N) when not is_integer(N) ->
    {fail, N, is_not_integer}.

%%
%% Add Function with Edge Cases
%%

add(_, A, B) when is_number(A) and is_number(B) ->
    A + B;
add(_, A, _) when not is_number(A) ->
    {fail, A, is_not_number};
add(_, _, B) when not is_number(B) ->
    {fail, B, is_not_number}.

%%
%% Subtract Function with Edge Cases
%%

subtract(_, A, B) when is_number(A) and is_number(B) ->
    A - B;
subtract(_, A, _) when not is_number(A) ->
    {fail, A, is_not_number};
subtract(_, _, B) when not is_number(B) ->
    {fail, B, is_not_number}.

%%
%% Multiply Function with Edge Cases
%%

multiply(_, A, B) when is_number(A) and is_number(B) ->
    A * B;
multiply(_, A, _) when not is_number(A) ->
    {fail, A, is_not_number};
multiply(_, _, B) when not is_number(B) ->
    {fail, B, is_not_number}.

%%
%% Divide Function with Edge Cases
%%

divide(_, _, 0) ->
    {fail, 0, division_by_zero};
divide(_, A, B) when is_number(A) and is_number(B) ->
    A / B;
divide(_, A, _) when not is_number(A) ->
    {fail, A, is_not_number};
divide(_, _, B) when not is_number(B) ->
    {fail, B, is_not_number}.

%%
%% Unit Tests
%%

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

factorializer_test_() ->
    {setup, 
        fun() -> 
                Pid = spawn(?MODULE, factorializer, []), 
                register(test_factorializer, Pid)
            end, 
        [
            ?_assertEqual(120, factorial_of(test_factorializer, 5)),
            ?_assertEqual(1, factorial_of(test_factorializer, 0)),
            ?_assertEqual({fail, -3, is_negative}, factorial_of(test_factorializer, -3)),
            ?_assertEqual({fail, bob, is_not_integer}, factorial_of(test_factorializer, bob)),
            ?_assertEqual({fail, 5.0, is_not_integer}, factorial_of(test_factorializer, 5.0))
        ]
    }.

adder_test_() ->
    {setup, 
        fun() -> 
                Pid = spawn(?MODULE, adder, []), 
                register(test_adder, Pid)
            end, 
        [
            ?_assertEqual(8, add(test_adder, 5, 3)),
            ?_assertEqual(0, add(test_adder, 0, 0)),
            ?_assertEqual(0.0, add(test_adder, 0.0, 0.0)),
            ?_assertEqual(0, add(test_adder, -5, 5)),
            ?_assertEqual(1.5, add(test_adder, 0.75, 0.75)),
            ?_assertEqual({fail, bob, is_not_number}, add(test_adder, bob, 3)),
            ?_assertEqual({fail, sue, is_not_number}, add(test_adder, 3, sue)),
            ?_assertEqual({fail, bob, is_not_number}, add(test_adder, bob, sue))
        ]
    }.

subtracter_test_() ->
    {setup, 
        fun() -> 
                Pid = spawn(?MODULE, subtracter, []), 
                register(test_subtracter, Pid)
            end, 
        [
            ?_assertEqual(2, subtract(test_subtracter, 5, 3)),
            ?_assertEqual(0, subtract(test_subtracter, 0, 0)),
            ?_assertEqual(0.0, subtract(test_subtracter, 0.0, 0.0)),
            ?_assertEqual(-10, subtract(test_subtracter, -5, 5)),
            ?_assertEqual(0.75, subtract(test_subtracter, 1.5, 0.75)),
            ?_assertEqual({fail, bob, is_not_number}, subtract(test_subtracter, bob, 3)),
            ?_assertEqual({fail, sue, is_not_number}, subtract(test_subtracter, 3, sue)),
            ?_assertEqual({fail, bob, is_not_number}, subtract(test_subtracter, bob, sue))
        ]
    }.

multiplier_test_() ->
    {setup, 
        fun() -> 
                Pid = spawn(?MODULE, multiplier, []), 
                register(test_multiplier, Pid)
            end, 
        [
            ?_assertEqual(15, multiply(test_multiplier, 5, 3)),
            ?_assertEqual(0, multiply(test_multiplier, 0, 0)),
            ?_assertEqual(0.0, multiply(test_multiplier, 0.0, 0.0)),
            ?_assertEqual(-25, multiply(test_multiplier, -5, 5)),
            ?_assertEqual(1.125, multiply(test_multiplier, 1.5, 0.75)),
            ?_assertEqual({fail, bob, is_not_number}, multiply(test_multiplier, bob, 3)),
            ?_assertEqual({fail, sue, is_not_number}, multiply(test_multiplier, 3, sue)),
            ?_assertEqual({fail, bob, is_not_number}, multiply(test_multiplier, bob, sue))
        ]
    }.

divider_test_() ->
    {setup, 
        fun() -> 
                Pid = spawn(?MODULE, divider, []), 
                register(test_divider, Pid)
            end, 
        [
            ?_assert((1.6 < divide(test_divider, 5, 3)) and (divide(test_divider, 5, 3) < 1.7)),
            ?_assertEqual(-1.0, divide(test_divider, -5, 5)),
            ?_assertEqual(2.0, divide(test_divider, 1.5, 0.75)),
            ?_assertEqual({fail, bob, is_not_number}, divide(test_divider, bob, 3)),
            ?_assertEqual({fail, sue, is_not_number}, divide(test_divider, 3, sue)),
            ?_assertEqual({fail, 0, division_by_zero}, divide(test_divider, 3, 0))
        ]
    }.

-endif.
