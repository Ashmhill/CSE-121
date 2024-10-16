-module(friend_tracker).
-export([start/1, rpc/2, run/1]).

%%
%% Spawn a process for adding, removing, and finding friends.
%% The parameter is an initial list of friends. It may be an 
%% empty list.
%%
start(Initial_friends_list) ->
    spawn(?MODULE, run, [Initial_friends_list]). % The MODULE macro is used instead of hard coding the module name.

%%--------------------------
%% Client functions
%%--------------------------

%%
%% Add, remove, or find a friend.
%%
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Response ->
            Response
    end.

%%---------------------------
%% Server function
%%---------------------------

run(Friend_list) ->
    receive
        % Add friend(s) to the list (can be a single friend or a list of friends)
        {From, {add, Friend}} when is_list(Friend) ->
            NewFriendList = Friend ++ Friend_list,
            From ! received,
            run(NewFriendList);
        {From, {add, Friend}} ->
            NewFriendList = [Friend | Friend_list],
            From ! received,
            run(NewFriendList);

        % Remove friend(s) from the list
        {From, {remove, Friend}} ->
            NewFriendList = case is_list(Friend) of
                true -> lists:subtract(Friend_list, Friend);
                false -> lists:delete(Friend, Friend_list)
            end,
            From ! received,
            run(NewFriendList);

        % Check if a specific friend is in the list
        {From, {has_friend, Friend}} ->
            Response = lists:member(Friend, Friend_list),
            From ! Response,
            run(Friend_list);

        % Check if all friends in the list are present
        {From, {has_friends, Friends}} when is_list(Friends) ->
            Response = lists:all(fun(Friend) -> lists:member(Friend, Friend_list) end, Friends),
            From ! Response,
            run(Friend_list);

        % Retrieve the full list of friends
        {From, get} ->
            From ! Friend_list,
            run(Friend_list);

        % Handle unknown messages
        {From, _} ->
            From ! {fail, unrecognized_message},
            run(Friend_list)
    end.


%%---------------------------
%% Unit tests (using EUnit)
%%---------------------------
-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

add_friend_test_() ->
{setup,
    fun() -> % Runs before any of the tests to set up the test
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_adder, Pid)
    end,
    fun(_) -> % Runs after all of the tests to clean up from the test
        unregister(test_adder)
    end,

    % Add friend tests start here
    [
        ?_assertEqual(received, rpc(test_adder, {add, bob})), % Test obvious case
        ?_assertEqual(received, rpc(test_adder, {add, 1})), % Test adding a number
        ?_assertEqual(received, rpc(test_adder, {add, #{name => suzannah, age => 23}})) % Test adding a map/dictionary
    ]
}.

add_friends_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_adder, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_adder)
    end,

    % Add friends tests start here
    [
        ?_assertEqual(received, rpc(test_adder, {add, [bob, alice, joe]})), % Happy path
        ?_assertEqual(received, rpc(test_adder, {add, []})), % Edge case: adding an empty list
        ?_assertEqual(received, rpc(test_adder, {add, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]})) % Test adding multiple maps
    ]
}.

has_friend_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_finder, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_finder)
    end,

    % Has friend tests start here
    [
        ?_assert(rpc(test_finder, {has_friend, sue})), % Should return true
        ?_assertNot(rpc(test_finder, {has_friend, bob})), % Should return false
        ?_assertNot(rpc(test_finder, {has_friend, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]})) % Should return false for a map
    ]
}.

has_friends_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_finder, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_finder)
    end,

    % Has friends tests start here
    [
        ?_assert(rpc(test_finder, {has_friends, [sue, fred]})), % Should return true
        ?_assert(rpc(test_finder, {has_friends, []})), % Should return true for an empty list
        ?_assertNot(rpc(test_finder, {has_friends, [bob]})), % Should return false
        ?_assertNot(rpc(test_finder, {has_friends, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]})) % Should return false for maps
    ]
}.

remove_friend_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_remover, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_remover)
    end,

    % Remove friend tests start here
    [
        ?_assertEqual(received, rpc(test_remover, {remove, fred})), % Happy path
        ?_assertEqual(received, rpc(test_remover, {remove, bob})), % Edge case: removing a non-existent friend
        ?_assertEqual(received, rpc(test_remover, {remove, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]})) % Edge case: removing maps
    ]
}.

remove_friends_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_remover, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_remover)
    end,

    % Remove friends tests start here
    [
        ?_assertEqual(received, rpc(test_remover, {remove, [sue, fred]})), % Happy path
        ?_assertEqual(received, rpc(test_remover, {remove, [bob]})), % Edge case: removing a non-existent friend
        ?_assertEqual(received, rpc(test_remover, {remove, [#{name => suzannah, age => 23}, #{name => gunhild, age => 20}]})) % Edge case: removing maps
    ]
}.

get_friends_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_remover, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_remover)
    end,

    % Get friends tests start here
    [
        ?_assertEqual([sue, grace, fred], rpc(test_remover, get)) % Should return the full list of friends
    ]
}.

bad_message_test_() ->
{setup,
    fun() -> % Runs before any of the tests
        Pid = spawn(?MODULE, run, [[sue, grace, fred]]),
        register(test_bad_message, Pid)
    end,
    fun(_) -> % Runs after all of the tests
        unregister(test_bad_message)
    end,

    % Bad message tests start here
    [
        ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, what)), % Test unknown message
        ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, nil)),
        ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, [])),
        ?_assertMatch({fail, unrecognized_message}, rpc(test_bad_message, {}))
    ]
}.

-endif.
