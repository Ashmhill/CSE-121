-module(todo).
-export([start/0, stop/0, add_task/3, view_tasks/0, delete_task/1, remote_view_tasks/0]).


-record(task, {id, title, priority, due_date}).
-define(SERVER, todo_server).

%%% API Functions %%%

% Start the To-Do server
start() ->
    case whereis(?SERVER) of
        undefined -> 
            register(?SERVER, spawn(fun() -> server_loop([]) end)),
            io:format("To-Do server started.~n");
        _ ->
            io:format("To-Do server is already running.~n")
    end.

% Stop the To-Do server
stop() ->
    case whereis(?SERVER) of
        undefined ->
            io:format("To-Do server is not running.~n");
        _ ->
            ?SERVER ! stop,
            io:format("To-Do server stopped.~n")
    end.


% Add a task to the To-Do server
add_task(Title, Priority, DueDate) ->
    whereis(?SERVER) ! {add_task, self(), Title, Priority, DueDate},
    receive
        {ok, Task} -> Task
    after 2000 ->
        {error, timeout}
    end.

% View all tasks, updated to support distributed communication
view_tasks() ->
    % Directly attempt to call the remote function on node1
    rpc:call('node1@Ashleys-MacBook-Air-231', ?MODULE, remote_view_tasks, []).


% Delete a task by ID
delete_task(TaskID) ->
    whereis(?SERVER) ! {delete_task, self(), TaskID},
    receive
        {ok, TaskID} -> TaskID;
        {error, not_found} -> {error, "Task not found"}
    after 2000 ->
        {error, timeout}
    end.

%%% Remote Call Helper Functions %%%

% Helper function to view tasks remotely
remote_view_tasks() ->
    whereis(?SERVER) ! {view_tasks, self()},
    receive
        {tasks, Tasks} -> Tasks
    after 2000 ->
        {error, timeout}
    end.

%%% Server Loop %%%

server_loop(Tasks) ->
    receive
        {add_task, From, Title, Priority, DueDate} ->
            NewTask = #task{id = length(Tasks) + 1, title = Title, priority = Priority, due_date = DueDate},
            From ! {ok, NewTask},
            server_loop([NewTask | Tasks]);

        {view_tasks, From} ->
            From ! {tasks, lists:reverse(Tasks)},
            server_loop(Tasks);

        {delete_task, From, TaskID} ->
            case lists:keyfind(TaskID, #task.id, Tasks) of
                false ->
                    From ! {error, not_found},
                    server_loop(Tasks);
                _ ->
                    NewTasks = lists:filter(fun(T) -> T#task.id =/= TaskID end, Tasks),
                    From ! {ok, TaskID},
                    server_loop(NewTasks)
            end;

        stop ->
            io:format("Shutting down to-do server.~n"),
            unregister(?SERVER),
            ok;

        _ ->
            io:format("Received an unknown message.~n"),
            server_loop(Tasks)
    end.
