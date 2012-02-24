%
% leader election protocol
%
% Thomas Arts
%   February 2002

% This algorithm assumes a set of nodes given. The process implemented by this
% module is supposed to run on at least all these nodes (peer nodes).
% The processes may start at arbitrary times and fail during execution. The
% protocol is such that by the end of execution, a leader is selected.
%
% The order on nodes is given by the candidate list. The first active node
% is supposed to become the leader. In case nodes are equally fast, the
% node order in the candidate list determines the order to select the
% leader: the earlier in the list, the more priority to become the leader.

-module(leader3).

% put a comment on the next row to disable tracing.
%-define(TRACE_ON,ok).

% Set up tracing if TRACE_ON is defined.
-ifndef(TRACE_ON).
-define(GEN_SERVER,gen_server).
-define(GEN_OPTIONS,[]).
-else.
-define(GEN_SERVER,gen_server2).
-define(GEN_OPTIONS,[{trace,global_tracer},{send_delay,true}]).
%-define(GEN_OPTIONS,[{trace,global_tracer}]).
-endif.

% implemented as gen_server, because gen_fsm misses some features
%
-behaviour(?GEN_SERVER).

-export([start/2,
         init/1,handle_cast/2,handle_call/3,handle_info/2]).

-import(lists,[member/2,foreach/2,foldl/3]).

-record(data,{leadername,
              candidate_nodes,
              leader = none,
              leadernode = none,
              iteration,
              down = [],
              monitored = [],
              clients = []
             }).

% data.iteration = {[all candidate nodes that have accepted me as leader],
%                   my position in the candidate list}
%
% data invariant:
%    - data.down ++ data.monitored == data.candidate_nodes
%

start(LeaderName,CandidateNodes) ->
  ?GEN_SERVER:start({local,LeaderName},
                        ?MODULE,{LeaderName,CandidateNodes},?GEN_OPTIONS).

% invariants:
%   - every candidate is monitored before it is sent a capture message
%   - a capture message sent to a non-existing process returns 'DOWN' message
%
% properties:
%   - if a candidate is leader of a few processes, then all these processes
%     receive a 'DOWN' message if the leader dies.
%
init({LeaderName,CandidateNodes}) ->
  process_flag(trap_exit,true),
%  sleep(100),
  Data =
    #data{leadername = LeaderName,
          candidate_nodes = CandidateNodes,
          iteration = {[], position(node(),CandidateNodes)}},
  {ok,
    case member(node(),CandidateNodes) of
         true ->
           {candidate,broadcast(capture,Data)};
         false ->
           {waitingworker,Data}
    end}.


handle_call(leader,From,{Role,Data}) ->
  case Data#data.leadernode of
       none ->
         {noreply,{Role,Data#data{clients = [From|Data#data.clients]}}};
       _ ->
         {reply,Data#data.leader,{Role,Data}}
  end.


handle_cast({capture,Iteration,Node,Candidate},{candidate,Data}) ->
  NewData =
    nodeup(node(Candidate),Data),
  case lexcompare(NewData#data.iteration,Iteration) of
       less ->
         ?GEN_SERVER:cast(Candidate,{accept,NewData#data.iteration,self()}),
         {noreply,{captured,
                   NewData#data{leader = Candidate}}};
       greater ->
         % I'll get either an accept or DOWN from Candidate later
         {noreply,{candidate,NewData}}
  end;
handle_cast({accept,Iteration,Candidate},{candidate,Data}) ->
  NewData =
    nodeup(node(Candidate),Data),
  {Captured,_} = Iteration,
  NewIteration =   % inherit all procs that have been accepted by Candidate
    foldl(fun(C,Iter) ->
             add_captured(Iter,C)
          end,NewData#data.iteration,[node(Candidate)|Captured]),
  check_majority(NewData#data{iteration = NewIteration});


handle_cast({capture,Iteration,Node,Candidate},{captured,Data}) ->
  {noreply,{captured,nodeup(node(Candidate),Data)}};
handle_cast({accept,Iteration,Candidate},{captured,Data}) ->
  % forward this to the leader
  ?GEN_SERVER:cast(Data#data.leader,{accept,Iteration,Candidate}),
  {noreply,{captured,nodeup(node(Candidate),Data)}};
handle_cast({elect,Candidate},{_,Data}) ->
  NewData =
    nodeup(node(Candidate),
           Data#data{leader = Candidate,
                     leadernode = node(Candidate),
                     iteration =
                       {[], position(node(), Data#data.candidate_nodes)}
                    }),
  {noreply,{surrendered,NewData}};

handle_cast(Msg,{surrendered,Data}) ->
  Candidate = last(Msg),
  NewData =
    nodeup(node(Candidate),Data),
  % no matter what we get, the leader will get it as well and answers
  {noreply,{surrendered,NewData}};

handle_cast(Msg,{elected,Data}) ->
  Candidate = last(Msg),
  NewData =
    nodeup(node(Candidate),Data),
  ?GEN_SERVER:cast(Candidate,{elect,self()}),
  {noreply,{elected,NewData}}.


handle_info({'DOWN',Ref,process,Candidate,Reason},{Role,Data}) ->
  NewData =
    nodedown(Ref,Candidate,Data),
  case {Role,NewData#data.leader} of
       {candidate,_} ->
         check_majority(NewData);
       {Cand,none} when Cand==captured; Cand==surrendered ->
         check_majority(broadcast(capture,NewData));
       _ ->
         {noreply,{Role,NewData}}
  end.

broadcast(capture,Data) ->
  ToMonitor =
    Data#data.candidate_nodes --
    [node()|[ N || {Ref,N}<-Data#data.monitored]],
  NewData =
    foldl(fun(Node,D) ->
             Ref = erlang:monitor(process,{D#data.leadername,Node}),
             D#data{down = Data#data.down -- [Node], % buggy
             %% D#data{down = D#data.down -- [Node], % correct
                    monitored = [{Ref,Node}|D#data.monitored]}
          end,Data,ToMonitor),
  foreach(fun(Node) ->
             ?GEN_SERVER:cast({NewData#data.leadername,Node},
                             {capture,NewData#data.iteration,node(),self()})
          end,NewData#data.candidate_nodes--[node()]),
  NewData;
broadcast(elect,Data) ->
  %gen_server:abcast(...).
  foreach(fun(Node) ->
             ?GEN_SERVER:cast({Data#data.leadername,Node},
                             {elect,self()})
          end,Data#data.candidate_nodes--[node()]),
  Data#data{leader = self(), leadernode = node()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_majority(Data) ->
%  io:format("check ~p~n",[Data]),
  {Captured,_} = Data#data.iteration,
  AcceptMeAsLeader = length(Captured) + 1,   % including myself
  NrCandidates = length(Data#data.candidate_nodes),
  NrDown = Data#data.down,
  if AcceptMeAsLeader > NrCandidates/2 ->
      io:format("~p :I am the leader~n",[node()]),
      {noreply,{elected,broadcast(elect,Data)}};
     AcceptMeAsLeader+length(NrDown) == NrCandidates ->
%     sleep(1000),
      io:format("~p: I am the leader~n",[node()]),
      {noreply,{elected,broadcast(elect,Data)}};
     true ->
      {noreply,{candidate,Data}}
  end.

sleep(N) ->
    receive
    after N ->
        ok
    end.

add_captured({Captured,Pos},CandidateNode) ->
  {[CandidateNode|[ Node || Node<-Captured, Node =/= CandidateNode]], Pos}.

nodeup(Node,Data) ->
  % make sure process is monitored from now on
  case [ N || {_,N}<-Data#data.monitored, N==Node] of
       [] ->
         Ref = erlang:monitor(process,{Data#data.leadername,Node}),
         Data#data{down = Data#data.down -- [Node],
                   monitored = [{Ref,Node}|Data#data.monitored]};
       _ ->    % already monitored, thus not in down
         Data
  end.

nodedown(Ref,Proc,Data) ->
  Node = findnode(Ref,Data#data.monitored),
  {Captured,Pos} = Data#data.iteration,
%  case lists:member(Node,Data#data.down) of
%      true ->
%     NewDown = Data#data.down;
%      false ->
%     NewDown = [Node|Data#data.down]
%  end,
  case Node == Data#data.leadernode of
       true ->
         Data#data{leader = none,
                   leadernode = none,
                   iteration = {Captured -- [Node],Pos},  % TAKE CARE !
                   down = [Node|Data#data.down],
%          down = NewDown,
                   monitored = Data#data.monitored -- [{Ref,Node}]};
       false ->
         Data#data{iteration = {Captured -- [Node],Pos},  % TAKE CARE !
                   down = [Node|Data#data.down],
%          down = NewDown,
                   monitored = Data#data.monitored -- [{Ref,Node}]}
  end.

findnode(Ref,[{Ref,Node}|_]) ->
  Node;
findnode(Ref,[_|Tail]) ->
  findnode(Ref,Tail).

% position of element counted from end of the list
%
position(X,[]) ->
  0;
position(X,[Head|Tail]) ->
  case X==Head of
       true ->
         length(Tail)+1;
       false ->
         position(X,Tail)
  end.

lexcompare({C1,P1},{C2,P2}) ->
  lexcompare([{length(C1),length(C2)},{P1,P2}]).

lexcompare([]) ->
  equal;
lexcompare([{X,Y}|Rest]) ->
  if X<Y  -> less;
     X==Y -> lexcompare(Rest);
     X>Y  -> greater
  end.

last({_,C}) ->
  C;
last({_,_,C}) ->
  C;
last({_,_,_,C}) ->
  C.
