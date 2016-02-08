%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2015 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%-------------------------------------------------------------------
%% @doc mongopool worker
%% @end
%%%-------------------------------------------------------------------

-module(mongopool_worker).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
                 code_change/3]).

-record(state, {connection}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% Wait mongodb server is available for connections
wait_connection(_, 0) ->
  throw(mongodb_connection_error);
wait_connection(Args, Remaining_retries) ->
  error_logger:info_msg("Remaining retries ~p~n", [Remaining_retries]),
  case mongo:connect(Args) of
    {ok, Connection} ->
      error_logger:info_msg("Connected!~n"),
      Connection;
    _Rest ->
      error_logger:info_msg("Sleep..~n"),
      timer:sleep(2000),
      wait_connection(Args, Remaining_retries - 1)
  end.

init({Args, Retries}) ->
  process_flag(trap_exit, true),
  application:ensure_all_started(mongodb),
  Connection = wait_connection(Args, Retries),
  {ok, #state{connection=Connection}}.

handle_call({find_one, Collection, Selector, Args}, _From,
            #state{connection=Connection}=State) ->
  {reply, mongo:find_one(Connection, Collection, Selector, Args), State};
handle_call({find, Collection, Selector, Args}, _From,
            #state{connection=Connection}=State) ->
  {reply, mongo:find(Connection, Collection, Selector, Args), State};
handle_call({count, Collection, Selector, Limit}, _From,
            #state{connection=Connection}=State) ->
  {reply, mongo:count(Connection, Collection, Selector, Limit), State};
handle_call({command, Command}, _From, #state{connection=Connection}=State) ->
  {reply, mongo:command(Connection, Command), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({insert, Collection, Doc}, #state{connection=Connection}=State) ->
  mongo:insert(Connection, Collection, Doc),
  {noreply, State};
handle_cast({update, Collection, Selector, Doc, Args},
            #state{connection=Connection}=State) ->
  mongo:update(Connection, Collection, Selector, Doc, Args),
  {noreply, State};
handle_cast({delete, Collection, Selector},
            #state{connection=Connection}=State) ->
  mongo:delete(Connection, Collection, Selector),
  {noreply, State};
handle_cast({delete_one, Collection, Selector},
            #state{connection=Connection}=State) ->
  mongo:delete_one(Connection, Collection, Selector),
  {noreply, State};
handle_cast({ensure_index, Collection, IndexSpec},
            #state{connection=Connection}=State) ->
  mongo:ensure_index(Connection, Collection, IndexSpec),
  {noreply, State};
handle_cast(_Msg, State) ->
      {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{connection=Connection}) ->
  ok = mongodb:disconnect(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
