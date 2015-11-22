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
%% @doc mongopool public API
%% @end
%%%-------------------------------------------------------------------

-module(mongopool_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

-export([insert/3, update/4, update/5, delete/3, delete_one/3,
         find_one/3, find_one/4, find/3, find/4, count/3, count/4,
         ensure_index/3, command/2]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mongopool_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions: mongodb interface
%%====================================================================

%% @doc Insert a document into the collection.
%%      Returns the document with an auto-generated _id if missing.
-spec insert(term(), mongo:collection(), A) -> A.
insert(PoolName, Collection, Doc) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {insert, Collection, Doc})
  end).

%% @doc Update a document into the collection.
%%      Returns the document with an auto-generated _id if missing.
-spec update(
        term(), mongo:collection(), mongo:selector(), bson:document()) -> ok.
update(PoolName, Collection, Selector, Doc) ->
  update(PoolName, Collection, Selector, Doc, []).

%% @doc Update a document into the collection.
%%      Returns the document with an auto-generated _id if missing.
-spec update(term(), mongo:collection(), mongo:selector(),
             bson:document(), proplists:proplist()) -> ok.
update(PoolName, Collection, Selector, Doc, Args) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {update, Collection, Selector, Doc, Args})
  end).

%% @doc Delete selected documents
-spec delete(term(), mongo:collection(), mongo:selector()) -> ok.
delete(PoolName, Collection, Selector) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {delete, Collection, Selector})
  end).

%% @doc Delete first selected documents
-spec delete_one(term(), mongo:collection(), mongo:selector()) -> ok.
delete_one(PoolName, Collection, Selector) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {delete_one, Collection, Selector})
  end).

%% @doc Return first selected document, if any
-spec find_one(term(), mongo:collection(),
               mongo:selector()) -> {} | {bson:document()}.
find_one(PoolName, Collection, Selector) ->
  find_one(PoolName, Collection, Selector, []).

%% @doc Return first selected document, if any
-spec find_one(term(), mongo:collection(), mongo:selector(),
               proplists:proplist()) -> {} | {bson:document()}.
find_one(PoolName, Collection, Selector, Args) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {find_one, Collection, Selector, Args})
  end).

%% @doc Return selected documents.
-spec find(term(), mongo:collection(), mongo:selector()) -> mongo:cursor().
find(PoolName, Collection, Selector) ->
  find(PoolName, Collection, Selector, []).

%% @doc Return selected documents.
-spec find(term(), mongo:collection(), mongo:selector(),
           proplists:proplist()) -> mongo:cursor().
find(PoolName, Collection, Selector, Args) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {find, Collection, Selector, Args})
  end).

%% @doc Count selected documents
-spec count(term(), mongo:collection(), mongo:selector()) -> integer().
count(PoolName, Collection, Selector) ->
  count(PoolName, Collection, Selector, 0).

%% @doc Count selected documents up to given max number; 0 means no max.
%%      Ie. stops counting when max is reached to save processing time.
-spec count(term(), mongo:collection(), mongo:selector(),
            integer()) -> integer().
count(PoolName, Collection, Selector, Limit) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {count, Collection, Selector, Limit})
  end).

%% @doc Create index on collection according to given spec.
%%      The key specification is a bson documents with the following fields:
%%      key      :: bson document,
%%                  for e.g.
%%                  {field, 1, other, -1, location, 2d},
%%                     <strong>required</strong>
%%      name     :: bson:utf8()
%%      unique   :: boolean()
%%      dropDups :: boolean()
-spec ensure_index(term(), mongo:collection(), bson:document()) -> ok.
ensure_index(PoolName, Collection, IndexSpec) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {ensure_index, Collection, IndexSpec})
  end).

%% @doc Execute given MongoDB command and return its result.
-spec command(term(), bson:document()) -> {boolean(),
                                           bson:document()}. % Action
command(PoolName, Command) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {command, Command})
  end).

% TODO sync_command
