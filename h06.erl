-module(h03).

-compile(export_all).


T = ets:new(table, []).

% options
set, ordered_set, bag, duplicate_bag

public, protected, private

{keypos, 2}

named_table


ets:insert(T, {12, "name", male, 25}).
ets:lookup(T, 12).
ets:insert(T, {12 , "new name", male, 38}).
ets:delete(T, 12).


ets:first(table).
ets:next/prev(table, key).

ets:match(table1, {'$1', '$2', male, 25}) % '_'
ets:match_object(table1, {'$1', '$2', male, 25})
ets:match_delete(table1, {13, '_', '_', '_'})

ets:select(table, match_spec, limit)

match_spec =[{mathc_pattern, [term], term}]
[
  {
    {'$1', '$2', <<1>>, '$3'},
    [
      {'andalso', {'>', '$3', 150}, {'<', '$3', 500}},
      {'orelse',  {'==', '$2', meet}, {'==', '$2', diary}}
    ],
    ['$1'] % '$-' - everything
  }
]

ms() ->
  ets:fun2ms(
    fun({Id, Name, Gender, Age} = User)
      when Age >= 17 andalso Gender =:= male -> [Id, Name]
    end
  ).

ets:select(table1, ms())

ets table has an owner process (keep it running)


insert. delete - atomic, isolated

when iterating won't read the same tuple several times but can skeep



dets - disk ets. the same API + file IO
1 file limited to 2GB
files would be inconsistent if node failed, on startup will be restored from transaction journal, but it would take time


Mnesia - DB