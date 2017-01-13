-module(great_ideas_catalogue).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0,
         add_idea/5, get_idea/1,
         ideas_by_author/1, ideas_by_rating/1,
         get_authors/0]).

-record(idea, {id, title, author, rating, description}).


init() ->
    ets:new(great_ideas_table, [set, named_table, {keypos, 2}]),
    ets:insert(great_ideas_table,
               [#idea{id = 1, title = "Мороженое с огурцами", author = "Боб Бобов", rating = 100,
                      description = "Крошим огурцы кубиками и добавляем в мороженое"},
                #idea{id = 2, title = "Добыча воды на Марсе", author = "Билл Билов", rating = 500,
                      description = "Бурим скважины на Марсе, доставляем воду на Землю ракетами"},
                #idea{id = 3, title = "Извлечение энергии квазаров", author = "П. И. Шурупов", rating = 100500,
                      description = "Секретно"},
                #idea{id = 4, title = "Куртка с тремя рукавами", author = "Боб Бобов", rating = 15,
                      description = "Рукава из разных материалов, расчитаны на разную погоду."},
                #idea{id = 5, title = "Кроссовки-степлеры", author = "Олечка", rating = 78,
                      description = "Полезная вещь для офиса и фитнеса"},
                #idea{id = 6, title = "Способ ловли кузнечиков", author = "Алекс Аквамаринов", rating = 777,
                      description = "Сачком их, сачком."},
                #idea{id = 7, title = "Вулканический зонт", author = "Боб Бобов", rating = 12,
                      description = "Защищает самолеты от вулканической пыли."},
                #idea{id = 8, title = "Телефон-шар", author = "Див Стобс", rating = 8383,
                      description = "Удобно лежит в руке, имеет устройство ввода на основе гироскопа"},
                #idea{id = 9, title = "Автоматическая кормушка для котов", author = "П. И. Шурупов", rating = 9000,
                      description = "Нужно использовать энергию квазаров для этой цели"},
                #idea{id = 10, title = "Самодвижущаяся лестница", author = "Васисуалий Л.", rating = 42,
                      description = "Имеет большой потенциал применения в небоскребах."}]),
    ok.


add_idea(Id, Title, Author, Rating, Description) ->
    ets:insert(great_ideas_table, #idea{id=Id, title=Title, author=Author, rating=Rating, description=Description}),
    ok.


get_idea(Id) ->
  Ideas = ets:select(great_ideas_table, ms_id(Id)),
  case Ideas of
    [Idea | _] -> {ok, Idea};
    _ -> not_found
  end.


ms_id(SearchId) ->
  ets:fun2ms(
    fun({idea, Id, Title, Author, Rating, Description} = Idea)
      when Id =:= SearchId -> Idea
    end
  ).


ideas_by_author(Author) ->
  ets:select(great_ideas_table, ms_author(Author)).


ms_author(SearchAuthor) ->
  ets:fun2ms(
    fun({idea, Id, Title, Author, Rating, Description} = Idea)
      when Author =:= SearchAuthor -> Idea
    end
  ).


ideas_by_rating(Rating) ->
  ets:select(great_ideas_table, ms_rating(Rating)).

ms_rating(SearchRating) ->
  ets:fun2ms(
    fun({idea, Id, Title, Author, Rating, Description} = Idea)
      when Rating >= SearchRating -> Idea
    end
  ).


get_authors() -> get_authors(ets:first(great_ideas_table), #{}).

get_authors (Cont, Acc) ->
  case Cont of
    '$end_of_table' -> sort_authors( maps:to_list(Acc) );
    _ ->
      [{idea, _, _, Author, _, _}] = ets:lookup(great_ideas_table, Cont),
      case maps:find(Author, Acc) of
        {ok, Value} -> get_authors(ets:next(great_ideas_table, Cont), maps:put(Author, Value+1, Acc));
        error       -> get_authors(ets:next(great_ideas_table, Cont), maps:put(Author, 1, Acc))
      end
  end.

sort_authors (Authors) ->
  lists:sort(fun compare_authors/2, Authors).

compare_authors ({Name1, Count1}, {Name2, Count2}) ->
  if Count1 < Count2 -> false;
     Count1 > Count2 -> true;
     Name1 =< Name2  -> true;
     true            -> false
end.