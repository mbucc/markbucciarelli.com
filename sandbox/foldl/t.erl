-module(t).


%% Notes:
%  http://erlang.org/pipermail/erlang-questions/2012-May/066647.html
%  http://stackoverflow.com/a/9945513

-compile(export_all).


sum(X, Sum) -> element(2, X) + Sum.


%   30> t:group1({a,2}, [{a,1}]).
%   1, Rest=[]
%   [{a,[2]}]
%   31> 
group1({Act, Val}, [_|Rest]) ->
	io:format("1, Rest=~p~n", [Rest]),
	H = {Act, [Val]},
	[H|Rest];
group1({Act, Val}, []) ->
	io:put_chars("2\n"),
	[{Act, [Val]}].


%  35> t:group2({a,2}, [{a,1}]).
%  [{a,[2]},{a,1}]
%  36> t:group2({a,2}).         
%  ** exception error: undefined function t:group2/1
%  37> t:group2({a,2}, []).
%  [{a,[2]}]
%  38> 
group2({Act, Val}, Acc) ->
	H = {Act, [Val]},
	[H|Acc].


%  42> t:group3({a,2}, [{a,1}]).
%  [{a,[2|1]}]
%  43> t:group3({a,2}, []).     
%  [{a,[2]}]
%  44>
%
%  First test was wrong!!  Should have been:
%
%  45> t:group3({a,2}, [{a,[1]}]).
%  [{a,[2,1]}]
%
%  47> lists:foldl(fun t:group3/2, [], L).
%  [{b,[1]},{a,[2,1]}]
%
%	Done!
%
group3({Act, Val}, [{Act, Vals}|Rest] ) ->
	H1 = {Act, [Val|Vals]},
	[H1|Rest];
group3({Act, Val}, Acc) ->
	H = {Act, [Val]},
	[H|Acc].

% Want min value in first list element.
% So result should be [ {act, Minval, AllVals}, ...]
%
%
% 49> lists:foldl(fun t:group4/2, [], L).
% [{b,1,[1]},{a,1,[2,1]}]
% 50> 
%
group4({Act, Val}, [{Act, FirstVal, Vals}|Rest] ) ->
	H1 = {Act, FirstVal, [Val|Vals]},
	[H1|Rest];
group4({Act, Val}, Acc) ->
	H = {Act, Val, [Val]},
	[H|Acc].



%  65> c(t).
%  {ok,t}
%  66> rr(t).
%  [event]
%  67> Events = [#event{act="Big Apple", date={2018, 8, 20}, price=50.00}, 
%  67> #event{act="Big Apple", date={2018, 8, 21}, price=50.00},           
%  67> #event{act="Red Sox", date={2018, 8, 20}, price=80.00}].            
%  [#event{act = "Big Apple",date = {2018,8,20},price = 50.0},
%   #event{act = "Big Apple",date = {2018,8,21},price = 50.0},
%   #event{act = "Red Sox",date = {2018,8,20},price = 80.0}]
%  68> lists:foldl(fun t:group5/2, [], Events).
%  [{"Red Sox",
%    #event{act = "Red Sox",date = {2018,8,20},price = 80.0},
%    [{2018,8,20}]},
%   {"Big Apple",
%    #event{act = "Big Apple",date = {2018,8,20},price = 50.0},
%    [{2018,8,21},{2018,8,20}]}]
%  69> 
%  

-record(event, {act, date, price}).

group5(#event{act = Act} = E, [{Act, FirstVal, Dates}|Rest] ) ->
	H1 = {Act, FirstVal, [E#event.date|Dates]},
	[H1|Rest];
group5(E, Acc) when is_record(E, event) ->
	H = {E#event.act, E, [E#event.date]},
	[H|Acc].


