%%minino application

-module(templates).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).

%% views
-export([home_view/3]).

-record(state, {}).

%% minino funs

init(_MConf) ->
    {ok, #state{}}.

dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view},
     {home_page, ["home"], home_view}
    ].


%% views

home_view(MReq, _Args, _State) ->
    Breeds =some_cat_breeds(),
    {ok, Html} = minino_api:render_template("home.html", [{breeds, Breeds}]),
    minino_api:response(Html, MReq).


some_cat_breeds() ->
    ["Abyssinian", 
     "American Bobtail", 
     "Bambino", 
     "Bengal", 
     "Birman", 
     "Burmese", 
     "Burmilla", 
     "Dwelf", 
     "Egyptian Mau", 
     "Exotic Shorthair", 
     "German Rex", 
     "Minskin", 
     "Nebelung", 
     "Napoleon", 
     "Persian", 
     "Peterbald", 
     "Snowshoe", 
     "Sokoke", 
     "Somali", 
     "Sphynx", 
     "Tonkinese", 
     "Ukrainian Levkoy", 
     "Ussuri", 
     "York Chocolate Cat"
    ].
