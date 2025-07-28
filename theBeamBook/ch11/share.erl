-module(share).
-export([share/2, size/1]).

share(0,Y) -> {Y,Y};
share(N,Y) -> [share(N-1, [N|Y])  || _ <- Y].

size(N) ->
    T = share:share(N, [a,b,c]),
    {{size, erts_debug:size(T)},
    {flat_size, erts_debug:flat_size(T)}}.