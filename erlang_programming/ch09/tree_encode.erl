-module(tree_encode).
-export([serialize/1, deserialize/1]).

serialize(Tree) ->
   treeToList(Tree).

treeToList({leaf,N}) ->
    [2,N];
treeToList({node, T1, T2}) ->
    TTL1 = treeToList(T1),
    [Size1|_] = TTL1,
    TTL2 = treeToList(T2),
    [Size2|List2] = TTL2,
    [Size1+Size2 | TTL1 ++ List2].


deserialize([_|Ls]) ->
    listToTree(Ls).

listToTree([2,N]) ->
    {leaf,N};
listToTree([N]) ->
    {leaf, N};
listToTree([M|Rest]) ->
    {Code1, Code2} = lists:split(M-1, Rest),
    {
        node,
        listToTree(Code1),
        listToTree(Code2)
    }.
