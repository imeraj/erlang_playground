-module(tree_encode_tests).
-include_lib("eunit/include/eunit.hrl").
-import(tree_encode, [serialize/1, deserialize/1]).

% Testing
tree0() ->
    {leaf, ant}.

tree1() ->
    {node,
        {node,
            {leaf, cat},
            {node,
                {leaf, dog},
                {leaf, emu}
            }
        },
        {leaf, fish}
    }.

leaf_test() ->
    ?assertEqual(tree0(), deserialize(serialize(tree0()))).

node_test() ->
    ?assertEqual(tree1(), deserialize(serialize(tree1()))).

leaf_negative_test() ->
    ?assertError(function_clause, serialize([1, ant])).

node_negative_test() ->
    ?assertError(function_clause, serialize([8,6,2,cat,2,dog,emu,fish])).
