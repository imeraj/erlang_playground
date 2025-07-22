-module(beamfile).
-export([read/1]).

read(Filename) ->
    {ok, File} = file:read_file(Filename),
    <<"FOR1", Size:32/integer, "BEAM", Chunks/binary>> = File,
    AllChunks = read_chunks(Chunks, []),
    {Size, AllChunks, parse_chunks(AllChunks, [])}.

read_chunks(<<N, A, M, E, Size:32/integer, Tail/binary>>, Acc) ->
    ChunkLength = align_by_four(Size),
    <<Chunk:ChunkLength/binary, Rest/binary>> = Tail,
    read_chunks(Rest, [{[N, A, M, E], Size, Chunk} | Acc]);
read_chunks(<<>>, Acc) ->
    lists:reverse(Acc).

parse_chunks([{"AtU8", _Size, <<_NumberofAtoms:32/integer, Atoms/binary>>} | Rest], Acc) ->
    parse_chunks(Rest, [{atoms, parse_atoms(Atoms)} | Acc]);
parse_chunks([{"ExpT", _Size, <<_NumofEntries:32/integer, Exports/binary>>}| Rest], Acc) ->
    parse_chunks(Rest, [{exports, parse_exports(Exports)} | Acc]);
parse_chunks([{"ImpT", _Size, <<_NumofEntries:32/integer, Imports/binary>>}| Rest], Acc) ->
    parse_chunks(Rest, [{imports, parse_imports(Imports)} | Acc]);
parse_chunks([{"Code", Size, <<SubSize:32/integer, Chunk/binary>>} | Rest], Acc) ->
    <<Info:SubSize/binary, Code/binary>> = Chunk,
    %% 8 is size of SubSize and ChunkSize (Size)
    OpCodeSize = Size - SubSize - 8,
    <<OpCodes:OpCodeSize/binary, _Align/binary>> = Code,
    parse_chunks(Rest, [{code, parse_code_info(Info), OpCodes} | Acc]);
parse_chunks([{"StrT", _Size, <<Strings/binary>>} | Rest], Acc) ->
    parse_chunks(Rest, [{strings, binary_to_list(Strings)} | Acc]);
parse_chunks([{"Attr", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
     Attribs = binary_to_term(Bin),
     parse_chunks(Rest,[{attributes,Attribs} | Acc]);
parse_chunks([{"CInf", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
     CInfo = binary_to_term(Bin),
     parse_chunks(Rest,[{compile_info,CInfo} | Acc]);
parse_chunks([{"LocT", _Size, <<_NumofEntries:32/integer, FuncTable/binary>>}| Rest], Acc) ->
    parse_chunks(Rest, [{local_func_table, parse_func_table(FuncTable)} | Acc]);
parse_chunks([{"LitT", _ChunkSize, <<_UnCompressedTableSize:32, Compressed/binary>>} | Rest], Acc) ->
    <<_NumLiterals:32, Table/binary>> = zlib:uncompress(Compressed),
    Literals = parse_literals(Table),
    parse_chunks(Rest, [{literals, Literals} | Acc]);
parse_chunks([{"Abst", _ChunkSize, <<>>} | Rest], Acc) ->
    parse_chunks(Rest,Acc);
parse_chunks([{"Abst", _ChunkSize, <<AbstractCode/binary>>} | Rest], Acc) ->
    parse_chunks(Rest,[{abstract_code,binary_to_term(AbstractCode)}|Acc]);
parse_chunks([{"Meta", Size, Chunk} | Rest], Acc) ->
    <<MetaInfo:Size/binary, _Pad/binary>> = Chunk,
    Meta = binary_to_term(MetaInfo),
    parse_chunks(Rest, [{meta, Meta}|Acc]);
parse_chunks([_Chunk|Rest], Acc) ->
    parse_chunks(Rest, Acc);
parse_chunks([], Acc) -> Acc.

parse_atoms(<<AtomLength, Atom:AtomLength/binary, Rest/binary>>) when AtomLength > 0 ->
    [list_to_atom(binary_to_list(Atom)) | parse_atoms(Rest)];
parse_atoms(_Alignment) -> [].

parse_literals(<<Size:32, Literal:Size/binary, Rest/binary>>) ->
    [binary_to_term(Literal) | parse_literals(Rest)];
parse_literals(<<>>) -> [].

parse_exports(<<Function:32/integer, Arity:32/integer, Label:32/integer, Rest/binary>>) ->
    [{Function, Arity, Label} | parse_exports(Rest)];
parse_exports(<<>>) -> [].

parse_imports(<<Module:32/integer, Function:32/integer, Arity:32/integer, Rest/binary>>) ->
    [{Module, Function, Arity} | parse_imports(Rest)];
parse_imports(<<>>) -> [].

parse_func_table(<<Function:32/integer, Arity:32/integer, Label:32/integer, Rest/binary>>) ->
    [{Function, Arity, Label} | parse_func_table(Rest)];
parse_func_table(<<>>) -> [].

parse_code_info(<<InstructionSet:32/integer,
                      OpCodeMax:32/integer,
                      NumberOfLabels:32/integer,
                      NumberOfFunctions:32/integer,
                      Rest/binary>>) ->
        [{instructionset, InstructionSet},
         {opcodemax, OpCodeMax},
         {numberoflabels, NumberOfLabels},
         {numberoffunctions, NumberOfFunctions},
         case Rest of
             <<>> ->
                 [];
             _ ->
                 [{newInfo, Rest}]
         end].

align_by_four(N) -> (4 * ((N + 3) div 4)).
