-module(rebar_compiler_erl).

-export([src_dirs/1,
         src_ext/0,
         out/1,
         source_map/0,
         needed_files/2,
         compile/4]).

-include("rebar.hrl").

-define(ERLCINFO_VSN, 2).
-define(ERLCINFO_FILE, "erlcinfo").
-type erlc_info_v() :: {digraph:vertex(), term()} | 'false'.
-type erlc_info_e() :: {digraph:vertex(), digraph:vertex()}.
-type erlc_info() :: {list(erlc_info_v()), list(erlc_info_e()), list(string())}.
-record(erlcinfo, {
    vsn = ?ERLCINFO_VSN :: pos_integer(),
    info = {[], [], []} :: erlc_info()
}).

-define(DEFAULT_OUTDIR, "ebin").
-define(RE_PREFIX, "^(?!\\._)").

src_dirs(AppInfo) ->
    OutDir = rebar_app_info:dir(AppInfo),
    SrcDirs = rebar_dir:src_dirs(rebar_app_info:opts(AppInfo), ["src"]),
    lists:filter(fun(D) ->
                         ec_file:is_dir(filename:join(OutDir, D))
                 end, SrcDirs).


src_ext() ->
    ".erl".

out(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    [{".beam", EbinDir}].

source_map() ->
    [].

needed_files(FoundFiles, AppInfo) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    Dir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    RebarOpts = rebar_app_info:opts(AppInfo),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ?DEBUG("erlopts ~p", [ErlOpts]),
    ?DEBUG("files to compile ~p", [FoundFiles]),

    %% Make sure that outdir is on the path
    ok = filelib:ensure_dir(filename:join(EbinDir, "dummy.beam")),
    true = code:add_patha(filename:absname(EbinDir)),

    G = init_erlcinfo(include_abs_dirs(ErlOpts, OutDir), FoundFiles, OutDir, EbinDir),

    {ParseTransforms, Rest} = split_source_files(FoundFiles, ErlOpts),
    NeededErlFiles = case needed_files(G, ErlOpts, RebarOpts, OutDir, EbinDir, ParseTransforms) of
                         [] ->
                             needed_files(G, ErlOpts, RebarOpts, OutDir, EbinDir, Rest);
                         _  ->
                             %% at least one parse transform in the opts needs updating, so recompile all
                             FoundFiles
                     end,
    {ErlFirstFiles, ErlOptsFirst} = erl_first_files(RebarOpts, ErlOpts, Dir, NeededErlFiles),
    SubGraph = digraph_utils:subgraph(G, NeededErlFiles),
    DepErlsOrdered = digraph_utils:topsort(SubGraph),
    OtherErls = lists:reverse(DepErlsOrdered),

    PrivIncludes = [{i, filename:join(OutDir, Src)} || Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])],
    AllOpts = ErlOpts ++ PrivIncludes ++
        [{i, filename:join(OutDir, "include")}, {i, OutDir}, return],

    true = digraph:delete(SubGraph),
    true = digraph:delete(G),

    {{ErlFirstFiles, ErlOptsFirst}, {[Erl || Erl <- OtherErls, not lists:member(Erl, ErlFirstFiles)], AllOpts}}.

compile(Source, [{_, OutDir}], Config, ErlOpts) ->
    %% TargetDir = filename:root_dir(Source),
    case compile:file(Source, [{outdir, OutDir} | ErlOpts]) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, []} ->
            ok;
        {ok, _Mod, Ws} ->
            FormattedWs = format_error_sources(Ws, Config),
            rebar_base_compiler:ok_tuple(Source, FormattedWs);
        {error, Es, Ws} ->
            error_tuple(Source, Es, Ws, Config, ErlOpts);
        error ->
            error
    end.

%%

error_tuple(Module, Es, Ws, AllOpts, Opts) ->
    FormattedEs = format_error_sources(Es, AllOpts),
    FormattedWs = format_error_sources(Ws, AllOpts),
    rebar_base_compiler:error_tuple(Module, FormattedEs, FormattedWs, Opts).

format_error_sources(Es, Opts) ->
    [{rebar_base_compiler:format_error_source(Src, Opts), Desc}
     || {Src, Desc} <- Es].

%% Get files which need to be compiled first, i.e. those specified in erl_first_files
%% and parse_transform options.  Also produce specific erl_opts for these first
%% files, so that yet to be compiled parse transformations are excluded from it.
erl_first_files(Opts, ErlOpts, Dir, NeededErlFiles) ->
    ErlFirstFilesConf = rebar_opts:get(Opts, erl_first_files, []),
    valid_erl_first_conf(ErlFirstFilesConf),
    NeededSrcDirs = lists:usort(lists:map(fun filename:dirname/1, NeededErlFiles)),
    %% NOTE: order of files here is important!
    ErlFirstFiles =
        [filename:join(Dir, File) || File <- ErlFirstFilesConf,
                                     lists:member(filename:join(Dir, File), NeededErlFiles)],
    {ParseTransforms, ParseTransformsErls} =
        lists:unzip(lists:flatmap(
                      fun(PT) ->
                              PTerls = [filename:join(D, module_to_erl(PT)) || D <- NeededSrcDirs],
                              [{PT, PTerl} || PTerl <- PTerls, lists:member(PTerl, NeededErlFiles)]
                      end, proplists:get_all_values(parse_transform, ErlOpts))),
    ErlOptsFirst = lists:filter(fun({parse_transform, PT}) ->
                                        not lists:member(PT, ParseTransforms);
                                   (_) ->
                                        true
                                end, ErlOpts),
    {ErlFirstFiles ++ ParseTransformsErls, ErlOptsFirst}.

split_source_files(SourceFiles, ErlOpts) ->
    ParseTransforms = proplists:get_all_values(parse_transform, ErlOpts),
    lists:partition(fun(Source) ->
                            lists:member(filename_to_atom(Source), ParseTransforms)
                    end, SourceFiles).

filename_to_atom(F) -> list_to_atom(filename:rootname(filename:basename(F))).

%% Get subset of SourceFiles which need to be recompiled, respecting
%% dependencies induced by given graph G.
needed_files(G, ErlOpts, RebarOpts, Dir, OutDir, SourceFiles) ->
    lists:filter(fun(Source) ->
                         TargetBase = target_base(OutDir, Source),
                         Target = TargetBase ++ ".beam",
                         PrivIncludes = [{i, filename:join(Dir, Src)}
                                         || Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])],
                         AllOpts = [{outdir, filename:dirname(Target)}
                                   ,{i, filename:join(Dir, "include")}
                                   ,{i, Dir}] ++ PrivIncludes ++ ErlOpts,
                         digraph:vertex(G, Source) > {Source, filelib:last_modified(Target)}
                              orelse opts_changed(AllOpts, TargetBase)
                              orelse erl_compiler_opts_set()
                 end, SourceFiles).

opts_changed(NewOpts, Target) ->
    TotalOpts = case erlang:function_exported(compile, env_compiler_options, 0) of
        true  -> NewOpts ++ compile:env_compiler_options();
        false -> NewOpts
    end,
    case compile_info(Target) of
        {ok, Opts} -> lists:any(fun effects_code_generation/1, lists:usort(TotalOpts) -- lists:usort(Opts));
        _          -> true
    end.

effects_code_generation(Option) ->
    case Option of
        beam -> false;
        report_warnings -> false;
        report_errors -> false;
        return_errors-> false;
        return_warnings-> false;
        report -> false;
        warnings_as_errors -> false;
        binary -> false;
        verbose -> false;
        {cwd,_} -> false;
        {outdir, _} -> false;
        _ -> true
    end.

compile_info(Target) ->
    case beam_lib:chunks(Target, [compile_info]) of
        {ok, {_mod, Chunks}} ->
            CompileInfo = proplists:get_value(compile_info, Chunks, []),
            {ok, proplists:get_value(options, CompileInfo, [])};
        {error, beam_lib, Reason} ->
            ?WARN("Couldn't read debug info from ~p for reason: ~p", [Target, Reason]),
            {error, Reason}
    end.

erl_compiler_opts_set() ->
    EnvSet = case os:getenv("ERL_COMPILER_OPTIONS") of
        false -> false;
        _     -> true
    end,
    %% return false if changed env opts would have been caught in opts_changed/2
    EnvSet andalso not erlang:function_exported(compile, env_compiler_options, 0).

erlcinfo_file(Dir) ->
    filename:join(rebar_dir:local_cache_dir(Dir), ?ERLCINFO_FILE).

include_abs_dirs(ErlOpts, BaseDir) ->
    ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
    InclDirs = lists:map(fun(Incl) -> filename:absname(Incl) end, ErlOptIncludes),
    [filename:join([BaseDir, "include"])|InclDirs].

%% Get dependency graph of given Erls files and their dependencies (header files,
%% parse transforms, behaviours etc.) located in their directories or given
%% InclDirs. Note that last modification times stored in vertices already respect
%% dependencies induced by given graph G.
init_erlcinfo(InclDirs, Erls, Dir, EbinDir) ->
    G = digraph:new([acyclic]),
    try restore_erlcinfo(G, InclDirs, Dir)
    catch
        _:_ ->
            ?WARN("Failed to restore ~ts file. Discarding it.~n", [erlcinfo_file(Dir)]),
            file:delete(erlcinfo_file(Dir))
    end,
    Dirs = source_and_include_dirs(InclDirs, Erls),
    %% A source file may have been renamed or deleted. Remove it from the graph
    %% and remove any beam file for that source if it exists.
    Modified = maybe_rm_beams_and_edges(G, EbinDir, Erls),
    Modified1 = lists:foldl(update_erlcinfo_fun(G, Dirs), Modified, Erls),
    if Modified1 -> store_erlcinfo(G, InclDirs, Dir); not Modified1 -> ok end,
    G.

restore_erlcinfo(G, InclDirs, Dir) ->
    case file:read_file(erlcinfo_file(Dir)) of
        {ok, Data} ->
            % Since externally passed InclDirs can influence erlcinfo graph (see
            % modify_erlcinfo), we have to check here that they didn't change.
            #erlcinfo{vsn=?ERLCINFO_VSN, info={Vs, Es, InclDirs}} =
                binary_to_term(Data),
            lists:foreach(
              fun({V, LastUpdated}) ->
                      digraph:add_vertex(G, V, LastUpdated)
              end, Vs),
            lists:foreach(
              fun({_, V1, V2, _}) ->
                      digraph:add_edge(G, V1, V2)
              end, Es);
        {error, _} ->
            ok
    end.

store_erlcinfo(G, InclDirs, Dir) ->
    Vs = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
    Es = lists:map(fun(E) -> digraph:edge(G, E) end, digraph:edges(G)),
    File = erlcinfo_file(Dir),
    ok = filelib:ensure_dir(File),
    Data = term_to_binary(#erlcinfo{info={Vs, Es, InclDirs}}, [{compressed, 2}]),
    file:write_file(File, Data).

valid_erl_first_conf(FileList) ->
    Strs = filter_file_list(FileList),
    case rebar_utils:is_list_of_strings(Strs) of
        true -> true;
        false -> ?ABORT("An invalid file list (~p) was provided as part of your erl_first_files directive",
                        [FileList])
    end.

filter_file_list(FileList) ->
    Atoms = lists:filter( fun(X) -> is_atom(X) end, FileList),
    case Atoms of
        [] ->
            FileList;
        _ ->
          atoms_in_erl_first_files_warning(Atoms),
          lists:filter( fun(X) -> not(is_atom(X)) end, FileList)
     end.

atoms_in_erl_first_files_warning(Atoms) ->
  W = "You have provided atoms as file entries in erl_first_files; "
      "erl_first_files only expects lists of filenames as strings. "
      "The following modules (~p) may not work as expected and it is advised "
      "that you change these entires to string format "
      "(e.g., \"src/module.erl\") ",
  ?WARN(W, [Atoms]).

module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

maybe_rm_beams_and_edges(G, Dir, Files) ->
    Vertices = digraph:vertices(G),
    case lists:filter(fun(File) ->
                              case filename:extension(File) =:= ".erl" of
                                  true ->
                                      maybe_rm_beam_and_edge(G, Dir, File);
                                  false ->
                                      false
                              end
                      end, lists:sort(Vertices) -- lists:sort(Files)) of
        [] ->
            false;
        _ ->
            true
    end.

source_and_include_dirs(InclDirs, Erls) ->
    SourceDirs = lists:map(fun filename:dirname/1, Erls),
    lists:usort(InclDirs ++ SourceDirs).

update_erlcinfo(G, Dirs, Source) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            case filelib:last_modified(Source) of
                0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    modified;
                LastModified when LastUpdated < LastModified ->
                    modify_erlcinfo(G, Source, LastModified, filename:dirname(Source), Dirs);
                _ ->
                    Modified = lists:foldl(
                        update_erlcinfo_fun(G, Dirs),
                        false, digraph:out_neighbours(G, Source)),
                    MaxModified = update_max_modified_deps(G, Source),
                    case Modified orelse MaxModified > LastUpdated of
                        true -> modified;
                        false -> unmodified
                    end
            end;
        false ->
            modify_erlcinfo(G, Source, filelib:last_modified(Source), filename:dirname(Source), Dirs)
    end.

update_erlcinfo_fun(G, Dirs) ->
    fun(Erl, Modified) ->
        case update_erlcinfo(G, Dirs, Erl) of
            modified -> true;
            unmodified -> Modified
        end
    end.

maybe_rm_beam_and_edge(G, OutDir, Source) ->
    %% This is NOT a double check it is the only check that the source file is actually gone
    case filelib:is_regular(Source) of
        true ->
            %% Actually exists, don't delete
            false;
        false ->
            Target = target_base(OutDir, Source) ++ ".beam",
            ?DEBUG("Source ~ts is gone, deleting previous beam file if it exists ~ts", [Source, Target]),
            file:delete(Target),
            digraph:del_vertex(G, Source),
            true
    end.

update_max_modified_deps(G, Source) ->
    MaxModified = lists:max(lists:map(
        fun(File) -> {_, MaxModified} = digraph:vertex(G, File), MaxModified end,
        [Source|digraph:out_neighbours(G, Source)])),
    digraph:add_vertex(G, Source, MaxModified),
    MaxModified.

modify_erlcinfo(G, Source, LastModified, Dir, Dirs) ->
    {ok, Fd} = file:open(Source, [read]),
    Incls = parse_attrs(Fd, [], Dir),
    AbsIncls = expand_file_names(Incls, Dirs),
    ok = file:close(Fd),
    digraph:add_vertex(G, Source, LastModified),
    digraph:del_edges(G, digraph:out_edges(G, Source)),
    lists:foreach(
      fun(Incl) ->
              update_erlcinfo(G, Dirs, Incl),
              digraph:add_edge(G, Source, Incl)
      end, AbsIncls),
    modified.

parse_attrs(Fd, Includes, Dir) ->
    case io:parse_erl_form(Fd, "") of
        {ok, Form, _Line} ->
            case erl_syntax:type(Form) of
                attribute ->
                    NewIncludes = process_attr(Form, Includes, Dir),
                    parse_attrs(Fd, NewIncludes, Dir);
                _ ->
                    parse_attrs(Fd, Includes, Dir)
            end;
        {eof, _} ->
            Includes;
        _Err ->
            parse_attrs(Fd, Includes, Dir)
    end.

process_attr(Form, Includes, Dir) ->
    AttrName = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
    process_attr(AttrName, Form, Includes, Dir).

process_attr(import, Form, Includes, _Dir) ->
    case erl_syntax_lib:analyze_import_attribute(Form) of
        {Mod, _Funs} ->
            [module_to_erl(Mod)|Includes];
        Mod ->
            [module_to_erl(Mod)|Includes]
    end;
process_attr(file, Form, Includes, _Dir) ->
    {File, _} = erl_syntax_lib:analyze_file_attribute(Form),
    [File|Includes];
process_attr(include, Form, Includes, _Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = erl_syntax:string_value(FileNode),
    [File|Includes];
process_attr(include_lib, Form, Includes, Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    RawFile = erl_syntax:string_value(FileNode),
    maybe_expand_include_lib_path(RawFile, Dir) ++ Includes;
process_attr(behavior, Form, Includes, _Dir) ->
    process_attr(behaviour, Form, Includes, _Dir);
process_attr(behaviour, Form, Includes, _Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = module_to_erl(erl_syntax:atom_value(FileNode)),
    [File|Includes];
process_attr(compile, Form, Includes, _Dir) ->
    [Arg] = erl_syntax:attribute_arguments(Form),
    case erl_syntax:concrete(Arg) of
        {parse_transform, Mod} ->
            [module_to_erl(Mod)|Includes];
        {core_transform, Mod} ->
            [module_to_erl(Mod)|Includes];
        L when is_list(L) ->
            lists:foldl(
              fun({parse_transform, Mod}, Acc) ->
                      [module_to_erl(Mod)|Acc];
                 ({core_transform, Mod}, Acc) ->
                      [module_to_erl(Mod)|Acc];
                 (_, Acc) ->
                      Acc
              end, Includes, L);
        _ ->
            Includes
    end;
process_attr(_, _Form, Includes, _Dir) ->
    Includes.

%% NOTE: If, for example, one of the entries in Files, refers to
%% gen_server.erl, that entry will be dropped. It is dropped because
%% such an entry usually refers to the beam file, and we don't pass a
%% list of OTP src dirs for finding gen_server.erl's full path. Also,
%% if gen_server.erl was modified, it's not rebar's task to compile a
%% new version of the beam file. Therefore, it's reasonable to drop
%% such entries. Also see process_attr(behaviour, Form, Includes).
-spec expand_file_names([file:filename()],
                        [file:filename()]) -> [file:filename()].
expand_file_names(Files, Dirs) ->
    %% We check if Files exist by itself or within the directories
    %% listed in Dirs.
    %% Return the list of files matched.
    lists:flatmap(
      fun(Incl) ->
              case filelib:is_regular(Incl) of
                  true ->
                      [Incl];
                  false ->
                      lists:flatmap(
                        fun(Dir) ->
                                FullPath = filename:join(Dir, Incl),
                                case filelib:is_regular(FullPath) of
                                    true ->
                                        [FullPath];
                                    false ->
                                        []
                                end
                        end, Dirs)
              end
      end, Files).

%% Given a path like "stdlib/include/erl_compile.hrl", return
%% "OTP_INSTALL_DIR/lib/erlang/lib/stdlib-x.y.z/include/erl_compile.hrl".
%% Usually a simple [Lib, SubDir, File1] = filename:split(File) should
%% work, but to not crash when an unusual include_lib path is used,
%% utilize more elaborate logic.
maybe_expand_include_lib_path(File, Dir) ->
    File1 = filename:basename(File),
    case filename:split(filename:dirname(File)) of
        [_] ->
            warn_and_find_path(File, Dir);
        [Lib | SubDir] ->
            case code:lib_dir(list_to_atom(Lib), list_to_atom(filename:join(SubDir))) of
                {error, bad_name} ->
                    warn_and_find_path(File, Dir);
                AppDir ->
                    [filename:join(AppDir, File1)]
            end
    end.

%% The use of -include_lib was probably incorrect by the user but lets try to make it work.
%% We search in the outdir and outdir/../include to see if the header exists.
warn_and_find_path(File, Dir) ->
    SrcHeader = filename:join(Dir, File),
    case filelib:is_regular(SrcHeader) of
        true ->
            [SrcHeader];
        false ->
            IncludeDir = filename:join(rebar_utils:droplast(filename:split(Dir))++["include"]),
            IncludeHeader = filename:join(IncludeDir, File),
            case filelib:is_regular(IncludeHeader) of
                true ->
                    [filename:join(IncludeDir, File)];
                false ->
                    []
            end
    end.
