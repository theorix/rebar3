-module(rebar_compiler).

-export([compile_all/2]).

-include("rebar.hrl").

-callback src_ext() -> [string()].

-callback src_dirs() -> [file:filename()].

-callback source_map() -> [file:filename()].

-callback needed_files(rebar_app_info:t()) -> [file:filename()].

-callback compile() -> ok | {ok, [string()]} | {ok, [string()], [string()]}.

compile_all(Compilers, AppInfo) ->
    OutDir = rebar_utils:to_list(rebar_app_info:out_dir(AppInfo)),

    %% Make sure that outdir is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),

    %% necessary for erlang:function_exported/3 to work as expected
    %% called here for clarity as it's required by both opts_changed/2
    %% and erl_compiler_opts_set/0 in needed_files
    _ = code:ensure_loaded(compile),

    lists:foreach(fun(CompilerMod) ->
                          run(CompilerMod, AppInfo),
                          case rebar_dir:extra_src_dirs(rebar_app_info:opts(AppInfo), []) of
                              [] ->
                                  ok;
                              ExtraSrcDirs ->
                                  [case filelib:is_dir(filename:join(rebar_app_info:dir(AppInfo), Dir)) of
                                       true ->
                                           AppInfo1 = rebar_app_info:set(AppInfo, src_dirs, [Dir]),
                                           AppInfo2 = rebar_app_info:set(AppInfo1, extra_src_dirs, ["src"]),
                                           EbinDir = filename:join(rebar_app_info:out_dir(AppInfo2), Dir),
                                           AppInfo4 = rebar_app_info:ebin_dir(AppInfo2, EbinDir),
                                           run(CompilerMod, AppInfo4);
                                       _ ->
                                           ok
                                   end || Dir <- ExtraSrcDirs]
                          end
                  end, Compilers),
    ok.

run(CompilerMod, AppInfo) ->
    Dir = rebar_utils:to_list(rebar_app_info:dir(AppInfo)),
    Outs = CompilerMod:out(AppInfo),
    SrcExt = CompilerMod:src_ext(),


    SourceExtRe = "^(?!\\._).*\\" ++ SrcExt ++ [$$],
    FoundFiles = lists:flatmap(fun(SrcDir) ->
                      Recursive = rebar_dir:recursive(rebar_app_info:opts(AppInfo), SrcDir),
                      rebar_utils:find_files_in_dirs([filename:join(Dir, SrcDir)], SourceExtRe, Recursive)
                  end, CompilerMod:src_dirs(AppInfo)),

    {{FirstFiles, FirstFileOpts}, {RestFiles, Opts}} = CompilerMod:needed_files(FoundFiles, AppInfo),

    %% Check opts for flag indicating that compile should check lastmod
    %% CheckLastMod = proplists:get_bool(check_last_mod, Opts),
    %% CheckLastMod = true,

    Config = rebar_app_info:opts(AppInfo),
    compile_each(FirstFiles, FirstFileOpts, Config, Outs, CompilerMod),
    compile_each(RestFiles, Opts, Config, Outs, CompilerMod).

compile_each([], _Opts, _Config, _Outs, _CompilerMod) ->
    ok;
compile_each([Source | Rest], Opts, Config, Outs, CompilerMod) ->
    case CompilerMod:compile(Source, Outs, Config, Opts) of
        ok ->
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        {ok, Warnings} ->
            rebar_base_compiler:report(Warnings),
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        skipped ->
            ?DEBUG("~tsSkipped ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        Error ->
            NewSource = rebar_base_compiler:format_error_source(Source, Config),
            ?ERROR("Compiling ~ts failed", [NewSource]),
            rebar_base_compiler:maybe_report(Error),
            ?DEBUG("Compilation failed: ~p", [Error]),
            ?FAIL
    end,
    compile_each(Rest, Opts, Config, Outs, CompilerMod).
