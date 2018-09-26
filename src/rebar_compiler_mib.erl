-module(rebar_compiler_mib).

-export([src_dirs/1,
         src_ext/0,
         out/1,
         source_map/0,
         needed_files/2,
         compile/4]).

-include("rebar.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

src_dirs(_) ->
    ["mibs"].

src_ext() ->
    ".mib".

out(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    [{".bin", filename:join([Dir, "priv", "mibs"])},
     {".hrl", filename:join(Dir, "include")}].

source_map() ->
    [].

needed_files(FoundFiles, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles, not lists:member(Source, FirstFiles)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), mib_opts, []),
    {{FirstFiles, Opts}, {RestFiles, Opts}}.

compile(Source, OutDirs, _, Opts) ->
    {_, BinOut} = lists:keyfind(".bin", 1, OutDirs),
    {_, HrlOut} = lists:keyfind(".hrl", 1, OutDirs),

    ok = filelib:ensure_dir(filename:join(BinOut, "dummy.bin")),
    ok = filelib:ensure_dir(filename:join(HrlOut, "dummy.hrl")),
    Mib = filename:join(BinOut, filename:basename(Source, ".mib")),
    HrlFilename = Mib ++ ".hrl",

    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,

    case snmpc:compile(Source, AllOpts) of
        {ok, _} ->
            MibToHrlOpts =
                case proplists:get_value(verbosity, AllOpts, undefined) of
                    undefined ->
                        #options{specific = [],
                                 cwd = rebar_dir:get_cwd()};
                    Verbosity ->
                        #options{specific = [{verbosity, Verbosity}],
                                 cwd = rebar_dir:get_cwd()}
                end,
            ok = snmpc:mib_to_hrl(Mib, Mib, MibToHrlOpts),
            rebar_file_utils:mv(HrlFilename, HrlOut),
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.
