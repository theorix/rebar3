-module(rebar_compiler_xrl).

-export([src_dirs/1,
         src_ext/0,
         out/1,
         source_map/0,
         needed_files/2,
         compile/4]).

src_dirs(_) ->
    ["src"].

src_ext() ->
    ".xrl".

out(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    [{".erl", filename:join([Dir, "src"])}].


source_map() ->
    [].

needed_files(FoundFiles, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles, not lists:member(Source, FirstFiles)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), xrl_opts, []),

    {{FirstFiles, Opts}, {RestFiles, Opts}}.


compile(Source, [{_, OutDir}], _, Opts) ->
    BaseName = filename:basename(Source),
    Target = filename:join([OutDir, BaseName]),
    AllOpts = [{parserfile, Target} | Opts],
    AllOpts1 = [{includefile, filename:join(OutDir, I)} || {includefile, I} <- AllOpts,
                                                           filename:pathtype(I) =:= relative],
    %% case needs_compile(Source, Target) of
    %%     true ->
            case leex:file(Source, AllOpts1 ++ [{return, true}]) of
                {ok, _} ->
                    ok;
                {ok, _Mod, Ws} ->
                    rebar_base_compiler:ok_tuple(Source, Ws);
                {error, Es, Ws} ->
                    rebar_base_compiler:error_tuple(Source,
                                                    Es, Ws, AllOpts1)
            end.
    %%     false ->
    %%         skipped
    %% end.

%% needs_compile(Source, Target) ->
%%     filelib:last_modified(Source) > filelib:last_modified(Target).
