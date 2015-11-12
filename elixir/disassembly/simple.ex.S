{module, elixir_compiler_0}.  %% version = 0

{exports, [{'__MODULE__',1},
           {'__RELATIVE__',0},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, '__MODULE__', 1, 2}.
  {label,1}.
    {line,[{location,"simple.ex",1}]}.
    {func_info,{atom,elixir_compiler_0},{atom,'__MODULE__'},1}.
  {label,2}.
    {test,is_eq_exact,{f,1},[{x,0},{literal,{}}]}.
    {allocate,0,0}.
    {move,{literal,<<131,103,100,0,13,110,111,110,111,100,101,64,110,111,104,
                     111,115,116,0,0,0,50,0,0,0,0,0>>},
          {x,0}}.
    {line,[]}.
    {call_ext,1,{extfunc,erlang,binary_to_term,1}}.
    {test_heap,3,1}.
    {put_tuple,2,{x,5}}.
    {put,{x,0}}.
    {put,{integer,0}}.
    {move,{atom,true},{x,2}}.
    {move,{literal,{greet,[{line,2}],nil}},{x,3}}.
    {move,{atom,def},{x,1}}.
    {move,{literal,[{do,<<"Hello there">>}]},{x,4}}.
    {move,{integer,2},{x,0}}.
    {line,[{location,"simple.ex",2}]}.
    {call_ext_last,6,{extfunc,elixir_def,store_definition,6},0}.


{function, '__RELATIVE__', 0, 4}.
  {label,3}.
    {line,[{location,"simple.ex",1}]}.
    {func_info,{atom,elixir_compiler_0},{atom,'__RELATIVE__'},0}.
  {label,4}.
    {move,{literal,<<"simple.ex">>},{x,0}}.
    return.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,elixir_compiler_0},{atom,module_info},0}.
  {label,6}.
    {move,{atom,elixir_compiler_0},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,elixir_compiler_0},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,elixir_compiler_0},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
