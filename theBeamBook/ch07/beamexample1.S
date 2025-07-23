{module, beamexample1}.  %% version = 0

{exports, [{id,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, id, 1, 2}.
  {label,1}.
    {line,[{location,"beamexample1.erl",4}]}.
    {func_info,{atom,beamexample1},{atom,id},1}.
  {label,2}.
    {test,is_integer,{f,1},[{x,0}]}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,beamexample1},{atom,module_info},0}.
  {label,4}.
    {move,{atom,beamexample1},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,beamexample1},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,beamexample1},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
