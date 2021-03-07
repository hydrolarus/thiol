program pos_arg_after_named_arg
begin
    var x: int := f(1, 2, x: 12, 45);
end

// args: --no-colour
//
// expected stderr:
// error: positional argument used after named argument
//   ┌─ ../tests/fail/pos_arg_after_named_arg.rsh:3:34
//   │
// 3 │     var x: int := f(1, 2, x: 12, 45);
//   │                           -----  ^^ positional argument used here
//   │                           │       
//   │                           named argument used here
// 
// aborting due to previous error