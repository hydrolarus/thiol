program call_on_non_function
begin
    var x: int := 12(14);
end

// args: --no-colour
//
// expected stderr:
// error: function call on a non-callable value
//   ┌─ ../tests/fail/call_on_non_function.rsh:3:19
//   │
// 3 │     var x: int := 12(14);
//   │                   ^^ not a callable expression
// 
// aborting due to previous error