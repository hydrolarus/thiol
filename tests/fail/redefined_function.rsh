function test(x: int) returns int
begin
    return x;
end

function test(x: int, y: int) returns int
begin
    return y;
end

// args: --no-colour
//
// expected stderr:
// error: function redefinition
//   ┌─ ../tests/fail/redefined_function.rsh:6:10
//   │    
// 1 │   ╭ function test(x: int) returns int
//   │              ---- previous definition of function with the same name
// 2 │   │ begin
// 3 │   │     return x;
// 4 │   │ end
//   │   ╰───'
// 5 │     
// 6 │ ╭   function test(x: int, y: int) returns int
//   │              ^^^^ redefinition of function
// 7 │ │   begin
// 8 │ │       return y;
// 9 │ │   end
//   │ ╰─────'
// 
// aboring due to previous error