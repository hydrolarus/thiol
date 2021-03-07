program primitive_type_constructors_invalid_position
begin
    var x: int := float3;
end

// args: --no-colour
//
// expected stderr:
// error: type constructor used in an invalid position
//   ┌─ ../tests/fail/primitive_type_constructor_invalid_position.rsh:3:19
//   │
// 3 │     var x: int := float3;
//   │                   ^^^^^^ invalid position for type constructor
// 
// aborting due to previous error