const
    A: int := 12;

    B: int := 24;

    A: bool := true;

// args: --no-colour
//
// expected stderr:
// error: constant redefinition
//   ┌─ ../tests/fail/redefined_constant.rsh:6:5
//   │
// 2 │     A: int := 12;
//   │     -------------
//   │     │
//   │     previous definition of constant with the same name
//   ·
// 6 │     A: bool := true;
//   │     ^---------------
//   │     │
//   │     redefinition of constant
// 
// aboring due to previous error