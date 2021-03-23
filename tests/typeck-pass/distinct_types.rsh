// A test showing the interactions between type aliases, distinct aliases
// and records.

type
    A = int;
    B = A;

    C = distinct B;
    D = C;
    
    E = distinct int;

    R = record end
    S = record end
    T = S;

// args: --dump-type-context
//
// expected stdout:
// A = (0) int
// B = (0) int
// C = (1) int
// D = (1) int
// E = (2) int
// R = (3) record  end
// S = (4) record  end
// T = (4) record  end