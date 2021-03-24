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

    Wrap<T> = distinct T;
    X = Wrap<int>;
    Y = Wrap<C>;
    Z = Wrap<E>;


// args: --dump-type-context
//
// expected stdout:
// A = int
// B = int
// C = (0) int
// D = (0) int
// E = (1) int
// R = (2) record  end
// S = (3) record  end
// T = (3) record  end
// X = (4) int
// Y = (4) (0) int
// Z = (4) (1) int