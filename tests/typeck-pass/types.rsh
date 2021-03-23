type
    Vector2D = float2 is Vector in UV;
    AnotherVector2D = float2 is Vector in IlasBra;

    A = record end
    B = A;
    C = record end

    Option<T> = record
        is_some: bool;
        value: T;
    end

    Pair<A, B> = record
        first : A;
        second : B;
    end

    MaybeColour = Option<float4 is Colour>;

    MaybeVector2D = Option<Vector2D>;

    Thingy = Pair<Vector2D, Vector2D>;