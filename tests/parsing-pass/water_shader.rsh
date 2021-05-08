// args: --parse-only

const
    WATER_COLOUR: float3 is Colour := float3(0, 117 / 255, 242 / 255);
    Z_NEAR: float := 10;
    Z_FAR: float := 400;

// std stubs for now
type
    Texture<C> = record end
    Sampler<C> = record end

type
    Uniforms = record
        view: float4x4;
        projection: float4x4;
        time_size_width: float;
        viewport_height: float;
    end

const
    [Uniform(set: 0, binding: 0)]
    UNIFORMS: Uniforms;

    [Uniform(set: 0, binding: 1)]
    REFLECTION: Texture<float4 is Colour>;
    
    [Uniform(set: 0, binding: 2)]
    TERRAIN_DEPTH_TEX: Texture<float4>;
    
    [Uniform(set: 0, binding: 3)]
    COLOUR_SAMPLER: Sampler<float4>;


function to_linear_depth(depth: float) returns float
begin
    var z_n: float := 2 * depth - 1;
    result := 2 * Z_NEAR * Z_FAR / (Z_FAR + Z_NEAR - z_n * (Z_FAR - Z_NEAR));
end



program fragment
input
    [Position]
    frag_coord: float4;

    [Location(0)]
    water_screen_pos: float2 is Point;
    [Location(1)]
    fresnel: float;
    [Location(2)]
    light: float3 is Colour;
output
    [Location(0)]
    colour: float4 is Colour;
begin
    var reflection_colour: float3;
    reflection_colour := REFLECTION.sample(COLOUR_SAMPLER, water_screen_pos).xyz;
    
    var pixel_depth: float := to_linear_depth(frag_coord.z);
    
    var terrain_data: float4;
    terrain_data := TERRAIN_DEPTH_TEX.sample(
        sampler: COLOUR_SAMPLER,
        coord: frag_coord.xy / float2(UNIFORMS.time_size_width.w, UNIFORMS.viewport_height),
    );
    var terrain_depth: float := to_linear_depth(terrain_data.r);
    
    var dist: float := terrain_depth - pixel_depth;
    var clamped: float := smoothstep(lower: 0, upper: 1.5, value: dist).pow(4.8);
    
    colour.a := clamped * (1 - fresnel);
    
    var final_colour: float3 := light + reflection_colour;
    var depth_colour: float3 := mix(
        start: final_colour,
        end_: water_colour,
        value: smoothstep(lower: 1, upper: 5, value: dist) * 2,
    );
    
    colour.xyz := depth_colour;
end