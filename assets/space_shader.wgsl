// Import Bevy's PBR bindings if needed, or define necessary ones.
// For a simple skybox, we might only need the view direction.
// Bevy's Skybox pipeline typically provides world position or view direction.
// Let's assume we receive the world-space direction vector from the vertex shader.

//----------------------------------------------------------------------------//
// Utilities / Noise Functions                                                //
//----------------------------------------------------------------------------//

// Simple pseudo-random number generator (hash function)
// Takes a 3D vector, returns a pseudo-random float between 0.0 and 1.0
fn hash31(p: vec3<f32>) -> f32 {
    // Use sine functions for a simple hash - results are not cryptographically secure,
    // but good enough for visual randomness.
    // Using large numbers and dot products helps mix the inputs.
    let p_dot = dot(p, vec3<f32>(127.1, 311.7, 74.7));
    return fract(sin(p_dot) * 43758.5453123); // fract() keeps it in [0, 1)
}

// 3D Fractional Brownian Motion (FBM) - layered noise for nebula clouds
// `p`: position/direction in 3D space
// `octaves`: number of noise layers to combine
// `persistence`: how much influence each subsequent octave has (usually < 1)
// `lacunarity`: how much detail/frequency increases for each octave (usually > 1)
fn fbm3d(p: vec3<f32>, octaves: i32, persistence: f32, lacunarity: f32) -> f32 {
    var total: f32 = 0.0;
    var frequency: f32 = 1.0;
    var amplitude: f32 = 1.0;
    var max_value: f32 = 0.0; // Used for normalization

    for (var i = 0; i < octaves; i = i + 1) {
        // Simple noise: Use hash function on integer grid cells and interpolate
        // For simplicity here, we'll just use the hash directly without interpolation
        // A proper noise function (like Perlin or Simplex) would give smoother results
        // but is more complex to implement in WGSL from scratch.
        // Let's use a scaled hash for a basic cloudy effect.
        total = total + hash31(p * frequency) * amplitude;

        max_value = max_value + amplitude;
        amplitude = amplitude * persistence;
        frequency = frequency * lacunarity;
    }

    // Normalize the result to be roughly between 0.0 and 1.0
    if max_value > 0.0 {
        return total / max_value;
    } else {
        return 0.0;
    }
}


//----------------------------------------------------------------------------//
// Vertex Shader                                                              //
//----------------------------------------------------------------------------//
// This struct defines the output of the vertex shader
// It must match the input struct of the fragment shader
struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    // Pass the direction vector from the camera to the vertex in world space
    @location(0) world_direction: vec3<f32>,
};

// Define the View bind group structure expected from Bevy
// (Typically group 0, binding 0 for view uniforms)
struct View {
    view_proj: mat4x4<f32>, // Combined view-projection matrix
    inverse_view: mat4x4<f32>, // Inverse view matrix (camera world transform)
    // Add other fields if needed (e.g., world_position) based on Bevy version/setup
};
@group(0) @binding(0) var<uniform> view: View;

// Define the vertex inputs expected from the mesh
// Bevy's default mesh pipeline provides these locations.
// For a skybox, we often use a simple cube mesh.
struct VertexInput {
     @location(0) position: vec3<f32>, // Vertex position in model space
     // Add other attributes like normals or UVs if needed, though likely not for this shader
     // @location(1) normal: vec3<f32>,
     // @location(2) uv: vec2<f32>,
};


// *** THE FIX IS HERE: Renamed function from vs_main to vertex ***
// This is the entry point Bevy expects by default.
@vertex
fn vertex(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    // Calculate the world-space position of the vertex.
    // For a skybox cube centered at the origin, the model-space position
    // directly represents the direction vector from the center.
    let model_position = vec4<f32>(in.position, 1.0);

    // Calculate the clip space position.
    // We want the skybox to be infinitely far away, so we set z = w.
    // Use the view_proj matrix but manipulate the result.
    let clip_position_intermediate = view.view_proj * model_position;
    out.clip_position = clip_position_intermediate.xyww; // Force z=w for max depth

    // Calculate the world-space direction vector.
    // This is the vector from the camera's position to the vertex position.
    // For a skybox, we can often approximate this by transforming the model-space
    // position (which acts as a direction) by the inverse view matrix
    // (which contains the camera's orientation in world space).
    // We only care about direction, so use vec4(..., 0.0) for the transform
    // to ignore translation and normalize the result.
    out.world_direction = normalize((view.inverse_view * vec4<f32>(in.position, 0.0)).xyz);

    return out;
}


//----------------------------------------------------------------------------//
// Fragment Shader                                                            //
//----------------------------------------------------------------------------//

// *** Ensure the fragment shader entry point is named fragment ***
// (Or fs_main if you configure Bevy differently, but 'fragment' is common)
@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let dir = normalize(in.world_direction); // Ensure direction is normalized

    // --- Star Generation ---
    var star_color = vec3<f32>(0.0);
    // Use hash function on the direction vector to generate stars
    // Increase density by scaling the input vector
    let star_density_scale = 500.0;
    var star_hash = hash31(dir * star_density_scale);

    // Create sharp points for stars: raise hash to a high power
    // Only keep the brightest spots
    star_hash = pow(star_hash, 500.0); // Higher power = smaller, sharper stars

    // Add some brightness variation based on another hash
    let star_brightness_hash = hash31(dir * star_density_scale + vec3(123.4, 567.8, 910.1));
    let star_brightness = star_brightness_hash * 0.5 + 0.5; // Range [0.5, 1.0]

    // Define star color (mostly white, slightly bluish/yellowish variations possible)
    let base_star_color = vec3<f32>(0.95, 0.95, 1.0); // Slightly blue-white
    star_color = base_star_color * star_hash * star_brightness * 2.0; // Scale brightness


    // --- Nebula Generation ---
    var nebula_color = vec3<f32>(0.0);
    // Use FBM noise based on the direction vector
    let nebula_scale = 2.0; // Controls the overall size of nebula clouds
    let nebula_octaves = 5; // Number of noise layers
    let nebula_persistence = 0.45; // How much detail persists in higher octaves
    let nebula_lacunarity = 2.1; // How much frequency increases per octave

    let fbm_noise = fbm3d(dir * nebula_scale, nebula_octaves, nebula_persistence, nebula_lacunarity);

    // Map noise value to color and intensity
    // Example: Use noise to control density of a colored cloud
    let nebula_density = smoothstep(0.3, 0.6, fbm_noise); // Create smoother transitions

    // Define nebula colors (e.g., deep blues, purples, faint reds)
    let nebula_color1 = vec3<f32>(0.1, 0.05, 0.3); // Deep purple/blue
    let nebula_color2 = vec3<f32>(0.4, 0.1, 0.2); // Faint reddish/purple

    // Use another layer of noise or the same noise to mix colors
    let color_mix_noise = hash31(dir * nebula_scale * 0.5 + vec3(42.0)); // Slower variation for color mix
    nebula_color = mix(nebula_color1, nebula_color2, smoothstep(0.4, 0.6, color_mix_noise));

    // Apply density
    nebula_color = nebula_color * nebula_density * 0.8; // Adjust intensity


    // --- Combine Layers ---
    // Start with a very dark base color for space
    let base_space_color = vec3<f32>(0.01, 0.01, 0.02); // Very dark blue

    // Add nebula first, then stars on top
    var final_color = base_space_color + nebula_color + star_color;

    // Clamp color to avoid issues, although HDR might be desirable in a PBR setup
    final_color = clamp(final_color, vec3<f32>(0.0), vec3<f32>(1.0)); // Basic clamp

    // Output final color with full alpha
    return vec4<f32>(final_color, 1.0);
}
