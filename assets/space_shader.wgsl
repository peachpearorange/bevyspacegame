







// ==========================================
// space_shader.wgsl (Manual Struct Definitions)
// ==========================================

// --- Define Structs Manually ---

// View uniform structure (matching what Bevy provides at group 0, binding 0)
// Only include fields you actually use.
struct View {
    view_proj: mat4x4<f32>,
    inverse_view: mat4x4<f32>,
    // If you need camera world position later, add:
    // world_position: vec3<f32>,
};

// Vertex shader input (matching the mesh attributes Bevy provides)
struct VertexInput {
    @location(0) position: vec3<f32>,
    // Add other attributes if your mesh/shader needs them
    // @location(1) normal: vec3<f32>,
    // @location(2) uv: vec2<f32>,
};

// Data passed from vertex shader to fragment shader
struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>, // Required: Position for rasterizer
    @location(0) world_direction: vec3<f32>,    // Custom: Direction vector for skybox sampling
    // Add other interpolated data if needed
    // @location(1) uv: vec2<f32>,
};

// --- Declare Uniforms ---
// Declare the 'view' variable using the manually defined 'View' struct
@group(0) @binding(0) var<uniform> view: View;

// --- Utilities ---
fn hash31(p: vec3<f32>) -> f32 {
    let p_dot = dot(p, vec3<f32>(127.1, 311.7, 74.7));
    return fract(sin(p_dot) * 43758.5453123);
}

// --- Vertex Shader ---
@vertex
fn vertex(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    // Calculate clip space position, ensuring z=w for max depth
    let model_position = vec4<f32>(in.position, 1.0);
    let clip_position_intermediate = view.view_proj * model_position;
    out.clip_position = clip_position_intermediate.xyww;

    // Calculate world-space direction vector (from camera to vertex)
    // Uses the inverse view matrix to transform the vertex direction (model pos)
    out.world_direction = normalize((view.inverse_view * vec4<f32>(in.position, 0.0)).xyz);

    // Initialize other VertexOutput fields if you added any
    // out.uv = in.uv;

    return out;
}

// --- Fragment Shader ---
@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    // Use the interpolated world direction
    let dir = normalize(in.world_direction);

    // --- Basic Star Generation ---
    var star_color = vec3<f32>(0.0);
    let star_density_scale = 600.0;
    let grid_scale = 1.0;
    var star_hash = hash31(dir * star_density_scale);
    star_hash = pow(star_hash, 600.0); // Sharpen
    let brightness_hash = hash31(dir * grid_scale + vec3(12.3, 45.6, 78.9));
    let star_brightness = brightness_hash * 0.7 + 0.3;
    let base_star_color = mix(vec3(1.0, 1.0, 0.9), vec3(0.9, 0.9, 1.0), hash31(dir * grid_scale * 5.0));
    star_color = base_star_color * star_hash * star_brightness * 1.5; // Brightness scale

    // --- Background Color ---
    let base_space_color = vec3<f32>(0.01, 0.01, 0.03) + vec3(0.0, 0.0, abs(dir.y) * 0.02);

    // --- Combine ---
    var final_color = base_space_color + star_color;
    final_color = clamp(final_color, vec3<f32>(0.0), vec3<f32>(1.0)); // Clamp color

    return vec4<f32>(final_color, 1.0);
}
