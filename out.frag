#include "runtime.glsl"
#include "noise.glsl"
void program(inout vec3 color) { 
	
	color = mix(vec3(0.988, 0.729, 1.17e-2), vec3(1.0, 1.0, 1.0), smoothstep(((0.2 * perlin((10.0 + (uv - vec2(0.5, 0.5)) + u_time))) + (0.2 * perlin(((uv - vec2(0.5, 0.5)) + u_time))) + 0.2 + (((0.5 * sin(u_time)) + 0.5) * 0.2)), (2.0e-2 + (0.2 * perlin((10.0 + (uv - vec2(0.5, 0.5)) + u_time))) + (0.2 * perlin(((uv - vec2(0.5, 0.5)) + u_time))) + 0.2 + (((0.5 * sin(u_time)) + 0.5) * 0.2)), sqrt(((((uv - vec2(0.5, 0.5)) . x) * ((uv - vec2(0.5, 0.5)) . x)) + (((uv - vec2(0.5, 0.5)) . y) * ((uv - vec2(0.5, 0.5)) . y))))));
}
