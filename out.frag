#include "runtime.glsl"
#include "noise.glsl"
void program(inout vec3 color) { 
	vec2 _cache_0x1 = (20.0 * (uv - vec2(0.5, 0.5)));
	float _cache_0x2 = (0.5 * sin(u_time));
	
	color = mix(vec3(0.988, 0.729, 1.17e-2), vec3(1.0, 1.0, 1.0), smoothstep((_cache_0x2 + 0.5), (1.0e-3 + _cache_0x2 + 0.5), perlin(((uv * 10.0) + perlin((_cache_0x1 + u_time))))));
}
