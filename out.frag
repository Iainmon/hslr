#include "lib/runtime.glsl"
#include "lib/noise.glsl"
void program(inout vec3 color) { 
	float _cache_0x1 = (0.5 * sin(u_time));
	vec2 _cache_0x2 = (uv - vec2(0.5, 0.5));
	float _cache_0x3 = ((_cache_0x1 + 0.5) * 0.2);
	float _cache_0x4 = ((_cache_0x2 . y) * (_cache_0x2 . y));
	float _cache_0x5 = sqrt((((_cache_0x2 . x) * (_cache_0x2 . x)) + _cache_0x4));
	
	color = mix(vec3(0.988, 0.729, 1.17e-2), (0.5 + (0.5 * cos((vec3(0.0, 0.6, 1.0) + 3.1415 + (0.15 * u_time * 5.0))))), smoothstep(((0.2 * perlin((10.0 + _cache_0x2 + u_time))) + (0.2 * perlin((_cache_0x2 + u_time))) + 0.2 + _cache_0x3), (2.0e-2 + (0.2 * perlin((10.0 + _cache_0x2 + u_time))) + (0.2 * perlin((_cache_0x2 + u_time))) + 0.2 + _cache_0x3), _cache_0x5));
}
