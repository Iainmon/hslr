#include "runtime.glsl"
#include "noise.glsl"
void program(inout vec3 color) { 
	float _cache_0x1 = ((9.0 / 256.0) * pow((((uv - vec2(0.5, 0.5)) * 3.0) . y), 2.0));
	float _cache_0x2 = pow((((uv - vec2(0.5, 0.5)) * 3.0) . y), 2.0);
	float _cache_0x3 = (((uv - vec2(0.5, 0.5)) * 3.0) . y);
	vec3 _cache_0x4 = vec3(0.0, 0.6, 1.0);
	float _cache_0x5 = (1.0 / 256.0);
	
	color = mix(vec3(1.0, 1.0, 1.0), (0.5 + (0.5 * cos((_cache_0x4 + 3.1415 + (((10.0 * perlin((((uv - vec2(0.5, 0.5)) * 8.0) + u_time))) + (u_time * 2.0)) * 0.15))))), clamp(0.0, 1.0, (10000.0 * ((((((1.0 / 16.0) * pow(_cache_0x3, 6.0)) + (((pow((((uv - vec2(0.5, 0.5)) * 3.0) . x), 10.0) - ((5.0 / 2.0) * pow((((uv - vec2(0.5, 0.5)) * 3.0) . x), 8.0))) + ((35.0 / 16.0) * pow((((uv - vec2(0.5, 0.5)) * 3.0) . x), 6.0))) - ((25.0 / 32.0) * pow((((uv - vec2(0.5, 0.5)) * 3.0) . x), 4.0))) + ((25.0 / 256.0) * pow((((uv - vec2(0.5, 0.5)) * 3.0) . x), 2.0))) - ((3.0 / 32.0) * pow(_cache_0x3, 4.0))) + ((9.0 / 256.0) * pow(_cache_0x3, 2.0))) - _cache_0x5))));
}
