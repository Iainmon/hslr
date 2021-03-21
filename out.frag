#include "runtime.glsl"
void program(inout vec3 color) { 
	float _cache_0x1 = (normalize(((cos(uv) . x) + (sin(uv) . y))) / 2.0);
	float _cache_0x2 = cos(((u_time * 0.4) + ((uv . x) * 30.0)));
	float _cache_0x3 = ((u_time * 0.4) + ((uv . x) * 30.0));
	float _cache_0x4 = (uv . x);
	
	color = vec3(((cos(((u_time * 0.4) + (_cache_0x4 * 30.0))) * 0.6) * sin((((100.0 * sin(u_time)) * (uv . y)) + u_time))), _cache_0x1, (cos(((u_time * 0.4) + (_cache_0x4 * 30.0))) * sqrt(u_time)));
}
