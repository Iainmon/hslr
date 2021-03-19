#include "runtime.glsl"
void program(inout vec3 color) { 
	float _cache_0x1 = cos(((u_time * 0.4) + (uv.x * 30.0)));
	float _cache_0x2 = ((u_time * 0.4) + (uv.x * 30.0));
	
	color = vec3(((cos(((u_time * 0.4) + (uv.x * 30.0))) * 0.6) * sin((((100.0 * sin(u_time)) * uv.y) + u_time))), (normalize((cos(uv.x) + sin(uv.y))) / 2.0), (cos(_cache_0x2) * sqrt(u_time)));
}
