#include "runtime.glsl"
void program(inout vec3 color) { 
	
	color = vec3((sin((((uv . y) * 100.0 * sin(u_time)) + u_time)) * (uv + uv + uv + (uv * uv * uv * uv) + uv) * 0.6), (normalize(((cos(uv) . x) + (sin(uv) . y))) / 2.0), ((uv + uv + uv + (uv * uv * uv * uv) + uv) * sqrt(u_time)));
}
