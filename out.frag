#include "runtime.glsl"
void program(inout vec3 color) { 
	color = vec3((cos(((u_time * 0.4) + (uv.x * 30.0))) * 0.6), (cos(((u_time * 0.4) + (uv.x * 30.0))) * 0.3), (cos(((u_time * 0.4) + (uv.x * 30.0))) * 0.5));
}
