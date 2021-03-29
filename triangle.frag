#include "lib/runtime.glsl"
#include "lib/noise.glsl"
void program(inout vec3 color) { 
	
	color = (vec3(1.0, 1.0, 1.0) * (((int(floor((gl_FragCoord . x))) & int((gl_FragCoord . y))) == int(0.0)) ? int(1.0) : int(0.0)));
}
