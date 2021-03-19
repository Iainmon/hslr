#include "HaskellSHadingLanguage.glsl"
out vec4 gl_FragColor;
vec3 program(void);
void main () { gl_FragColor = program(); } 
vec3 program(void) { 
	float _cache_0x1 = cos(cos(cos((color * color))));
	float _cache_0x2 = cos((color * color));
	vec3 color;
	color = (((_cache_0x2 + _cache_0x2) + cos(cos(_cache_0x2))) + cos(cos(_cache_0x2)));
	return color;
}
