#include "HaskellSHadingLanguage.glsl"
out vec4 gl_FragColor;
vec3 program(void);
void main () { gl_FragColor = program(); } 
vec3 program(void) { 
	float _cache_0x1 = ((cos((uv * uv)) + cos((uv * uv))) + (cos((uv * uv)) + cos((uv * uv))));
	float _cache_0x2 = (cos((uv * uv)) + cos((uv * uv)));
	float _cache_0x3 = cos(uv);
	float _cache_0x4 = (uv * uv);
	vec3 color;
	color = (((((cos(_cache_0x4) + cos(_cache_0x4)) + (cos(_cache_0x4) + cos(_cache_0x4))) + (((cos(_cache_0x4) + cos(_cache_0x4)) + (cos(_cache_0x4) + cos(_cache_0x4))) * cos(((cos(_cache_0x4) + cos(_cache_0x4)) + (cos(_cache_0x4) + cos(_cache_0x4)))))) + cos(cos(cos(cos(cos(cos(_cache_0x3))))))) + cos(cos(cos(cos(cos(cos(cos(cos(cos(cos(cos(cos(cos(_cache_0x3))))))))))))));
	return color;
}
