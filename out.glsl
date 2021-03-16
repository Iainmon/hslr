#include "HaskellSHadingLanguage.glsl"
out vec4 gl_FragColor;
vec3 program(void);
void main () { gl_FragColor = program(); } 
vec3 program(void) { 
	float _cache_0x1 = (_cache_0x2 + _cache_0x2);
	float _cache_0x2 = _cache_0x2;
	float _cache_0x1 = ((cos(_cache_0x3) + cos(_cache_0x3)) + (cos(_cache_0x3) + cos(_cache_0x3)));
	float _cache_0x2 = (cos(_cache_0x3) + cos(_cache_0x3));
	float _cache_0x3 = _cache_0x3;
	float _cache_0x1 = ((cos((uv * uv)) + cos((uv * uv))) + (cos((uv * uv)) + cos((uv * uv))));
	float _cache_0x2 = (cos((uv * uv)) + cos((uv * uv)));
	float _cache_0x3 = (uv * uv);
	float _cache_0x4 = uv;
	return (_cache_0x1 + (_cache_0x1 * cos(_cache_0x1)));
}
