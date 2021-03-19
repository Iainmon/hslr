#include "runtime.glsl"
void program(inout vec3 color) { 
	float _cache_0x1 = cos(_cache_0x2);
	float _cache_0x1 = cos(cos(_cache_0x2));
	float _cache_0x2 = cos(_cache_0x2);
	float _cache_0x1 = cos(cos(cos((color * color))));
	float _cache_0x2 = cos((color * color));
	
	color = (((_cache_0x2 + _cache_0x2) + _cache_0x1) + _cache_0x1);
}
