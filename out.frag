#include "lib/runtime.glsl"
#include "lib/noise.glsl"
void program(inout vec3 color) { 
	float _0x1 = sin(u_time);
	vec2 _0x2 = vec2(0.5, 0.5);
	float _0x3 = (0.5 * _0x1);
	vec2 _0x4 = (uv - _0x2);
	float _0x5 = (_0x3 + 0.5);
	float _0x6 = (_0x4 . y);
	float _0x7 = perlin((_0x4 + u_time));
	vec2 _0x8 = (10.0 + _0x4 + u_time);
	float _0x9 = perlin(_0x8);
	float _0xa = (0.2 * _0x9);
	float _0xb = (_0x6 * _0x6);
	float _0xc = ((0.2 * _0x7) + 0.2 + (_0x5 * 0.2));
	float _0xd = (_0xc + _0xa);
	
	color = mix(vec3(0.988, 0.729, 1.17e-2), (0.5 + (0.5 * cos((vec3(0.0, 0.6, 1.0) + 3.1415 + (0.15 * u_time * 5.0))))), smoothstep(_0xd, (_0xd + 2.0e-2), sqrt((((_0x4 . x) * (_0x4 . x)) + _0xb))));
}
