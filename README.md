# Haskell Shading Language

I have been wanting to make a functional shading language for almost a year, since I started learning Haskell. Here it is

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Control.Monad (when)
import Prelude hiding (cos)

color = var Float "color"
uv = var Float "uv"
cos = cosine

program = uv + (cos (color * color)) + (cos (color * color)) + (cos (cos (cos (color * color)))) + (cos (cos (cos (color * color))))

main = do
    let newContents = generateProgram $ optimize program
    when (length newContents > 0) $
        writeFile "out.glsl" newContents
```

Optimized output
```c#
#include "lib/runtime.glsl"
#include "lib/noise.glsl"
void program(inout vec3 color) { 
	float _cache_0x1 = sin(u_time);
	vec2 _cache_0x2 = vec2(0.5, 0.5);
	float _cache_0x3 = (0.5 * _cache_0x1);
	vec2 _cache_0x4 = (uv - _cache_0x2);
	float _cache_0x5 = (_cache_0x3 + 0.5);
	float _cache_0x6 = (_cache_0x4 . y);
	float _cache_0x7 = perlin((_cache_0x4 + u_time));
	vec2 _cache_0x8 = (10.0 + _cache_0x4 + u_time);
	float _cache_0x9 = perlin(_cache_0x8);
	float _cache_0xa = (0.2 * _cache_0x9);
	float _cache_0xb = (_cache_0x6 * _cache_0x6);
	float _cache_0xc = ((0.2 * _cache_0x7) + 0.2 + (_cache_0x5 * 0.2));
	float _cache_0xd = (_cache_0xc + _cache_0xa);
	
	color = mix(vec3(0.988, 0.729, 1.17e-2), (0.5 + (0.5 * cos((vec3(0.0, 0.6, 1.0) + 3.1415 + (0.15 * u_time * 5.0))))), smoothstep(_cache_0xd, (_cache_0xd + 2.0e-2), sqrt((((_cache_0x4 . x) * (_cache_0x4 . x)) + _cache_0xb))));
}
```
Regular output
```c#
#include "lib/runtime.glsl"
#include "lib/noise.glsl"
void program(inout vec3 color) { 
	
	color = mix(vec3(0.988, 0.729, 1.17e-2), (0.5 + (0.5 * cos((vec3(0.0, 0.6, 1.0) + 3.1415 + (0.15 * u_time * 5.0))))), smoothstep(((0.2 * perlin((10.0 + (uv - vec2(0.5, 0.5)) + u_time))) + (0.2 * perlin(((uv - vec2(0.5, 0.5)) + u_time))) + 0.2 + (((0.5 * sin(u_time)) + 0.5) * 0.2)), (2.0e-2 + (0.2 * perlin((10.0 + (uv - vec2(0.5, 0.5)) + u_time))) + (0.2 * perlin(((uv - vec2(0.5, 0.5)) + u_time))) + 0.2 + (((0.5 * sin(u_time)) + 0.5) * 0.2)), sqrt(((((uv - vec2(0.5, 0.5)) . x) * ((uv - vec2(0.5, 0.5)) . x)) + (((uv - vec2(0.5, 0.5)) . y) * ((uv - vec2(0.5, 0.5)) . y))))));
}
```
