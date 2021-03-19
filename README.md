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
out vec4 gl_FragColor;
vec3 program(void);
void main () { gl_FragColor = program(); } 
vec3 program(void) { 
	float _cache_0x1 = cos(cos(cos((color * color))));
	float _cache_0x2 = cos(cos((color * color)));
	float _cache_0x3 = cos((color * color));
	vec3 color;
	color = ((((uv + _cache_0x3) + _cache_0x3) + cos(cos(_cache_0x3))) + cos(cos(_cache_0x3)));
	return color;
}
```
Regular output
```c#
out vec4 gl_FragColor;
vec3 program(void);
void main () { gl_FragColor = program(); } 
vec3 program(void) { 
	vec3 color;
	color = ((((uv + cos((color * color))) + cos((color * color))) + cos(cos(cos((color * color))))) + cos(cos(cos((color * color)))));
	return color;
}
```