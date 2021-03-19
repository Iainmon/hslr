
in vec2 uv;
out vec3 color;

void regular_program(inout vec3 color)
{
    color = color * (cos(uv * uv) + cos(uv * uv));
}

void optimized_program(inout vec3 color)
{
    float _cache_0x1 = cos(uv * uv);
    color = color * (_cache_0x1 + _cache_0x1);
}