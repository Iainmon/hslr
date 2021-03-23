#define PI 3.14159265358979323846

float rand(in vec2 c) {
    return fract(sin(dot(c.xy , vec2(12.9898, 78.233))) * 43758.5453);
}

float noise(in vec2 p) {
    vec2 ip = floor(p);
    vec2 u = fract(p);
    u = u * u * (3.0 - 2.0 * u);
    
    float res =
    mix(mix(rand(ip), rand(ip + vec2(1.0, 0.0)), u.x),
    mix(rand(ip + vec2(0.0, 1.0)), rand(ip + vec2(1.0, 1.0)), u.x), u.y);
    return res * res;
}

vec2 fade(in vec2 t) {return t * t*t * (t * (t * 6.0 - 15.0) + 10.0); }

vec4 permute(in vec4 x) {
    return mod(((x * 34.0) + 1.0) * x, 289.0);
}

float perlin(in vec2 P) {
    vec4 Pi = floor(P.xyxy) + vec4(0.0, 0.0, 1.0, 1.0);
    vec4 Pf = fract(P.xyxy) - vec4(0.0, 0.0, 1.0, 1.0);
    Pi = mod(Pi, 289.0); // To avoid truncation effects in permutation
    vec4 ix = Pi.xzxz;
    vec4 iy = Pi.yyww;
    vec4 fx = Pf.xzxz;
    vec4 fy = Pf.yyww;
    vec4 i = permute(permute(ix) + iy);
    vec4 gx = 2.0 * fract(i * 0.0243902439) - 1.0; // 1/41 = 0.024...
    vec4 gy = abs(gx) - 0.5;
    vec4 tx = floor(gx + 0.5);
    gx = gx - tx;
    vec2 g00 = vec2(gx.x, gy.x);
    vec2 g10 = vec2(gx.y, gy.y);
    vec2 g01 = vec2(gx.z, gy.z);
    vec2 g11 = vec2(gx.w, gy.w);
    vec4 norm = 1.79284291400159 - 0.85373472095314 *
    vec4(dot(g00, g00), dot(g01, g01), dot(g10, g10), dot(g11, g11));
    g00 *= norm.x;
    g01 *= norm.y;
    g10 *= norm.z;
    g11 *= norm.w;
    float n00 = dot(g00, vec2(fx.x, fy.x));
    float n10 = dot(g10, vec2(fx.y, fy.y));
    float n01 = dot(g01, vec2(fx.z, fy.z));
    float n11 = dot(g11, vec2(fx.w, fy.w));
    vec2 fade_xy = fade(Pf.xy);
    vec2 n_x = mix(vec2(n00, n01), vec2(n10, n11), fade_xy.x);
    float n_xy = mix(n_x.x, n_x.y, fade_xy.y);
    return 2.3 * n_xy;
}

// Optimise this
vec2 fluidWarpSlope(in vec2 p) {
    float h = 0.1;
    float mag = 0.01;

    float u = perlin(p);
    float dx = perlin(p + vec2(h, 0.0)) - u;
    float dy = perlin(p + vec2(0.0, h)) - u;
    vec2 v = vec2(dy, -dx);
    v *= mag * 10.;
    return v;
}

vec2 fluidWarp(in vec2 st) {
    vec2 p = st;
    //float t = 0.5 * sin(u_time) + 0.5;
    int iterations = 100;
    
    for(int i = 0; i < iterations; i ++ ) {
        p += fluidWarpSlope(p);
    }
    
    return p;
}

#define NUM_OCTAVES 6

float fbm(in vec2 _st) {
    float v = 0.0;
    float a = 0.5;
    vec2 shift = vec2(100.0);
    // Rotate to reduce axial bias
    mat2 rot = mat2(cos(0.5), sin(0.5), - sin(0.5), cos(0.50));
    for(int i = 0; i < NUM_OCTAVES; ++ i) {
        v += a * noise(_st);
        _st = rot * _st * 2.0 + shift;
        a *= 0.5;
    }
    return v;
}

float tmap(float scale) {
    return .5 * sin(u_time * scale) + .5;
}
float slowTime(float speed) {
    return u_time * (1. / speed);
}