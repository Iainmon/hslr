#ifdef GL_ES
precision highp float;
out vec4 outputColor;
#endif
uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;
uniform sampler2D u_texture_0;
uniform vec3 u_camera;


vec2 coord(in vec2 p) {
    p = p / u_resolution.xy;
    if (u_resolution.x > u_resolution.y) {
        p.x *= u_resolution.x / u_resolution.y;
        p.x += (u_resolution.y - u_resolution.x) / u_resolution.y / 2.0;
    }else {
        p.y *= u_resolution.y / u_resolution.x;
        p.y += (u_resolution.x - u_resolution.y) / u_resolution.x / 2.0;
    }
    p -= 0.5;
    p *= vec2(-1.0, 1.0);
    return p;
}
#define mx coord(u_mouse)
#define rx 1.0 / min(u_resolution.x, u_resolution.y)

vec2 uv;

void setup();
void program(inout vec3 color);
void main() {
    uv = gl_FragCoord.xy / u_resolution.xy;
    vec3 color = vec3(1.);
    program(color);
    #ifdef GL_ES
        outputColor = vec4(color, 1.);
    #else
        gl_FragColor = vec4(color, 1.);
    #endif
}

vec2 rotateAround(in vec2 origin, vec2 position, float angle) {
    float s = sin(angle);
    float c = cos(angle);
    
    position.x -= origin.x;
    position.y -= origin.y;
    
    float xnew = position.x * c - position.y * s;
    float ynew = position.x * s + position.y * c;
    
    return vec2(xnew + origin.x, ynew + origin.y);
}