uniform float i_size;
//uniform vec2 i_resolution;

out vec4 fragColor;

vec2 toPolar(vec2 p, vec2 origin) {
  vec2 cuv = p - origin;

  float ca = atan(cuv.x, cuv.y) + radians(90.0);
  float cr = length(cuv);

  return vec2(ca, cr);
}

vec3 hsv2rgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
 
void main(void)
{
  vec2 i_resolution = vec2(1920,1080);
  vec2 uv = (gl_FragCoord.xy / i_resolution.xy - vec2(0.5)) * vec2(i_resolution.x/i_resolution.y, 1.0);
  vec2 cuv = toPolar(uv, vec2(0));

	vec3 color;
	color.x = cuv.x;
	color.y = 1.0;
	color.z = max(0, min(1, 0.5));

  fragColor = vec4(hsv2rgb(color), 1.0);
}