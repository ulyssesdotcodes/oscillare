out vec4 fragColor;

void main()
{
  vec2 uv = vUV.st;
  vec4 color = texture2D(sTD2DInputs[0], vUV.st);

  float amt = color.x * 0.001;

  color *= round(sin((uv.x + uv.y) * 6.283 * (1/amt)) * 0.5 + (0.5 * (0.0005 / amt)));

  fragColor = color;
}