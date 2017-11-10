uniform int uFrames;

out vec4 fragColor;

void main()
{
  vec4 A = texture(sTD2DInputs[0], vUV.st);
  int mod = uFrames % 3 - 1;
  fragColor = vec4(A.xyz * vec3(max(0, -mod), 1 - abs(mod), max(0, mod)), A.z);
}