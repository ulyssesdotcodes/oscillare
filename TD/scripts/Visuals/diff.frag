out vec4 fragColor;

void main()
{
  vec4 A = texture(sTD2DInputs[0], vUV.st);
  vec4 B = texture(sTD2DInputs[1], vUV.st);
  float diff = A == B ? 0 : 1;
  fragColor = vec3(A.xyz, diff * A.z);
}