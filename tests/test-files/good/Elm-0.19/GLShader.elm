module AllSyntax.GLShader exposing (vertexShader)


vertexShader : Shader { position : Vec3, coord : Vec3 } { u | view : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform   mat4 view;
varying   vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]
