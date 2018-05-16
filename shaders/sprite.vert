#version 330 core

layout (location = 0) in vec4 vertex;

out vec2 vTextureCoords;
 
uniform mat4 mProjection;
uniform mat4 mModelView;

void main()
{
    gl_Position = mProjection * mModelView * vec4(vertex.xy, 0.0, 1.0);
    vTextureCoords = vertex.zw;
}