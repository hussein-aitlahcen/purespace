varying vec2 vTextureCoords;
 
uniform mat4 projection;
uniform vec2 position;
uniform vec2 textureCoords;

void main()
{
     vTextureCoords = textureCoords;
	   gl_Position = projection * vec4(position, 0.0, 1.0);
}