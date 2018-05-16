#version 330 core

in vec2 vTextureCoords;

out vec4 FragColor;

uniform sampler2D spriteTexture;
        
void main()
{    
    FragColor = texture2D(spriteTexture, vTextureCoords);
}  