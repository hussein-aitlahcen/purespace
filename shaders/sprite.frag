#version 330 core

in vec2 vTextureCoords;

out vec4 vFragColor;

uniform sampler2D sSpriteTexture;
        
void main()
{    
    vFragColor = texture2D(sSpriteTexture, vTextureCoords);
}  