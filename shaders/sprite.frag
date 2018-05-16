varying vec2 vTextureCoords;

uniform sampler2D spriteTexture;
        
void main()
{    
    gl_FragColor = texture2D(spriteTexture, vTextureCoords);
}  