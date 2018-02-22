function imgOut = CustomToneMap(img, scale)
    
LScaled = img .* scale;
imgOut = LScaled ./ (1.0 + LScaled);

end