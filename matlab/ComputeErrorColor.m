function [errorColorR, errorColorG, errorColorB] = ComputeErrorColor(approx, gt, x, y)

white(1,1,1) = 1.0;
white(1,1,2) = 1.0;
white(1,1,3) = 1.0;

upp1 = 20.0;

b1(1,1,1) = 1.0;
b1(1,1,2) = 0.5089;
b1(1,1,3) = 0.34902;

d1(1,1,1) = 0.21177;
d1(1,1,2) = 0.29412;
d1(1,1,3) = 0.69804;


upp2 = 10.0;

b2(1,1,1) = 1.0;
b2(1,1,2) = 0.35686;
b2(1,1,3) = 0.14902;

d2(1,1,1) = 0.07059;
d2(1,1,2) = 0.18039;
d2(1,1,3) = 0.69804;



error = approx - gt;

if error < 0.0 
    % too dark
    
    if abs(error) < upp1
        cTrue = white;
        cFalse = d1;
        low = 0.0;
        up = upp1;
    else
        cTrue = d1;
        cFalse = d2;
        low = upp1;
        up = upp2;
    end
    

else
    % too bright
    
    if abs(error) < upp1
        cTrue = white;
        cFalse = b1;
        low = 0.0;
        up = upp1;
    else
        cTrue = b1;
        cFalse = b2;
        low = upp1;
        up = upp2;
    end
    
end

error = (max(low, min(up, error)) - low) / (up - low);

errorColor = (1.0 - error) * cTrue + error * cFalse;

errorColorR = errorColor(1,1,1);
errorColorG = errorColor(1,1,2);
errorColorB = errorColor(1,1,3);

end