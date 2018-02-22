numOfPix = size(SolidAngle, 1) * size(SolidAngle, 2);

for saIdx = 1:numOfPix
    
    
    if mod(saIdx, 1000) == 0 
       disp(strcat(num2str(round(saIdx / numOfPix * 10000) / 100), '%')); 
    end
    
    saBucket = round(SolidAngle(saIdx) * 1000) + 1;
     
    for a = 1:size(Approximations,1)
%         error = errorImage(:,:,a);
%         error = error(saIdx);
%     
%         solidAngleError(saBucket, 1, a) = solidAngleError(saBucket, 1, a) + 1;
%         solidAngleError(saBucket, 2, a) = solidAngleError(saBucket, 1, a) + error;    
%         
%         globalSolidAngleError(saBucket, 1, a) = globalSolidAngleError(saBucket, 1, a) + 1;
%         globalSolidAngleError(saBucket, 2, a) = globalSolidAngleError(saBucket, 1, a) + error;
    end
end 

disp('fin');