%%% Evaluate the Results
light = 'ARC3_60712332_(STD)';
approximations = Approximations;
formFactor = FormFactor;
solidAngle = SolidAngle;
numSteps = 5;
numGTSamples = 16;
evalPath = EvalPath;
globalSolidAngleError = SolidAngleError;

toneMapScale = 1.0;

solidAngleError = zeros(round(max(max(solidAngle)) * 1000 + 1), 2, size(approximations,1));
solidAngleScale = 0 : 0.001 : ((size(solidAngleError, 1) - 1) / 1000);


LightPath = strcat('results/', light, '/');

evalPath = strcat(evalPath, '/', light); 
if exist(evalPath, 'dir')
    cmd_rmdir(evalPath);
end
mkdir(evalPath);

errorReportEntry = cell(size(approximations,1) + 1, 1);
eri = 1;
errorReportEntry(eri) = {light};
eri = eri + 1;

ResultFile = fopen(strcat(evalPath, '/data.csv'), 'w');
fprintf(ResultFile, 'Approximation;Mean Squared Error; Max Error; Correlation FF; Correlation SA\n');

%% Load Ground Truth

createGTFilePath = @(iter) strcat(LightPath, 'GroundTruth_', int2str(numGTSamples), '_', int2str(iter), '.exr');
GroundTruth = exrread(createGTFilePath(0));
GroundTruthTone = CustomToneMap(GroundTruth, toneMapScale);
for i = 1:(numSteps - 1)
    GT = exrread(createGTFilePath(i));
    GroundTruth = cat(2, GroundTruth, GT);
    GroundTruthTone = cat(2, GroundTruthTone, CustomToneMap(GT, toneMapScale));
end
imwrite(GroundTruthTone, strcat(evalPath, '/', 'GroundTruth.png'));
GroundTruth = double(GroundTruth (:,:,1));


%% Load Approximations

errorImage = zeros(size(GroundTruth,1), size(GroundTruth,2), size(approximations,1));

for a = 1:size(approximations,1)

    createApproxFilePath = @(iter) strcat(LightPath, approximations{a}, '_', int2str(iter), '.exr');
    Approx = (exrread(createApproxFilePath(0)));
    ApproxTone = CustomToneMap(Approx, toneMapScale);
    for i = 1:(numSteps - 1)
        A = exrread(createApproxFilePath(i));
        Approx = cat(2, Approx, A);
        ApproxTone = cat(2, ApproxTone, CustomToneMap(A, toneMapScale));
    end
    imwrite(ApproxTone, strcat(evalPath, '/', approximations{a}, '.png'));
    Approx = double(Approx(:,:,1));
    

    %% Compute Error
    eimg = abs(Approx - GroundTruth);
    errorImage(:,:, a) = eimg;
    

    msError = immse(GroundTruth, Approx); %% USE    
    maxError = max(max(eimg)); %% USE
    corrFF = corr2(eimg, formFactor); %% USE
    corrSA = corr2(eimg, solidAngle); %% USE
    errorImg = ones(size(eimg)) - (eimg ./ maxError); %% USE
    
    fprintf(ResultFile, strcat(approximations{a}, ';', num2str(msError), ';', num2str(maxError), ';', num2str(corrFF), ';', num2str(corrSA), '\n'));
    imwrite(errorImg, strcat(evalPath, '/', approximations{a}, '_error.png'));
    
    errorReportEntry(eri) = {num2str(msError)};
    eri = eri + 1;

end

fclose(ResultFile);

%% Collect Data

for saIdx = 1:(size(solidAngle, 1) * size(solidAngle, 2))

    saBucket = round(solidAngle(saIdx) * 1000) + 1;
        
    [row, col] = ind2sub(size(solidAngle), saIdx);
    error = errorImage(row, col, :);

    solidAngleError(saBucket, 1, :) = solidAngleError(saBucket, 1, :) + 1;
    solidAngleError(saBucket, 2, :) = solidAngleError(saBucket, 2, :) + error;     
end
    

for row = 1:size(solidAngleError, 1)
    globalSolidAngleError(row, 1, :) = globalSolidAngleError(row, 1, :) + solidAngleError(row, 1, :);
    globalSolidAngleError(row, 2, :) = globalSolidAngleError(row, 2, :) + solidAngleError(row, 2, :);    
    
    solidAngleError(row, 2, :) = solidAngleError(row, 2, :) ./ solidAngleError(row, 1, :);
end
solidAngleError(isnan(solidAngleError))=0;

%% Plot per approximation

for a = 1:size(approximations,1)
    
    e = solidAngleError(:, 2, a);
    p = polyfit(solidAngleScale,e,4);
    
    x1 = linspace(0,max(solidAngleScale));
    y1 = polyval(p,x1);
    
    h = figure;
    title(approximations(a));
    hold on;
    plot(solidAngleScale, e,'.');
    plot(x1,y1,'LineWidth', 3);
    hold off;
    
    saveas(h,strcat(evalPath, '/', approximations{a},'_solid_angle.png'))
    close(h);
end


%% Collective Plot

h = figure;
title('Solid Angle');
hold on;

for a = 1:size(approximations,1)
    
    e = solidAngleError(:, 2, a);
    p = polyfit(solidAngleScale,e,4);
    
    x1 = linspace(0,max(solidAngleScale));
    y1 = polyval(p,x1);
    
    plot(x1,y1,'LineWidth', 1);
end

legend(approximations);
hold off;

saveas(h,strcat(evalPath, '/', 'Solid_Angle.png'))
close(h);