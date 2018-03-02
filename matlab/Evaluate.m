%%% Evaluate the Results
function [errorReportEntry, errorPerSolidAngle] = Evaluate(light, approximations, formFactor, solidAngle, numSteps, numGTSamples, evalPath)

toneMapScale = 1.0;

LightPath = strcat('results/', light, '/');

EvalPath = strcat(evalPath, light); 
if exist(EvalPath, 'dir')
    cmd_rmdir(EvalPath);
end
mkdir(EvalPath);

errorReportEntry = cell(size(approximations,1) + 1, 1);
eri = 1;
errorReportEntry(eri) = {light};
eri = eri + 1;

ResultFile = fopen(strcat(EvalPath, '/data.csv'), 'w');
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
imwrite(GroundTruthTone, strcat(EvalPath, '/', 'GroundTruth.png'));
GroundTruth = double(GroundTruth (:,:,1));


%% Load Approximations

errorImage = zeros(size(GroundTruth,1), size(GroundTruth,2), size(approximations,1));
errorSign = zeros(size(GroundTruth,1), size(GroundTruth,2), size(approximations,1));

s = solidAngle(:);

errorPerSolidAngle = zeros(size(s, 1), size(approximations,1) + 1);
errorPerSolidAngle(:, 1) = s;

for a = 1:size(approximations,1)

    createApproxFilePath = @(iter) strcat(LightPath, approximations{a}, '_', int2str(iter), '.exr');
    Approx = (exrread(createApproxFilePath(0)));
    ApproxTone = CustomToneMap(Approx, toneMapScale);
    for i = 1:(numSteps - 1)
        A = exrread(createApproxFilePath(i));
        Approx = cat(2, Approx, A);
        ApproxTone = cat(2, ApproxTone, CustomToneMap(A, toneMapScale));
    end
    imwrite(ApproxTone, strcat(EvalPath, '/', approximations{a}, '.png'));
    Approx = double(Approx(:,:,1));
    

    %% Compute Error
    ediff = Approx - GroundTruth;
    errorSign(:,:, a) = sign(ediff);
    eimg = abs(ediff);
    errorImage(:,:, a) = eimg;
    
    errorPerSolidAngle(:, a + 1) = eimg(:);
    
    msError = immse(GroundTruth, Approx); %% USE    
    maxError = max(max(eimg)); %% USE
    corrFF = corr2(eimg, formFactor); %% USE
    corrSA = corr2(eimg, solidAngle); %% USE
    errorImg = ones(size(eimg)) - (eimg ./ maxError); %% USE
    
    fprintf(ResultFile, strcat(approximations{a}, ';', num2str(msError), ';', num2str(maxError), ';', num2str(corrFF), ';', num2str(corrSA), '\n'));
    imwrite(errorImg, strcat(EvalPath, '/', approximations{a}, '_error.png'));
    
    errorReportEntry(eri) = {num2str(msError)};
    eri = eri + 1;

end

fclose(ResultFile);

%% Generate Plots

PlotPath = strcat(EvalPath, '/Plots/');
if exist(PlotPath, 'dir')
    cmd_rmdir(PlotPath);
end
mkdir(PlotPath);

BuildApproximationGraphs(approximations, errorImage, errorSign, solidAngle, numSteps, PlotPath);

BuildCompareGraph(approximations, errorImage, solidAngle, PlotPath);




end