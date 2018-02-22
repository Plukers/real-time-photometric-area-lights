%%% Evaluate the Results
function errorReportEntry = Evaluate(light, approximations, formFactor, solidAngle, numSteps, numGTSamples, evalPath, globalSolidAngleError)

toneMapScale = 1.0;

solidAngleError = zeros(round(max(max(solidAngle)) * 1000 + 1), 2, size(approximations,1));
solidAngleScale = 0 : 0.001 : ((size(solidAngleError, 1) - 1) / 1000);


LightPath = strcat('results/', light, '/');

EvalPath = strcat(evalPath, '/', light); 
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
    eimg = abs(Approx - GroundTruth);
    errorImage(:,:, a) = eimg;
    

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



numOfPix = size(solidAngle, 1) * size(solidAngle, 2);

for a = 1:size(approximations,1)
	errorLayer = errorImage(:,:,a);
    
    for saIdx = 1:numOfPix
                        
        saBucket = round(solidAngle(saIdx) * 1000) + 1;
        error = errorLayer(saIdx);
        
        solidAngleError(saBucket, 1, a) = solidAngleError(saBucket, 1, a) + 1;
        solidAngleError(saBucket, 2, a) = solidAngleError(saBucket, 1, a) + error;    

        globalSolidAngleError(saBucket, 1, a) = globalSolidAngleError(saBucket, 1, a) + 1;
        globalSolidAngleError(saBucket, 2, a) = globalSolidAngleError(saBucket, 1, a) + error;     
    end
    
end

for row = 1:size(solidAngleError, 1)
    for a = 1:size(approximations,1)
        solidAngleError(row, 2, a) = solidAngleError(row, 2, a) / solidAngleError(row, 1, a);
    end
end


fclose(ResultFile);

end