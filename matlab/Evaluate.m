%%% Evaluate the Results
function errorReportEntry = Evaluate(light, approximations, formFactor, solidAngle, numSteps, numGTSamples)

LightPath = strcat('results/', light, '/');

EvalPath = strcat(LightPath, '/evaluation'); 
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
GroundTruthTone = ReinhardTMO(GroundTruth, 0.0001);
for i = 1:(numSteps - 1)
    GT = exrread(createGTFilePath(i));
    GroundTruth = cat(2, GroundTruth, GT);
    GroundTruthTone = cat(2, GroundTruthTone, ReinhardTMO(GT, 0.0001));
end
imwrite(ReinhardTMO(GroundTruthTone, 0.0001), strcat(EvalPath, '/', 'GroundTruth.png'));
GroundTruth = double(GroundTruth (:,:,1));


%% Load Approximations

for a = 1:size(approximations,1)

    createApproxFilePath = @(iter) strcat(LightPath, approximations{a}, '_', int2str(iter), '.exr');
    Approx = (exrread(createApproxFilePath(0)));
    ApproxTone = ReinhardTMO(Approx, 0.0001);
    for i = 1:(numSteps - 1)
        A = exrread(createApproxFilePath(i));
        Approx = cat(2, Approx, A);
        ApproxTone = cat(2, ApproxTone, ReinhardTMO(A, 0.0001));
    end
    imwrite(ReinhardTMO(ApproxTone, 0.0001), strcat(EvalPath, '/', approximations{a}, '.png'));
    Approx = double(Approx(:,:,1));
    

    %% Compute Error

    errorImage = abs(Approx - GroundTruth);

    msError = immse(GroundTruth, Approx); %% USE    
    maxError = max(max(errorImage)); %% USE
    corrFF = corr2(errorImage, formFactor); %% USE
    corrSA = corr2(errorImage, solidAngle); %% USE
    errorImg = ones(size(errorImage)) - (errorImage ./ maxError); %% USE
    
    fprintf(ResultFile, strcat(approximations{a}, ';', num2str(msError), ';', num2str(maxError), ';', num2str(corrFF), ';', num2str(corrSA), '\n'));
    imwrite(errorImg, strcat(EvalPath, '/', approximations{a}, '_error.png'));
    
    errorReportEntry(eri) = {num2str(msError)};
    eri = eri + 1;

end


fclose(ResultFile);

end