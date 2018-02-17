%%% Evaluate the Results
function [] = Evaluate(light, approximations, formFactor, solidAngle, numSteps, numGTSamples)

LightPath = strcat('results/', light, '/');

EvalPath = strcat(LightPath, '/evaluation'); 
if exist(EvalPath, 'dir')
    cmd_rmdir(EvalPath);
end
mkdir(EvalPath);

ResultFile = fopen(strcat(EvalPath, '/data.csv'), 'w');
fprintf(ResultFile, 'Approximation;Mean Squared Error; Max Error; Correlation FF; Correlation SA\n');

%% Load Ground Truth

createGTFilePath = @(iter) strcat(LightPath, 'GroundTruth_', int2str(iter), '_', int2str(numGTSamples), '.exr');
GroundTruth = exrread(createGTFilePath(0));
for i = 1:(numSteps - 1)
    GroundTruth = cat(2, GroundTruth, exrread(createGTFilePath(i)));
end
imwrite(ReinhardTMO(GroundTruth, 0.0001), strcat(EvalPath, '/', 'GroundTruth.png'));
GroundTruth = GroundTruth (:,:,1);
%imshow(ReinhardTMO(GroundTruth, 0.0001));


%% Load Approximations

for apmtn = approximations

    createApproxFilePath = @(iter) strcat(LightPath, apmtn{:}, '_', int2str(iter), '.exr');
    Approx = (exrread(createApproxFilePath(0)));
    for i = 1:(numSteps - 1)
        Approx = cat(2, Approx, exrread(createApproxFilePath(i)));
    end
    imwrite(ReinhardTMO(Approx, 0.0001), strcat(EvalPath, '/', apmtn{:}, '.png'));
    Approx = Approx(:,:,1);
    

    %% Compute Error

    errorImage = (Approx - GroundTruth).^2;

    msError = immse(GroundTruth, Approx); %% USE    
    maxError = max(max(abs(errorImage))); %% USE
    corrFF = corr2(errorImage, formFactor); %% USE
    corrSA = corr2(errorImage, solidAngle); %% USE
    errorImg = ones(size(errorImage)) - (errorImage ./ maxError); %% USE
    
    fprintf(ResultFile, strcat(apmtn{:}, ';', num2str(msError), ';', num2str(maxError), ';', num2str(corrFF), ';', num2str(corrSA), '\n'));
    imwrite(errorImg, strcat(EvalPath, '/', apmtn{:}, '_error.png'));


    % numOfElements = size(formFactor, 1) * size(formFactor, 2);
    % 
    % errorFFx = zeros(numOfElements, 1);
    % errorFFy = zeros(numOfElements, 1);
    % for i = 1:numOfElements
    %     errorFFx(i) = formFactor(i);
    %     errorFFy(i) = errorSign(i) * errorImage(i);
    % end
    % [errorFFx, errorFFxI] = sort(errorFFx);
    % errorFFy = errorFFy(errorFFxI);
    % 
    % errorFFp = polyfit(errorFFx,errorFFy,6);
    % 
    % x1 = linspace(0,max(errorFFx) + 0.01);
    % y1 = polyval(errorFFp,x1);
    % figure
    % plot(errorFFx, errorFFy, 'g');
    % hold on
    % plot(x1,y1, 'b');
    % hold off

end

fclose('all');

end