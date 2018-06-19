function RunEvaluationFun()

%% Config

disp(sprintf('Starting Evaluation'));

% General
NumSteps = 5;

EvalPath = 'results/Evaluation/';
if exist(EvalPath, 'dir')
    cmd_rmdir(EvalPath);
end
mkdir(EvalPath);

% Ground Truth
fGTData = fopen('results/GTData.txt', 'r');
NumGTSamples = str2num(fgetl(fGTData));
fclose(fGTData);

% Lights
fLightData = fopen('results/PhotometryData.txt', 'r');
tline = fgetl(fLightData);
Lights = cell(str2num(tline), 1);
i = 1;
tline = fgetl(fLightData);
while ischar(tline)
    Lights(i) = {tline};
    i = i + 1;
    tline = fgetl(fLightData);
end
fclose(fLightData);

% Approximations
fApproxData = fopen('results/ApproximationData.txt', 'r');
tline = fgetl(fApproxData);
i = 1;
while ischar(tline)
    Approximations(i) = {tline};
    i = i + 1;
    tline = fgetl(fApproxData);
end
fclose(fApproxData);
Approximations = Approximations';

clear fGTData fLightData fApproxData tline i

%% Setup Reference Data
FormFactor = exrread('results/FormFactor_0.exr');
for i = 1:(NumSteps - 1)
    FormFactor = cat(2, FormFactor, exrread(strcat('results/FormFactor_', int2str(i), '.exr')));
end
FormFactor = double(FormFactor(:,:,1));

SolidAngle = exrread('results/SolidAngle_0.exr');
for i = 1:(NumSteps - 1)
    SolidAngle = cat(2, SolidAngle, exrread(strcat('results/SolidAngle_', int2str(i), '.exr')));
end
SolidAngle = double(SolidAngle(:,:,1));

SolidAngleVec = SolidAngle(:);

clear i

%% Evaluate


% SolidAngleError = zeros(round(max(max(SolidAngle)) * 1000 + 1), 2, size(Approximations,1));
% SolidAngleScale = 0 : 0.001 : ((size(SolidAngleError, 1) - 1) / 1000);

% ErrorReport = cell(size(Approximations,1) + 1, size(Lights,1) + 1);
% ErrorPerSolidAngle = zeros(1, size(Approximations,1) + 1);

for i = 1:size(Approximations,1)
    ErrorReport(i + 1,1) = Approximations(i);
end


    function [errorReportEntry, errorPerSolidAngle] = EvaluateLight(light)

        disp(sprintf('Evaluating Light: %s', light));

        toneMapScale = 1.0;
        
        LightPath = strcat('results/', light, '/');
        
        evalPath = strcat(EvalPath, light); 
        if exist(evalPath, 'dir')
            cmd_rmdir(evalPath);
        end
        mkdir(evalPath);
        
        errorReportEntry = cell(size(Approximations,1) + 1, 1);
        eri = 1;
        errorReportEntry(eri) = {light};
        eri = eri + 1;
        
        ResultFile = fopen(strcat(evalPath, '/data.csv'), 'w');
        fprintf(ResultFile, 'Approximation;Mean Squared Error; Max Error; Correlation FF; Correlation SA\n');
        
        %% Load Ground Truth
        
        createGTFilePath = @(iter) strcat(LightPath, 'GroundTruth_', int2str(NumGTSamples), '_', int2str(iter), '.exr');
        GroundTruth = exrread(createGTFilePath(0));
        GroundTruthTone = CustomToneMap(GroundTruth, toneMapScale);
        for j = 1:(NumSteps - 1)
            GT = exrread(createGTFilePath(j));
            GroundTruth = cat(2, GroundTruth, GT);
            GroundTruthTone = cat(2, GroundTruthTone, CustomToneMap(GT, toneMapScale));
        end
        disp(sprintf('Writing GroundTruthTone to: %s', strcat(evalPath, '/', 'GroundTruth.png')));
        imwrite(GroundTruthTone, strcat(evalPath, '/', 'GroundTruth.png'));
        GroundTruth = double(GroundTruth (:,:,1));
        
        
        %% Load Approximations
        
        errorImage = zeros(size(GroundTruth,1), size(GroundTruth,2), size(Approximations,1));
        errorSign = zeros(size(GroundTruth,1), size(GroundTruth,2), size(Approximations,1));
        
        
        errorPerSolidAngle = zeros(size(SolidAngleVec, 1), size(Approximations,1) + 1);
        errorPerSolidAngle(:, 1) = SolidAngleVec;
        
        for a = 1:size(Approximations,1)
        
            createApproxFilePath = @(iter) strcat(LightPath, Approximations{a}, '_', int2str(iter), '.exr');
            Approx = (exrread(createApproxFilePath(0)));
            ApproxTone = CustomToneMap(Approx, toneMapScale);
            for j = 1:(NumSteps - 1)
                A = exrread(createApproxFilePath(j));
                Approx = cat(2, Approx, A);
                ApproxTone = cat(2, ApproxTone, CustomToneMap(A, toneMapScale));
            end
            disp(sprintf('Writing ApproxTone to: %s', strcat(evalPath, '/', Approximations{a}, '.png')));
            imwrite(ApproxTone, strcat(evalPath, '/', Approximations{a}, '.png'));
            Approx = double(Approx(:,:,1));
            
        
            %% Compute Error
            ediff = Approx - GroundTruth;
            errorSign(:,:, a) = sign(ediff);
            eimg = abs(ediff);
            errorImage(:,:, a) = eimg;
            
            errorPerSolidAngle(:, a + 1) = eimg(:);
            
            msError = immse(GroundTruth, Approx); %% USE    
            maxError = max(max(eimg)); %% USE
            corrFF = corr2(eimg, FormFactor); %% USE
            corrSA = corr2(eimg, SolidAngle); %% USE
            errorImg = ones(size(eimg)) - (eimg ./ maxError); %% USE
            
            fprintf(ResultFile, strcat(Approximations{a}, ';', num2str(msError), ';', num2str(maxError), ';', num2str(corrFF), ';', num2str(corrSA), '\n'));

            disp(sprintf('Writing errorImg to: %s', strcat(evalPath, '/', Approximations{a}, '_error.png')));
            imwrite(errorImg, strcat(evalPath, '/', Approximations{a}, '_error.png'));
            
            errorReportEntry(eri) = {num2str(msError)};
            eri = eri + 1;
        
        end
        
        fclose(ResultFile);
        
        %% Generate Plots
        
        %PlotPath = strcat(evalPath, '/Plots/');
        %if exist(PlotPath, 'dir')
        %    cmd_rmdir(PlotPath);
        %end
        %mkdir(PlotPath);
        
        %BuildApproximationGraphs(Approximations, errorImage, errorSign, SolidAngle, NumSteps, PlotPath);
        
        %BuildCompareGraph(Approximations, errorImage, SolidAngle, PlotPath);
        
        
        
        
        end


for i = 1:size(Lights,1)
    [errorReportEntry, errorPerSolidAngle] = EvaluateLight(Lights{i});
      
    ErrorReport(:,i + 1) = errorReportEntry;
%     ErrorPerSolidAngle = cat(1, ErrorPerSolidAngle, errorPerSolidAngle);
    disp(sprintf('Evaluated %u of %u Lights', i, size(Lights,1)));
end


fErrorReport = fopen(strcat(EvalPath, 'data.csv'), 'w');
for i = 1:(size(Approximations,1) + 1)
    sep = '';
    for e = ErrorReport(i, 1:end)
        fprintf(fErrorReport, strcat(sep, e{:}));
        sep = ';';
    end
    fprintf(fErrorReport, '\n');
end
fclose(fErrorReport);


%% Plot solid angle error

% h = figure;
% title('Solid Angle');
% hold on
% 
% s = ErrorPerSolidAngle(:, 1);
% 
% for a = 1:size(Approximations,1)
%     
%     e = ErrorPerSolidAngle(:,a + 1);
%     
%     p = polyfit(s,e,3);
%     x1 = linspace(0,max(max(solidAngle)));
%     y1 = polyval(p,x1);
%     plot(x1,y1,'LineWidth', 1);
%     
% end
% legend(Approximations);
% 
% hold off;
% 
% saveas(h,strcat(savePath, 'Solid_Angle.png'))
% close(h);





clear i fErrorReport sep h s

disp(sprintf('Finished'));

end