function RunEvaluationFun(evaluation)

%% Config

disp(sprintf('Starting Evaluation'));

% General
NumSteps = 5;

DataPath = fullfile('results', evaluation, 'Data');

EvalPath = fullfile('results', evaluation, 'Evaluation');
if exist(EvalPath, 'dir')
    cmd_rmdir(EvalPath);
end
mkdir(EvalPath);

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
fApproxData = fopen(fullfile(DataPath, 'ApproximationData.txt'), 'r');
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


for i = 1:size(Approximations,1)
    ErrorReport(i + 1,1) = Approximations(i);
end

    function [] = BuildCompareGraphForLight(errorImage, savePath)
       
        %% Plot all
        
        h = figure;
        title('Solid Angle');
        hold on
        
        for a = 1:size(Approximations,1)
            
            e = errorImage(:,:,a);
            e = e(:);
            
            p = polyfit(SolidAngleVec,e,3);
            x1 = linspace(0,max(max(SolidAngle)));
            y1 = polyval(p,x1);
            plot(x1,y1,'LineWidth', 1);
            
        end
        legend(Approximations,'location','southoutside');
        
        hold off;
        
        saveas(h,strcat(savePath, 'Solid_Angle.png'))
        close(h);
        
        end


    function [] = BuildApproximationGraphsForLight(errorImage, errorSign, savePath)

        %% Setup
        imgWidth = size(errorImage, 2) / NumSteps;
        
        stepLegend = cell(NumSteps, 1);
        stepSolidAngle = cell(NumSteps, 1);
        for step = 1:NumSteps
            stepSolidAngle(step) = {SolidAngle(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth))};
            stepLegend(step) = {num2str(step)};
        end
        
        %% Plot per approximation
        
        for a = 1:size(Approximations,1)
            
            e = errorImage(:,:,a);
            
            h = figure;
            for step = 1:NumSteps
                
                stepErrorImg = e(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth));
                seStep = stepErrorImg(:);
                stepSolidAngleImg = stepSolidAngle{step};
                ssaStep = stepSolidAngleImg(:);
                plot(ssaStep, seStep,'.','markersize',0.5);
                hold on
            end
            grid on
            title(Approximations(a));
            legend(stepLegend,'location','northwest');
            %legendmarkeradjust(20);
            
            e = e(:);
            p = polyfit(SolidAngleVec,e,3);
            x1 = linspace(0,max(max(SolidAngle)));
            y1 = polyval(p,x1);
            plot(x1,y1,'LineWidth', 3);
            hold off;
            
            saveas(h,strcat(savePath, Approximations{a},'_solid_angle.png'))
            close(h);
        end
        
        for a = 1:size(Approximations,1)
            
            e = errorSign(:,:,a) .* errorImage(:,:,a);
            
            h = figure;
            for step = 1:NumSteps
                
                stepErrorImg = e(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth));
                seStep = stepErrorImg(:);
                stepSolidAngleImg = stepSolidAngle{step};
                ssaStep = stepSolidAngleImg(:);
                plot(ssaStep, seStep,'.','markersize',0.5);
                hold on
            end
            grid on
            title(Approximations(a));
            legend(stepLegend,'location','northwest');
            %legendmarkeradjust(20);
            
            e = e(:);
            
            ep = e(e(:) >= 0, :);
            sp = SolidAngleVec(e(:) >= 0, :);
            p = polyfit(sp,ep,3);
            x1 = linspace(0,max(sp));
            y1 = polyval(p,x1);
            plot(x1,y1,'LineWidth', 3);
            
            en = e(e(:) < 0, :);
            sn = SolidAngleVec(e(:) < 0, :);
            p = polyfit(sn,en,3);
            x1 = linspace(0,max(sn));
            y1 = polyval(p,x1);
            plot(x1,y1,'LineWidth', 3)
            
            hold off;
            
            saveas(h,strcat(savePath, Approximations{a},'_solid_angle_signed.png'))
            close(h);
        end
        
        end


    function [errorReportEntry, errorPerSolidAngle] = EvaluateLight(light)

        disp(sprintf('Evaluating Light: %s', light));

        toneMapScale = 1.0;
                
        evalPath = fullfile(EvalPath, light)
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
        
        createGTFilePath = @(iter) fullfile('results', light, strcat('GroundTruth', '_', int2str(iter), '.exr'));
        GroundTruth = exrread(createGTFilePath(0));
        GroundTruthTone = CustomToneMap(GroundTruth, toneMapScale);
        for j = 1:(NumSteps - 1)
            GT = exrread(createGTFilePath(j));
            GroundTruth = cat(2, GroundTruth, GT);
            GroundTruthTone = cat(2, GroundTruthTone, CustomToneMap(GT, toneMapScale));
        end
        % disp(sprintf('Writing GroundTruthTone to: %s', strcat(evalPath, '/', 'GroundTruth.png')));
        imwrite(GroundTruthTone, strcat(evalPath, '/', 'GroundTruth.png'));
        GroundTruth = double(GroundTruth (:,:,1));
        
        
        %% Load Approximations
        
        errorImage = zeros(size(GroundTruth,1), size(GroundTruth,2), size(Approximations,1));
        errorSign = zeros(size(GroundTruth,1), size(GroundTruth,2), size(Approximations,1));
        
        
        errorPerSolidAngle = zeros(size(SolidAngleVec, 1), size(Approximations,1) + 1);
        errorPerSolidAngle(:, 1) = SolidAngleVec;
        
        for a = 1:size(Approximations,1)
        
            createApproxFilePath = @(iter)  fullfile(DataPath, light, strcat(Approximations{a}, '_', int2str(iter), '.exr'));
            Approx = (exrread(createApproxFilePath(0)));
            ApproxTone = CustomToneMap(Approx, toneMapScale);
            for j = 1:(NumSteps - 1)
                A = exrread(createApproxFilePath(j));
                Approx = cat(2, Approx, A);
                ApproxTone = cat(2, ApproxTone, CustomToneMap(A, toneMapScale));
            end
            % disp(sprintf('Writing ApproxTone to: %s', strcat(evalPath, '/', Approximations{a}, '.png')));
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

            % disp(sprintf('Writing errorImg to: %s', strcat(evalPath, '/', Approximations{a}, '_error.png')));
            imwrite(errorImg, strcat(evalPath, '/', Approximations{a}, '_error.png'));
            
            errorReportEntry(eri) = {num2str(msError)};
            eri = eri + 1;
        
        end
        
        fclose(ResultFile);
        
        %% Generate Plots
        
        PlotPath = strcat(evalPath, '/Plots/');
        if exist(PlotPath, 'dir')
            cmd_rmdir(PlotPath);
        end
        mkdir(PlotPath);
        
        BuildApproximationGraphsForLight( errorImage, errorSign, PlotPath);
        
        BuildCompareGraphForLight(errorImage, PlotPath);
        
        
        
        
        end


for i = 1:size(Lights,1)
    [errorReportEntry, errorPerSolidAngle] = EvaluateLight(Lights{i});
      
    ErrorReport(:,i + 1) = errorReportEntry;
%     ErrorPerSolidAngle = cat(1, ErrorPerSolidAngle, errorPerSolidAngle);
    disp(sprintf('Evaluated %u of %u Lights', i, size(Lights,1)));
end


fErrorReport = fopen(fullfile(EvalPath, 'data.csv'), 'w');
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