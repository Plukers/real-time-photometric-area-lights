%% Config

disp('Starting Evaluation');

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

clear i

%% Evaluate


SolidAngleError = zeros(round(max(max(SolidAngle)) * 1000 + 1), 2, size(Approximations,1));
SolidAngleScale = 0 : 0.001 : ((size(SolidAngleError, 1) - 1) / 1000);

ErrorReport = cell(size(Approximations,1) + 1, size(Lights,1) + 1);
ErrorPerSolidAngle = zeros(1, size(Approximations,1) + 1);

for i = 1:size(Approximations,1)
    ErrorReport(i + 1,1) = Approximations(i);
end

for i = 1:size(Lights,1)
    [errorReportEntry, errorPerSolidAngle] = Evaluate(Lights{i}, Approximations, FormFactor, SolidAngle, NumSteps, NumGTSamples, EvalPath);
    ErrorReport(:,i + 1) = errorReportEntry;
%     ErrorPerSolidAngle = cat(1, ErrorPerSolidAngle, errorPerSolidAngle);
    disp(strcat('Evaluated ', num2str(i), ' of ', num2str(size(Lights,1)), ' Lights'));
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

disp('Finished');
