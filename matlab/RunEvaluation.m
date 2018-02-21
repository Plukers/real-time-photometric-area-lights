%% Config

% General
NumSteps = 5;

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
Approximations = cell(str2num(tline), 1);
i = 1;
tline = fgetl(fApproxData);
while ischar(tline)
    Approximations(i) = {tline};
    i = i + 1;
    tline = fgetl(fApproxData);
end
fclose(fApproxData);

clear fGTData fLightData fApproxData tline i

%% Setup Reference Data
FormFactor = exrread('results/FormFactor_0.exr');
for i = 1:(numSteps - 1)
    FormFactor = cat(2, FormFactor, exrread(strcat('results/FormFactor_', int2str(i), '.exr')));
end
FormFactor = double(FormFactor(:,:,1));

SolidAngle = exrread('results/SolidAngle_0.exr');
for i = 1:(numSteps - 1)
    SolidAngle = cat(2, SolidAngle, exrread(strcat('results/SolidAngle_', int2str(i), '.exr')));
end
SolidAngle = double(SolidAngle(:,:,1));

clear i

%% Evaluate

ErrorReport = cell(size(Approximations,1) + 1, size(Lights,1) + 1);

for i = 1:size(Approximations,1)
    ErrorReport(i + 1,1) = Approximations(i);
end

for i = 1:size(Lights,1)
    ErrorReport(:,i + 1) = Evaluate(Lights{i}, Approximations, FormFactor, SolidAngle, NumSteps, NumGTSamples);
end


% fErrorReport = fopen('results/data.csv', 'w');
% fprintf(fErrorReport, ErrorReport);
% fclose(fErrorReport);

