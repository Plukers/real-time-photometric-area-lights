function RunCustomToneMapFun(evaluation)

%% Config

disp(sprintf('Starting Evaluation'));

% General
NumSteps = 5;
toneMapScale = 10.0;

DataPath = fullfile('results', evaluation, 'Data');

SavePath = fullfile('results', evaluation, 'Tonemap');
if exist(SavePath, 'dir')
    cmd_rmdir(SavePath);
end
mkdir(SavePath);

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

% Tonemap

for i = 1:size(Lights,1)
    light = Lights{i};
    
    savePath = fullfile(SavePath, light)
    if exist(savePath, 'dir')
        cmd_rmdir(savePath);
    end
    mkdir(savePath);

    for a = 1:size(Approximations,1)

        createApproxFilePath = @(iter)  fullfile(DataPath, light, strcat(Approximations{a}, '_', int2str(iter), '.exr'));
        createApproxSavePath = @(iter)  fullfile(savePath, strcat(Approximations{a}, '_', int2str(iter), '.png'));

        for j = 0:(NumSteps - 1)
            Approx = (exrread(createApproxFilePath(j)));
            ApproxTone = CustomToneMap(Approx, toneMapScale);        
            imwrite(ApproxTone, createApproxSavePath(j));
        end

    end

end

end
