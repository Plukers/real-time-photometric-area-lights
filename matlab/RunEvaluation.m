%% Config

numSteps = 5
numGTSamples = 6000


%% Setup Reference Data
FormFactor = exrread('results/FormFactor_0.exr');
for i = 1:(numSteps - 1)
    FormFactor = cat(2, FormFactor, exrread(strcat('results/FormFactor_', int2str(i), '.exr')));
end
FormFactor = FormFactor(:,:,1);

SolidAngle = exrread('results/SolidAngle_0.exr');
for i = 1:(numSteps - 1)
    SolidAngle = cat(2, SolidAngle, exrread(strcat('results/SolidAngle_', int2str(i), '.exr')));
end
SolidAngle = SolidAngle(:,:,1);

% xIdx = single(1:size(FormFactor, 2));
% xIdx = repmat(xIdx, size(FormFactor, 1),1);

% yIdx = single(1:size(FormFactor, 1));
% yIdx = yIdx';
% yIdx = repmat(yIdx, 1, size(FormFactor, 2));

% zIdx(1,1,1) = 1;
% zIdx(1,1,2) = 2;
% zIdx(1,1,3) = 3;
% zIdx = repmat(zIdx, size(FormFactor, 1), size(FormFactor, 2), 3);


clear i

%% Evaluate

Lights = {'ARC3_60712332_(STD)'};
Approximations = {'StructuredIrrSampling'};

for l = Lights
Evaluate(l{:}, Approximations, FormFactor, SolidAngle, numSteps, numGTSamples);
end

