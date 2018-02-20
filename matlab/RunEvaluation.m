%% Config

numSteps = 5
numGTSamples = 50000


%% Setup Reference Data
FormFactor = exrread('results/FormFactor_0.exr');
for i = l:(numSteps - 1)
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

Lights = {'ARC3_60712332_(STD)','INT_60714483_(STD_LEO)','IYON_M_60714889_(STD_LEO)','MIRL_NIV_42925637_(STD_LEO)','PANOS_INF_60813864_(STD_LEO)','PERLUCE_42181512_(STD)-0-90','SLOIN_A_SL_42184612_(STD_LEO)','TECTON_C_42927033_(STD_LEO)','TECTON_C_42927238_(STD_LEO)','TECTON_MIREL_42185315_(STD_LEO)','THOR36L50AS3KA_DC'};
Approximations = {'CenterPointApprox', 'BaumFFApprox', 'MRPApprox', 'StructuredIrrSampling', 'StructuredSampling'};

ErrorReport = fopen('results/data.csv', 'w');
ErrorReportHeader = 'Light';
for approx = Approximations
   ErrorReportHeader = strcat(ErrorReportHeader, ';', approx{:}); 
end
ErrorReportHeader = strcat(ErrorReportHeader, '\n'); 
fprintf(ErrorReport, ErrorReportHeader);

for l = Lights
Evaluate(l{:}, Approximations, FormFactor, SolidAngle, numSteps, numGTSamples, ErrorReport);
end

fclose(ErrorReport);

