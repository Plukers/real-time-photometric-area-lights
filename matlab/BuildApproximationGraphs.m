function [] = BuildApproximationGraphs(approximations, errorImage, solidAngle, numSteps, savePath)

%% Setup
imgWidth = size(errorImage, 2) / numSteps;
s = solidAngle(:);

stepLegend = cell(numSteps, 1);
stepSolidAngle = cell(numSteps, 1);
for step = 1:numSteps
    stepSolidAngle(step) = {solidAngle(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth))};
    stepLegend(step) = {num2str(step)};
end

%% Plot per approximation

for a = 1:size(approximations,1)
    
    e = errorImage(:,:,a);
    
    h = figure;
    title(approximations(a));
    hold on
    
    for step = 1:numSteps
        
        stepErrorImg = e(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth));
        seStep = stepErrorImg(:);
        stepSolidAngleImg = stepSolidAngle{step};
        ssaStep = stepSolidAngleImg(:);
        plot(ssaStep, seStep,'.','markersize',0.5);
    end
    legend(stepLegend,'location','northwest');
    legendmarkeradjust(20);
    
    e = e(:);
    p = polyfit(s,e,3);
    x1 = linspace(0,max(max(solidAngle)));
    y1 = polyval(p,x1);
    plot(x1,y1,'LineWidth', 3);
    hold off;
    
    saveas(h,strcat(savePath, approximations{a},'_solid_angle.png'))
    close(h);
end

end