function [] = BuildApproximationGraphs(approximations, errorImage, errorSign, solidAngle, numSteps, savePath)

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
    for step = 1:numSteps
        
        stepErrorImg = e(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth));
        seStep = stepErrorImg(:);
        stepSolidAngleImg = stepSolidAngle{step};
        ssaStep = stepSolidAngleImg(:);
        plot(ssaStep, seStep,'.','markersize',0.5);
        hold on
    end
    grid on
    title(approximations(a));
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

for a = 1:size(approximations,1)
    
    e = errorSign(:,:,a) .* errorImage(:,:,a);
    
    h = figure;
    for step = 1:numSteps
        
        stepErrorImg = e(1:end, (step * imgWidth - imgWidth + 1):(step * imgWidth));
        seStep = stepErrorImg(:);
        stepSolidAngleImg = stepSolidAngle{step};
        ssaStep = stepSolidAngleImg(:);
        plot(ssaStep, seStep,'.','markersize',0.5);
        hold on
    end
    grid on
    title(approximations(a));
    legend(stepLegend,'location','northwest');
    legendmarkeradjust(20);
    
    e = e(:);
    
    ep = e(e(:) >= 0, :);
    sp = s(e(:) >= 0, :);
    p = polyfit(sp,ep,3);
    x1 = linspace(0,max(sp));
    y1 = polyval(p,x1);
    plot(x1,y1,'LineWidth', 3);
    
    en = e(e(:) < 0, :);
    sn = s(e(:) < 0, :);
    p = polyfit(sn,en,3);
    x1 = linspace(0,max(sn));
    y1 = polyval(p,x1);
    plot(x1,y1,'LineWidth', 3)
    
    hold off;
    
    saveas(h,strcat(savePath, approximations{a},'_solid_angle_signed.png'))
    close(h);
end

end