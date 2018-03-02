function [] = BuildCompareGraph(approximations, errorImage, solidAngle, savePath)

s = solidAngle(:);

%% Plot all

h = figure;
title('Solid Angle');
hold on

for a = 1:size(approximations,1)
    
    e = errorImage(:,:,a);
    e = e(:);
    
    p = polyfit(s,e,3);
    x1 = linspace(0,max(max(solidAngle)));
    y1 = polyval(p,x1);
    plot(x1,y1,'LineWidth', 1);
    
end
legend(approximations,'location','southoutside');

hold off;

saveas(h,strcat(savePath, 'Solid_Angle.png'))
close(h);

end