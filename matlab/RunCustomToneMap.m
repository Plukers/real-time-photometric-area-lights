for h = heights
    RunCustomToneMapFun(evaluation, h * 10);
    fclose('all');
end

disp(sprintf('Finished'));