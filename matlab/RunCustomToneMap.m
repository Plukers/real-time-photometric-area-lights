for h = heights
    RunCustomToneMapFun(evaluation, h);
    fclose('all');
end

disp(sprintf('Finished'));