
DataPath = fullfile('results', evaluation, 'Data');

EvalPath = fullfile('results', evaluation, 'Evaluation');
if exist(EvalPath, 'dir')
    cmd_rmdir(EvalPath);
end
mkdir(EvalPath);

for h = heights
    RunEvaluationFun(evaluation, h * 10, DataPath, EvalPath);
    fclose('all');
end

disp(sprintf('Finished'));