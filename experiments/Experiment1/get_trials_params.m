function [vDirection,vCoh,vTask] = get_trials_params(params)

Nsets = params.Nsets;
Nblocks = params.Nblocks;

% randomize blocks for detection/discrimination. Always interleaved, but
% first can be detection or discrimination
% 0 is discrimination, 1 detection
vTask = reshape([ones(Nblocks/2,1) zeros(Nblocks/2,1)]',Nblocks,1);
if binornd(1,0.5)
    vTask = 1-vTask;
end
%initialize
[vDirection, vCoh] = deal([]);

for i=1:length(vTask)
    
    detection = vTask(i);
    block_array = [ones(Nsets/(Nblocks*4),1) ones(Nsets/(Nblocks*4),1); ...
        zeros(Nsets/(Nblocks*4),1) ones(Nsets/(Nblocks*4),1);...
        ones(Nsets/(Nblocks*4),1) 3*ones(Nsets/(Nblocks*4),1); ...
        zeros(Nsets/(Nblocks*4),1) 3*ones(Nsets/(Nblocks*4),1)];
    if ~detection
        block_array(:,1)=1;
    end
    
    %% randomize
    block_array = block_array(randperm(Nsets/Nblocks),:);
    vCoh = [vCoh; block_array(:,1)];
    vDirection = [vDirection; block_array(:,2)];
end
end
