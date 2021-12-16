clear all
workspace
version = '2018-05-14';
addpath('..\..\2018\preRNG\Matlab')
% PsychDebugWindowConfiguration()

%{
  An adaptation of Ariel Zylberberg's code, originally used for exp. 1 in
    Zylberberg, A., Bartfeld, P., & Signman, M. (2012).
    The construction of confidence in percpetual decision.
    Frontiers in integrative neuroscience,6, 79.

  Adapted by Matan Mazor, 2018
%}

%% Psychtoolbox

prompt = {'Name: ', 'Practice '};
dlg_title = 'Filename'; % title of the input dialog box
num_lines = 1; % number of input lines
default = {'Xtest','0'}; % default filename
savestr = inputdlg(prompt,dlg_title,num_lines,default);

%set preferences and open screen
Screen('Preference','SkipSyncTests', 1)
screens=Screen('Screens');
screenNumber=max(screens);
doublebuffer=1;

[w, rect] = Screen('OpenWindow', screenNumber, 0,[], 32, doublebuffer+1);

%load parameters
params = loadPars(w, rect, savestr);

KbName('UnifyKeyNames');
AssertOpenGL;
PsychVideoDelayLoop('SetAbortKeys', KbName('Escape'));
HideCursor();
Priority(MaxPriority(w));

% Enable alpha blending with proper blend-function. We need it
% for drawing of smoothed points:
Screen('BlendFunction', w, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

%MM: initialize log vector for confidence ratings
log.confidence = nan(params.Nsets,1);
%MM: initialize decision log
log.resp = zeros(params.Nsets,2);
log.detection = nan(params.Nsets,1);
log.coherence = nan(params.Nsets,1);
log.correct = nan(params.Nsets,1);
log.estimates = [];
compliments = {'Good job', 'Nice work'};

%% Strart the trials
for num_trial = 1:params.Nsets
    
    %MM: allow a break once every 100 trials.
    if mod(num_trial,round(params.trialsPerBlock))==1
        
        if ~params.practice
            save(fullfile('data', params.filename),'params','log');
        end
        
        %detection or not?
        detection = params.vTask(ceil(num_trial/params.trialsPerBlock));
        
        if num_trial>1
            end_of_block_msg = sprintf('%s.\n Out of %d trials, how many did you get right?\n',...
                compliments{randperm(2,1)},params.trialsPerBlock);
            [string,terminatorChar] = GetEchoString(w,end_of_block_msg,0,params.center(2),[255,255,255],[0,0,0]);
            log.estimates = [log.estimates; str2num(string)];
            Screen('DrawText',w,'Whenever you''re ready, press any key to move on to the next block.',20,120,[255 255 255]);
        else
            if params.practice
                Screen('DrawText',w,...
                    'Press any key to start the practice.',...
                    20,120,[255 255 255]);
            else
                Screen('DrawText',w,...
                    sprintf('Press any key to start session number %d.', params.num_session),...
                    20,120,[255 255 255]);
            end
        end
        
        vbl=Screen('Flip', w);
        KbWait(-1)
        
        if detection
            params.Coh = params.DetCoh(end);
            Screen('DrawText',w,'This block will be a detection block. Press any key to begin.',20,20,[255 0 0]);
        else
            params.Coh = params.DisCoh(end);
            Screen('DrawText',w,'This block will be a discrimination block. Press any key to begin.',20,120,[255 255 255]);
        end
        
        vbl=Screen('Flip', w);
        WaitSecs(0.5)
        KbWait(-1)
        WaitSecs(0.5)
        
        %MM: response mapping is rotated when rotate xor when detection (not both).
        if (params.vertical && ~detection) || (~params.vertical && detection)
            params.S1target= params.uparrow_coords;
            params.S2target= params.downarrow_coords;
            params.S1Key = 'UpArrow';
            params.S2Key = 'DownArrow';
        else
            params.S1target= params.leftarrow_coords;
            params.S2target= params.rightarrow_coords;
            params.S1Key = 'LeftArrow';
            params.S2Key = 'RightArrow';
        end
    end
    
    % monitor and update coherence levels
    % if you're in the first two blocks of the first session, reduce
    % coherence every 10 trials for the first 40 trials if performance was
    % above chance

    if ~params.practice && ceil(num_trial/params.trialsPerBlock)<3 && params.num_session==1 && ...
            ismember(mod(num_trial,params.trialsPerBlock), 11:10:41)
        sum(log.correct(num_trial-9:num_trial))
        if sum(log.correct(num_trial-10:num_trial-1))>5
            params.Coh = params.Coh-0.15
        end
        if detection
            params.DetCoh = [params.DetCoh; params.Coh];
        else
            params.DisCoh = [params.DisCoh; params.Coh];
        end
    elseif ~params.practice && mod(num_trial, 20)==1 && mod(num_trial,params.trialsPerBlock)~=1
        current_performance = mean(log.correct(num_trial-20:num_trial-1));
        if current_performance>0.8
            params.Coh = params.Coh-0.03
        elseif current_performance<0.6
            params.Coh = params.Coh+0.03
        end
        if detection
            params.DetCoh = [params.DetCoh; params.Coh];
        else
            params.DisCoh = [params.DisCoh; params.Coh];
        end
    end
    
    %MM: generate the stimulus.
    [coh,xymatrix,nDotsCoh,NotRandomDots] = generate_stim(params, num_trial);
    
    %MM: save to log.
    log.coherence(num_trial) = params.vCoh(num_trial)*params.Coh;
    log.direction(num_trial) = params.vDirection(num_trial);
    log.xymatrix{num_trial} = xymatrix;
    log.coh{num_trial} = coh;
    log.nDotsCoh{num_trial} = nDotsCoh;
    log.NotRandomDots{num_trial} = NotRandomDots;
    log.detection(num_trial) = detection;
    
    %MM: this can be moved to the generate_stim
    if params.vertical== 1
        xymatrix = xymatrix([2 1],:,:);
    end
    
    % MM: fixation
    tini = GetSecs;
    Screen('DrawDots', w, [0 0]', ...
        params.fixation_diameter_px, [255 255 255]*0.4, params.center,1);
    vbl=Screen('Flip', w);%initial flip
    ex = 0; ey = 0;
    while (GetSecs - tini)<params.fixation_time
        [keyIsDown, seconds, keyCode ] = KbCheck;
        if keyCode(KbName('ESCAPE'))
            break;
        end
    end
    
    %MM: present stimulus
    
    %     Screen(w,'FillRect',params.bg);
    vbl=Screen('Flip', w);%initial flip
    
    [tini, tini_task] = deal(GetSecs);
    tresp=0;
    
    %% ANIMATION LOOP
    
    for i = 1:params.nframes
        
        %         Screen(w,'FillRect',params.bg);
        Screen('DrawDots', w, squeeze(xymatrix(:,:,i)),...
            params.dot_diameter_px, params.dot_color', params.center,1);
        
        if (doublebuffer==1)
            vbl=Screen('Flip', w, vbl + (params.waitframes-0.5)*params.ifi);
            VBL(i)=vbl;
        end
        
        %salir con esc
        [keyIsDown, seconds, keyCode ] = KbCheck;
        if keyIsDown
            if keyCode(KbName('ESCAPE'))
                break;
            end
        end
    end
    
    
    %% else, tomo respuesta
    %MM: draw target dots on the screen.
    Screen('FillPoly',w,[125,125,125],params.S1target);
    Screen('FillPoly',w,[125,125,125],params.S2target);
    
    if (doublebuffer==1)
        vbl=Screen('Flip', w, vbl + (params.waitframes-0.5)*params.ifi);
    end
    
    % MM: monitor keyboard for input
    while 1
        [keyIsDown, seconds, keyCode ] = KbCheck;
        if keyIsDown
            if keyCode(KbName('ESCAPE'))
                break;
            end
            if keyCode(KbName(params.S1Key))
                log.resp(num_trial,:) = [GetSecs-tini 1];
                break
                
            elseif keyCode(KbName(params.S2Key))
                log.resp(num_trial,:) = [GetSecs-tini 2];
                break
            end
        end
    end
    
    log.stimTime{num_trial} = VBL;
    if keyCode(KbName('ESCAPE'))
        break;
    end
    
    % MM: check is the response was accurate or not
    if detection
        if sign(params.coherenceResponseVec(log.resp(num_trial,2)))==...
                sign(params.vCoh(num_trial))
            log.correct(num_trial) = 1;
        else
            log.correct(num_trial) = 0;
        end
    else
        if params.sentidoResponseVec(log.resp(num_trial,2)) == params.vDirection(num_trial)
            log.correct(num_trial) = 1;
        else
            log.correct(num_trial) = 0;
        end
    end
    %MM: end of decision phase
    Screen(w,'FillRect',params.bg);
    
    %MM: draw target dots on the screen.
    Screen('FillPoly',w,[125,125,125]+130*(log.resp(num_trial,2)==1),params.S1target);
    Screen('FillPoly',w,[125,125,125]+130*(log.resp(num_trial,2)==2),params.S2target);
    Screen('Flip', w);
    WaitSecs(0.3);
    
    practice =  contains(params.subj,'practice');
    if log.resp(num_trial,2)==1
        responseKey = params.S1Key;
    elseif log.resp(num_trial,2)==2
        responseKey = params.S2Key;
    end
    
    %% CONFIDENCE JUDGMENT
    log.confidence(num_trial) = rateConf(w,params.pars_seguridad,...
        params.dot_color, params.bg, practice, responseKey);
    
    %MM: end of trial
    Screen(w,'FillRect',params.bg);
    Screen('Flip', w);
    WaitSecs(0.1);
end

%% Thanks for your participation
if num_trial == params.Nsets %don't show for 'esc'
    end_of_block_msg = sprintf('%s.\n Out of %d trials, how many did you get right?\n',...
        compliments{randperm(2,1)},params.trialsPerBlock);
    [string,terminatorChar] = GetEchoString(w,end_of_block_msg,0,params.center(2),[255,255,255],[0,0,0]);
    log.estimates = [log.estimates; str2num(string)];
    Screen('DrawText',w,'Whenever you''re ready, press any key to move on to the next block.',20,120,[255 255 255]);
    
    Screen(w,'FillRect',params.bg);
    Screen('DrawText',w,'End of session, thanks for your participation!',...
        params.center(1)-100,params.center(2),[255 255 255]);
    Screen('Flip', w);%initial flip
    WaitSecs(3);
end

%% MM: write to log
%MM: experimento is the log variable that includes all experiment
%parameters and results.
if ~params.practice
    
    log.date = date;
    log.filename = params.filename;
    log.version = version;
    save(fullfile('data', params.filename),'params','log');
    
end

%% close
Priority(0);
ShowCursor
Screen('CloseAll');
