function params = loadPars(w, rect, savestr)

params.subj = savestr{1};
params.practice = str2double(savestr{2});

load(fullfile('data','subjects.mat'));
if ismember(params.subj, subjects.keys)
    params.vertical = subjects(params.subj);
else
    error('Participant is not in subjects list');
end

%MM: A while-loop to start next session in line.
if ~params.practice
    num_session=0;
    stopper=0;
    orientations = {'horizontal','vertical'};
    while stopper==0

        num_session = num_session + 1;
        aux_filename = strjoin({params.subj,...
                ['session',num2str(num_session)],orientations{params.vertical+1},...
                '.mat'},'_');
        stopper = isempty(dir(fullfile('data',aux_filename)));
        if ~stopper
            old_params = load(fullfile('data',aux_filename));
        end
    end

    params.num_session = num_session;
    params.filename = aux_filename;
end

%% randomize
if ~params.practice
    subject_num = str2num(params.subj(1:2));
    serial_num = subject_num*100+num_session;
      params.protocolSum = preRNG('protocolFolder.zip',serial_num);
end

params.array_sentido_center = [1 3];
params.array_sentido_side = [2 4];
params.array_speed_deg_sec = 10;

params.SigmaCoh = 0.07;

%use the last coherence value, if not available start with 1.
if params.practice
    params.DetCoh = 0.75;
    params.DisCoh = 0.75;
elseif ~exist('old_params')
    params.DetCoh = 1;
    params.DisCoh = 1;
else
    params.DetCoh = old_params.params.DetCoh(end);
    params.DisCoh = old_params.params.DisCoh(end);
end

%% Visual properties
%background color
params.bg = [0 0 0];
%letter size
params.letter_size = 25;
%dot color
params.dot_color = [255 255 255];
params.target_color = [255 0 0];
params.fix_color = [0 0 255];
params.gaze_color = [0 255 0];
params.displace = 300;
Screen('TextFont',w,'Corbel');


%% Timing
params.fixation_time = 1;

%% Number of trials and blocks
if params.practice
    params.trialsPerBlock = 4;
    params.Nblocks = 2;
else
    params.trialsPerBlock = 100;
    params.Nblocks = 6;
end
params.Nsets = params.trialsPerBlock*params.Nblocks;

distance_from_monitor = 80; % en cm
mon_width = 40.5; %VERIFICAR, ancho del monitor
mon_height = mon_width*6/8; %VERIFICAR
newResolution.width = 1600;
newResolution.height = 1200;
cm_per_px_width  = mon_width/newResolution.width;
cm_per_px_height = mon_height/newResolution.height;
params.deg_per_px_width = cm_per_px_width * atan(1/distance_from_monitor) * 360/(2*pi);
params.deg_per_px_height = cm_per_px_height * atan(1/distance_from_monitor) * 360/(2*pi);

params.dot_diameter_deg = 0.14; %in degrees
params.dot_diameter_px = round(params.dot_diameter_deg/params.deg_per_px_width);

params.dot_density = 3.6; %dots/degree2
params.target_diameter_deg = 3;
params.target_diameter_px = round(params.target_diameter_deg/params.deg_per_px_width);

params.fixation_diameter_deg = 0.2;
params.fixation_diameter_px = round(params.fixation_diameter_deg/params.deg_per_px_width);

params.annulus_diameter = 6.5; %degrees

params.Ndots = 2*round(params.dot_density*params.annulus_diameter.^2/2); %number of dots in annulus

params.waitframes = 1;

params.SecsMovie = 0.7;

%MM: check frames per second
params.fps=Screen('FrameRate',w);
params.ifi=Screen('GetFlipInterval', w);
if params.fps==0
    params.fps=1/params.ifi;
end

params.nframes = params.SecsMovie*params.fps;

%Stupid hack to map between responses and the true value of the parameters.
%The first value in the vector is the value that is mapped to S1 response,
%and the second value to S2 response. Needed in order for 'yes' responses
%to be mapped to right and not left arrow.

params.sentidoResponseVec = [3 1];
if params.vertical
    params.coherenceResponseVec = [0 1];
else
    params.coherenceResponseVec = [1 0];
end

[params.center(1), params.center(2)] = RectCenter(rect);
params.rect = rect;

params.arrow_xpoints = params.target_diameter_px*0.25*[-2 -1 0 1 2 0 -2 ];
params.arrow_ypoints = params.target_diameter_px*0.25*[-1 -1 0 -1 -1 1 -1];

params.downarrow_coords = [params.center(1)+params.arrow_xpoints; ...
                           params.displace+params.center(2)+params.arrow_ypoints]';
 
params.uparrow_coords = [params.center(1)+params.arrow_xpoints; ...
                           -params.displace+params.center(2)-params.arrow_ypoints]';
                       
params.rightarrow_coords = [params.displace+ params.center(1)+params.arrow_ypoints; ...
                                params.center(2)+params.arrow_xpoints]';
                            
params.leftarrow_coords = [-params.displace+ params.center(1)-params.arrow_ypoints; ...
                                params.center(2)+params.arrow_xpoints]';
                            
%parameters for the confidence bar
pars_seguridad.center = params.center;
pars_seguridad.bar_length = 0.8*newResolution.height;
pars_seguridad.dot_type = 1;
pars_seguridad.dot_size = 9;
pars_seguridad.right_bar = Screen('MakeTexture', w, imread(fullfile('textures','right.png')));
pars_seguridad.left_bar = Screen('MakeTexture', w, imread(fullfile('textures','left.png')));
pars_seguridad.up_bar = Screen('MakeTexture', w, imread(fullfile('textures','up.png')));
pars_seguridad.down_bar = Screen('MakeTexture', w, imread(fullfile('textures','down.png')));

params.pars_seguridad = pars_seguridad;

%MM: direction and coherence for every trial
if params.practice == 2
    params.vDirection = ones(params.Nsets,1)+ 2*binornd(1,0.5,params.Nsets,1);
    params.vCoh = binornd(1,0.5,params.Nsets,1);
    params.vTask = [1, 1];
elseif params.practice == 1
    params.vDirection = ones(params.Nsets,1)+ 2*binornd(1,0.5,params.Nsets,1);
    params.vCoh = ones(params.Nsets,1);
    params.vTask = [0,0];
else
    [params.vDirection, params.vCoh, params.vTask] = ...
    get_trials_params(params);
end
end