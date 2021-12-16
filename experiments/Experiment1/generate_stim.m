function [vcoh,XYMATRIX,nDotsCoh,NotRandomDots]= generate_stim(params,num_trial)

MeanCoh = params.Coh*params.vCoh(num_trial);
direction = params.vDirection(num_trial);
SigmaCoh = params.SigmaCoh;
SecsMovie = params.SecsMovie;
speed_deg_sec = 10;
deg_per_px_width = params.deg_per_px_width;
fps = params.fps;
annulus_diameter = params.annulus_diameter;
min_distance = 0;
deg_per_px_height = params.deg_per_px_height;
Ndots = params.Ndots
[radio,angle] = get_dot_positions(1, Ndots, 0,annulus_diameter/2,min_distance);
x = radio.*cos(angle)/deg_per_px_width;
y = radio.*sin(angle)/deg_per_px_height;

cont=0;
%speed_deg_sec = 10;%grados por segundo
speed = speed_deg_sec/(deg_per_px_width*fps/2);%en pixels por frame

noiseType = 'gauss';
for num_pantallas=1:ceil(fps*SecsMovie/4) %6 secs. Divido por 4 porque grabo 4 frames por pasada
    for i=1:2
        inds_random = zeros(Ndots,1);
        inds_random((1:Ndots/2) + (i-1)*Ndots/2) = 1;
        %if i==1 && mod(num_pantallas,2)==1 %resampleo cada 4 pasadas, o sea cada 8 frames
        if i==1 %resampleo cada 2 pasadas, o sea cada 4 frames
            if MeanCoh == 0
                coh = 0
            else
                %genero coherencia al azar sampleando una gausseana
                if strcmp(noiseType,'gauss')
                    coh = min(1,MeanCoh + randn*SigmaCoh);
                    coh = max(coh,-1);
                    
                    %pruebo sampleando coherencia de una distribucion uniforme, media
                    %MeanCoh y extremos en [-2*SigmanCoh + s*SigmaCoh];
                elseif strcmp(noiseType,'random')
                    coh = MeanCoh + 2*(rand-0.5)*2*SigmaCoh;
                end
            end
        end %comentar para samplear todos los frames
        
        sampleIndex = 2*(num_pantallas-1)+i;
        vcoh(sampleIndex) = coh;%coherencia en el sample.
        for j=1:2
            cont=cont+1;
            if j==1
                [radio,angle] = get_dot_positions(1, Ndots/2, 0,annulus_diameter/2,min_distance);
                x(inds_random==1) = radio.*cos(angle)/deg_per_px_width;
                y(inds_random==1) = radio.*sin(angle)/deg_per_px_height;
                XYMATRIX(:,:,cont) = [x';y'];
                %size_color(cont) = Ndots;
            else
                %la coherencia determina el porcentaje de puntos que se
                %mueven coherentemente. El resto se mueve una distancia
                %fija en direccion al azar
                not_random = find(inds_random==0);
                not_random = not_random(randperm(length(not_random))); %los reordeno al azar
                
                %npuntos = floor(abs(coh)*Ndots/2);
                npuntos = round(abs(coh)*Ndots/2); %??
                inds_coh = not_random(1:npuntos); %indice de los puntos que se mueven coherentemente
                
                nDotsCoh(sampleIndex) = length(inds_coh);%numero de puntos que se mueven coherentemente
                if direction == 1;
                    x(inds_coh) = x(inds_coh) + sign(coh)*speed;
                    if sign(coh)>0; angle_mov_coh = 0; else angle_mov_coh = pi; end
                end
                if direction == 2;
                    y(inds_coh) = y(inds_coh) + sign(coh)*speed;
                    if sign(coh)>0; angle_mov_coh = 3*pi/2; else angle_mov_coh = pi/2; end
                end %y crece para abajo en psychotoolbox
                if direction == 3;
                    x(inds_coh) = x(inds_coh) - sign(coh)*speed;
                    if sign(coh)>0; angle_mov_coh = pi; else angle_mov_coh = 0; end
                end
                if direction == 4;
                    y(inds_coh) = y(inds_coh) - sign(coh)*speed;
                    if sign(coh)>0; angle_mov_coh = pi/2; else angle_mov_coh = 3*pi/2; end
                end
                
                
                move_random = not_random(npuntos+1:end);%el resto de los puntos, que deben moverse coherentemetne pero random
                angle_mov = rand(length(move_random),1)*2*pi;
                x(move_random) = x(move_random) + speed*cos(angle_mov);
                y(move_random) = y(move_random) + speed*sin(angle_mov);
                
                inds_valid = (x*deg_per_px_width).^2 + (y*deg_per_px_height).^2 <=(annulus_diameter/2).^2;
                
                %reemplazo al azar los que se fueron
                [radio,angle] = get_dot_positions(1, sum(inds_valid==0), 0,annulus_diameter/2,min_distance);
                x(inds_valid==0) = radio.*cos(angle)/deg_per_px_width;
                y(inds_valid==0) = radio.*sin(angle)/deg_per_px_height;
                
                XYMATRIX(:,:,cont) = [x';y'];
                
                %guardo "x", "y", y "angulo" de los puntos que se movieron
                %(NO al azar), ya sea coherentemente o random
                
                auxNotRandomDots = [x(inds_coh)     y(inds_coh)     repmat(angle_mov_coh,length(inds_coh),1);...
                    x(move_random)  y(move_random)  angle_mov];
                
                validNotRandomDots = ismember([inds_coh;move_random],find(inds_valid));
                auxNotRandomDots(validNotRandomDots==0,:) = [];
                
                NotRandomDots{sampleIndex} = auxNotRandomDots;
                
            end
            
            %size_color(cont) = sum(inds_valid);
        end
    end
end



