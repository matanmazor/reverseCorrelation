function [radio,angle] = get_dot_positions(Nsets, Ndots, min_radio,max_radio,min_distance)

aleatorio = sqrt(rand(Ndots,Nsets)); 
radio = min_radio + aleatorio*(max_radio-min_radio);

angle = zeros(size(radio));
angle(1,:) = rand(1,Nsets)*2*pi;
for i=1:Nsets
    for j=2:Ndots
        distance = -1;
        while distance < min_distance
            aux_angle = rand*2*pi;
            for k=1:j-1
                x2 = (radio(j,i)*cos(aux_angle)-radio(k,i)*cos(angle(k,i)))^2;
                y2 = (radio(j,i)*sin(aux_angle)-radio(k,i)*sin(angle(k,i)))^2;
                distance = sqrt(x2+y2);
            end
        end
        angle(j,i) = aux_angle;
    end
end