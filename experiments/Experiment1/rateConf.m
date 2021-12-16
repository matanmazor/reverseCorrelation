function [rating]= rateConf(w,pars,color,color_fondo, practice, responseKey)

SetMouse(pars.center(1),pars.center(2),w);
[ex,ey,buttons] = GetMouse(w);

bar_rect = [pars.center(1)-0.625*pars.bar_length,pars.center(2)-0.625*pars.bar_length,...
    pars.center(1)+0.625*pars.bar_length,pars.center(2)+0.625*pars.bar_length];
rating = nan;

while buttons(1)==0 || isnan(rating)
    if strcmp(responseKey,'RightArrow')
        Screen('DrawTextures', w, pars.right_bar, [], [bar_rect])
        rating = max(min(ex-pars.center(1),pars.bar_length/2),0)/(pars.bar_length/2);
        Screen('DrawDots', w, [pars.center(1)+rating*pars.bar_length/2,...
            pars.center(2)], pars.dot_size*2, [0 0 255],[0 0],pars.dot_type);
    elseif strcmp(responseKey,'LeftArrow')
        Screen('DrawTextures', w, pars.left_bar, [], [bar_rect])
        rating = max(min(pars.center(1)-ex,pars.bar_length/2),0)/(pars.bar_length/2);
        Screen('DrawDots', w, [pars.center(1)-rating*pars.bar_length/2,...
            pars.center(2)], pars.dot_size*2, [0 0 255],[0 0],pars.dot_type);
    elseif strcmp(responseKey,'UpArrow')
        Screen('DrawTextures', w, pars.up_bar, [], [bar_rect])
        rating = max(min(pars.center(2)-ey,pars.bar_length/2),0)/(pars.bar_length/2);
        Screen('DrawDots', w, [pars.center(1),...
            pars.center(2)-rating*pars.bar_length/2], pars.dot_size*2, [0 0 255],[0 0],pars.dot_type);
    elseif strcmp(responseKey,'DownArrow')
       Screen('DrawTextures', w, pars.down_bar, [], [bar_rect])
       rating = max(min(ey-pars.center(2),pars.bar_length/2),0)/(pars.bar_length/2);
        Screen('DrawDots', w, [pars.center(1),...
            pars.center(2)+rating*pars.bar_length/2], pars.dot_size*2, [0 0 255],[0 0],pars.dot_type);
    end
    
    vbl=Screen('Flip', w);
    [ex,ey,buttons] = GetMouse(w);
end

end