
detection_colors = c('#377eb8', '#e41a1c');
discrimination_colors = c('#4daf4a','#984ea3')
evidence_colors = c('black','#ebac23')

library(ggforce);

plotDiscByEvidence = function(exp, exp_label) {

  exp$discrimination_by_evidence <- exp$trial_df %>%
    filter(detection==0)%>%
    mutate(evidence_0 = round(evidence_0*5)/5,
           evidence_1 = round(evidence_1*5)/5)%>%
    group_by(evidence_0,evidence_1)%>%
    summarise(resp=mean(as.numeric(as.character(response))),
              correct=mean(as.numeric(as.character(correct))),
              n=n())%>%
    filter(n>10);


  exp$discrimination_by_evidence_plot <- exp$discrimination_by_evidence %>%
    ggplot(aes(x=evidence_0,y=evidence_1,fill=resp))+
    geom_tile(size=12)+
    scale_fill_gradient2(low=discrimination_colors[1],mid='white',high=discrimination_colors[2], midpoint=0.5)+
    scale_x_continuous(breaks=seq(-6,12,2))+
    scale_y_continuous(breaks=seq(-6,12,2))+
    labs(x='evidence for S2',y='evidence for S1')+
    geom_circle(aes(x0 = 0, y0 = 1, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 1, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    coord_fixed(ratio=1)+
    theme_classic()+
    theme(legend.position='none',
          plot.title = element_text(size=22),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
    );

  ggsave(paste('./figures/empirical_tiles',exp_label,'discrimination_decision.png',sep='/'),
         width=5,height=6)

}


plotDetByEvidence = function(exp, exp_label) {

  exp$detection_by_evidence <- exp$trial_df %>%
    filter(detection==1)%>%
    mutate(evidence_0 = round(evidence_0*5)/5,
           evidence_1 = round(evidence_1*5)/5)%>%
    group_by(evidence_0,evidence_1)%>%
    summarise(resp=mean(as.numeric(as.character(response))),
              correct=mean(as.numeric(as.character(correct))),
              n=n())%>%
    filter(n>10);


  exp$detection_by_evidence_plot <- exp$detection_by_evidence %>%
    ggplot(aes(x=evidence_0,y=evidence_1,fill=resp))+
    geom_tile(size=12)+
    scale_fill_gradient2(low=detection_colors[2],mid='white',high=detection_colors[1], midpoint=0.5)+
    scale_x_continuous(breaks=seq(-6,12,2))+
    scale_y_continuous(breaks=seq(-6,12,2))+
    labs(x='evidence for S2',y='evidence for S1')+
    geom_circle(aes(x0 = 0, y0 = 1, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 1, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    coord_fixed(ratio=1)+
    theme_classic()+
    theme(legend.position='none',
          plot.title = element_text(size=22),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
    );

  ggsave(paste('./figures/empirical_tiles',exp_label,'detection_decision.png',sep='/'),
         width=5,height=6)

}


plotDiscConfByEvidence = function(exp, exp_label) {

  exp$discrimination_confidence_by_evidence <- exp$trial_df %>%
    filter(detection==0 & correct==1)%>%
    mutate(evidence_0 = round(evidence_0*5)/5,
           evidence_1 = round(evidence_1*5)/5)%>%
    group_by(evidence_0,evidence_1)%>%
    summarise(confidence=mean(conf_bi),
              n=n())%>%
    filter(n>10);


  exp$discrimination_confidence_by_evidence_plot <- exp$discrimination_confidence_by_evidence %>%
    ggplot(aes(x=evidence_0,y=evidence_1,fill=confidence))+
    geom_tile(size=12)+
    scale_fill_gradient2(low=discrimination_colors[1],mid='white',high=discrimination_colors[2], midpoint=0)+
    scale_x_continuous(breaks=seq(-6,12,2))+
    scale_y_continuous(breaks=seq(-6,12,2))+
    labs(x='evidence for S2',y='evidence for S1')+
    geom_circle(aes(x0 = 0, y0 = 1, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 1, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    coord_fixed(ratio=1)+
    theme_classic()+
    theme(legend.position='none',
          plot.title = element_text(size=22),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
    );

  ggsave(paste('./figures/empirical_tiles',exp_label,'discrimination_confidence.png',sep='/'),
         width=5,height=6)

}

plotDetConfByEvidence = function(exp, exp_label) {

  exp$detection_confidence_by_evidence <- exp$trial_df %>%
    filter(detection==1 & correct==1)%>%
    mutate(evidence_0 = round(evidence_0*5)/5,
           evidence_1 = round(evidence_1*5)/5)%>%
    group_by(evidence_0,evidence_1)%>%
    summarise(confidence=mean(conf_bi),
              n=n())%>%
    filter(n>10);


  exp$detection_confidence_by_evidence_plot <- exp$detection_confidence_by_evidence %>%
    ggplot(aes(x=evidence_0,y=evidence_1,fill=confidence))+
    geom_tile(size=12)+
    scale_fill_gradient2(low=detection_colors[2],mid='white',high=detection_colors[1], midpoint=0)+
    scale_x_continuous(breaks=seq(-6,12,2))+
    scale_y_continuous(breaks=seq(-6,12,2))+
    labs(x='evidence for S2',y='evidence for S1')+
    geom_circle(aes(x0 = 0, y0 = 1, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 1, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    coord_fixed(ratio=1)+
    theme_classic()+
    theme(legend.position='none',
          plot.title = element_text(size=22),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
    );

  ggsave(paste('./figures/empirical_tiles',exp_label,'detection_confidence.png',sep='/'),
         width=5,height=6)

}


plotDiscStimByEvidence = function(exp, exp_label) {

  exp$discrimination_stimulus_by_evidence <- exp$trial_df %>%
    filter(detection==0)%>%
    mutate(evidence_0 = round(evidence_0*5)/5,
           evidence_1 = round(evidence_1*5)/5)%>%
    group_by(evidence_0,evidence_1)%>%
    summarise(stim=mean(as.numeric(as.character(stimulus))),
              correct=mean(as.numeric(as.character(correct))),
              n=n())%>%
    filter(n>10);


  exp$discrimination_stimulus_by_evidence_plot <- exp$discrimination_stimulus_by_evidence %>%
    ggplot(aes(x=evidence_0,y=evidence_1,fill=stim))+
    geom_tile(size=12)+
    scale_fill_gradient2(low=discrimination_colors[1],mid='white',high=discrimination_colors[2], midpoint=0.5)+
    scale_x_continuous(breaks=seq(-6,12,2))+
    scale_y_continuous(breaks=seq(-6,12,2))+
    labs(x='evidence for S2',y='evidence for S1')+
    geom_circle(aes(x0 = 0, y0 = 1, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 1, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    coord_fixed(ratio=1)+
    theme_classic()+
    theme(legend.position='none',
          plot.title = element_text(size=22),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
    );

  ggsave(paste('./figures/empirical_tiles',exp_label,'discrimination_stimulus.png',sep='/'),
         width=5,height=6)

}

plotDetSignalByEvidence = function(exp, exp_label) {

  exp$detection_signal_by_evidence <- exp$trial_df %>%
    filter(detection==1)%>%
    mutate(evidence_0 = round(evidence_0*5)/5,
           evidence_1 = round(evidence_1*5)/5)%>%
    group_by(evidence_0,evidence_1)%>%
    summarise(signal=mean(as.numeric(as.character(signal))),
              correct=mean(as.numeric(as.character(correct))),
              n=n())%>%
    filter(n>10);


  exp$detection_signal_by_evidence_plot <- exp$detection_signal_by_evidence %>%
    ggplot(aes(x=evidence_0,y=evidence_1,fill=signal))+
    geom_tile(size=12)+
    scale_fill_gradient2(low=detection_colors[2],mid='white',high=detection_colors[1], midpoint=0.5)+
    scale_x_continuous(breaks=seq(-6,12,2))+
    scale_y_continuous(breaks=seq(-6,12,2))+
    labs(x='evidence for S2',y='evidence for S1')+
    geom_circle(aes(x0 = 0, y0 = 1, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 1, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 1, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 0, r = 1/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_circle(aes(x0 = 0, y0 = 0, r = 2/sqrt(7)), inherit.aes = FALSE, size=1)+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    coord_fixed(ratio=1)+
    theme_classic()+
    theme(legend.position='none',
          plot.title = element_text(size=22),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
    );

  ggsave(paste('./figures/empirical_tiles',exp_label,'detection_signal.png',sep='/'),
         width=5,height=6)

}

plotEmpiricalTiles = function(exp, exp_label) {
  plotDiscStimByEvidence(exp,exp_label);
  plotDetSignalByEvidence(exp,exp_label);
  plotDiscByEvidence(exp,exp_label);
  plotDetByEvidence(exp,exp_label);
  plotDiscConfByEvidence(exp,exp_label);
  plotDetConfByEvidence(exp,exp_label);
}
