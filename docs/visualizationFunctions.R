detection_colors = c('#377eb8', '#e41a1c');
discrimination_colors = c('#4daf4a','#984ea3')
evidence_colors = c('#ebac23','black')

plotrcROC = function(df, labels, c, title, AUC, alpha=1) {
  #############################

  conf_discrete_counts <- df %>%
    mutate(subj_id=factor(subj_id),
           conf_discrete = conf_discrete%>%
             fct_rev()) %>%
    group_by(subj_id, response, correct, conf_discrete, .drop=FALSE) %>%
    tally() %>%
    spread(correct, n, sep='') %>%
    arrange(conf_discrete, by_group=TRUE) %>%
    group_by(subj_id, response)%>%
    mutate(cs_correct=cumsum(correct1)/sum(correct1),
           cs_incorrect=cumsum(correct0)/sum(correct0))

  conf_discrete_counts_group <- conf_discrete_counts %>%
    group_by(response, conf_discrete)%>%
    summarise(conf_incorrect = mean(cs_incorrect, na.rm=TRUE),
              conf_correct = mean(cs_correct, na.rm=TRUE),
              conf_incorrect_sem = se(cs_incorrect, na.rm=TRUE),
              conf_correct_sem  = se(cs_correct, na.rm=TRUE));


  rcROC <- ggplot(data=conf_discrete_counts_group %>%
                    mutate(response=ifelse(response==1,labels[1],labels[2]) %>%
                             factor(levels=labels)),
                  aes(x=conf_incorrect, y=conf_correct, color=response)) +
    geom_line(size=1.3) +
    geom_point(aes(shape = response))+
    geom_errorbar(aes(ymin = conf_correct-conf_correct_sem,ymax = conf_correct+conf_correct_sem)) +
    geom_errorbar(aes(xmin = conf_incorrect-conf_incorrect_sem,xmax = conf_incorrect+conf_incorrect_sem)) +
    geom_abline(slope=1)+
    theme_bw() + coord_fixed() +
    labs(x='p(conf | incorrect)', y='p(conf | correct)', title=title)+
    scale_color_manual(values=c)+
    scale_fill_manual(values=c) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=1),size=0.5,color='black',alpha=0)+
    geom_line(data=conf_discrete_counts %>%
                filter(as.integer(subj_id)<20) %>%
                mutate(response=ifelse(response==1,labels[1],labels[2]) %>%
                         factor(levels=labels)),
              aes(x=cs_incorrect,y=cs_correct, group=interaction(subj_id,response), color=response),alpha=0.4) +
    theme(legend.position='none');


  AUClong <- AUC %>%
    gather('response','rcAUC',3:4) %>%
    mutate(response=ifelse(response=='response1',labels[1],labels[2])%>%factor(levels=labels),
           'auROC2'=rcAUC);

  AUCplot <- ggplot(AUClong, aes(x=response,y=auROC2, color=response)) +
    ylim(0,1)+
    geom_boxplot(size=1,fill='white',outlier.alpha=0)+
    geom_jitter(alpha=0.3,size=2, width=0.3) +
    theme(axis.text.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1)
    )+
    scale_color_manual(values=c)+
    scale_fill_manual(values=c)+
    theme_classic()+
    theme(legend.position='none',
          plot.margin=unit(c(10, 25, 15, 25),'pt'),
          axis.title.y=element_blank(),
          axis.title.x=element_blank())+
    coord_flip()+
    labs(title='auROC2',x='');

  return(list('rcROC'=rcROC, 'AUC'=AUCplot))

}

plotAllAsymmetries <- function(experiment, filename) {

  subj_df <- experiment$df %>%
    group_by(subj_id,detection,response) %>%
    summarise(confidence=mean(confidence),
              RT=median(RT));

  DetRTplot <- ggplot(subj_df %>%
                        filter(detection==1) %>%
                        mutate(response=factor(response,levels=c(1,0),
                                               labels=c('yes','no'))),
                      aes(x=response,y=RT,group=response,fill=response,color=response)) +
    ylim(0,2500)+
    geom_boxplot(size=1,fill='white',outlier.alpha=0)+
    geom_jitter(alpha=0.3,size=2, width=0.3) +
    theme(axis.text.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1)
    )+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    theme_classic()+
    theme(legend.position='none',
          plot.margin=unit(c(10, 25, 15, 25),'pt'),
          axis.title.y=element_blank(),
          axis.title.x=element_blank())+
    coord_flip()+
    labs(title='RT (ms)');

  Detconfplot <- ggplot(subj_df %>%
                          filter(detection==1) %>%
                          mutate(response=factor(response,levels=c(1,0),
                                                 labels=c('yes','no'))),
                        aes(x=response,y=confidence,group=response,fill=response,color=response)) +
    ylim(0,1)+
    geom_boxplot(size=1,fill='white',outlier.alpha=0)+
    geom_jitter(alpha=0.3,size=2, width=0.3) +
    theme(axis.text.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1)
    )+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    theme_classic()+
    theme(legend.position='none',
          plot.margin=unit(c(10, 25, 15, 25),'pt'),
          axis.title.y=element_blank(),
          axis.title.x=element_blank())+
    coord_flip()+
    labs(title='confidence');

  DisRTplot <- ggplot(subj_df %>%
                        filter(detection==0) %>%
                        mutate(response=factor(response,levels=c(1,0),
                                               labels=c('S1','S2'))),
                      aes(x=response,y=RT,group=response,fill=response,color=response)) +
    ylim(0,2500)+
    geom_boxplot(size=1,fill='white',outlier.alpha=0)+
    geom_jitter(alpha=0.3,size=2, width=0.3) +
    theme(axis.text.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1)
    )+
    scale_color_manual(values=discrimination_colors)+
    scale_fill_manual(values=discrimination_colors)+
    theme_classic()+
    theme(legend.position='none',
          plot.margin=unit(c(10, 25, 15, 25),'pt'),
          axis.title.y=element_blank(),
          axis.title.x=element_blank())+
    coord_flip()+
    labs(title='RT (ms)');

  Disconfplot <- ggplot(subj_df %>%
                          filter(detection==0) %>%
                          mutate(response=factor(response,levels=c(1,0),
                                                 labels=c('S1','S2'))),
                        aes(x=response,y=confidence,group=response,fill=response,color=response)) +
    geom_boxplot(size=1,fill='white',outlier.alpha=0)+
    geom_jitter(alpha=0.3,size=2, width=0.3) +
    theme(axis.text.x = element_blank(),
          plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1)
    )+
    scale_color_manual(values=discrimination_colors)+
    scale_fill_manual(values=discrimination_colors)+
    theme_classic()+
    theme(legend.position='none',
          plot.margin=unit(c(10, 25, 15, 25),'pt'),
          axis.title.y=element_blank(),
          axis.title.x=element_blank())+
    coord_flip()+
    labs(title='confidence');

  DetROCplot <- plotrcROC(experiment$detection_df,
                          c('yes','no'),
                          detection_colors,
                          title='Detection',
                          experiment$AUC%>%filter(detection==1));

  DisROCplot <- plotrcROC(experiment$discrimination_df,
                          c('S1','S2'),
                          discrimination_colors,
                          title='Discrimination',
                          experiment$AUC%>%filter(detection==0));

  p <- grid.arrange(DetROCplot$rcROC,DisROCplot$rcROC,DetROCplot$AUC,DisROCplot$AUC,DetRTplot,DisRTplot,Detconfplot,Disconfplot,layout_matrix=rbind(c(1,1,1,3,3,3),c(1,1,1,5,5,5),c(1,1,1,7,7,7),c(2,2,2,4,4,4),c(2,2,2,6,6,6),c(2,2,2,8,8,8)))
  ggsave(filename,p,width=8,height=8,dpi=300)

}

#####################################
# REVERSE CORRELATION VISUALIZATION #
#####################################

plotDiscriminationKernels <- function(exp, exp_label, xlim, ylim) {

  exp$discrimination_accuracy_plot <- ggplot(exp$discrimination_accuracy_kernel%>%
                                              group_by(contrast,time, subj_id) %>%
                                              summarise(evidence=mean(evidence)) %>%
                                              group_by(contrast,time)%>%
                                              summarise(se=se(evidence, na.rm=T),
                                                        evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC', exp_label,'discrimination_accuracy.png',sep='/'),
         width=3.5,height=3)


  exp$discrimination_confidence_plot <- ggplot(exp$discrimination_confidence_kernel%>%
                                                group_by(side,time, subj_id) %>%
                                                summarise(evidence=mean(diff)) %>%
                                                group_by(side,time)%>%
                                                summarise(se=se(evidence, na.rm=T),
                                                          evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=discrimination_colors)+
    scale_fill_manual(values=discrimination_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'discrimination_confidence.png',sep='/'),
         width=3.5,height=3)


  exp$discrimination_confidence_sum_rel_plot <- ggplot(exp$discrimination_confidence_kernel%>%
                                                        group_by(time, subj_id) %>%
                                                        summarise(relative_evidence=mean(diff[side=='chosen'])-mean(diff[side=='unchosen']),
                                                                  sum_evidence = mean(diff[side=='chosen'])+mean(diff[side=='unchosen'])) %>%
                                                        pivot_longer(cols=ends_with('evidence'), names_to='contrast', values_to='evidence') %>%
                                                        group_by(contrast,time)%>%
                                                        summarise(se=se(evidence, na.rm=T),
                                                                  evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'discrimination_confidence_sum_rel.png',sep='/'),
         width=3.5,height=3);
  
  exp$discrimination_confidence_objective_plot <- ggplot(exp$discrimination_confidence_kernel_objective%>%
                                                 group_by(obj_side,time, subj_id) %>%
                                                 summarise(evidence=mean(diff)) %>%
                                                 group_by(obj_side,time)%>%
                                                 summarise(se=se(evidence, na.rm=T),
                                                           evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=obj_side)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=obj_side),alpha=0.5) +
    scale_color_manual(values=discrimination_colors)+
    scale_fill_manual(values=discrimination_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');
  
  ggsave(paste('./figures/RC',exp_label,'discrimination_confidence_objective.png',sep='/'),
         width=3.5,height=3)
  
  
  exp$discrimination_confidence_objective_sum_rel_plot <- ggplot(exp$discrimination_confidence_kernel_objective%>%
                                                         group_by(time, subj_id) %>%
                                                         summarise(relative_evidence=mean(diff[obj_side=='true'])-mean(diff[obj_side=='opposite']),
                                                                   sum_evidence = mean(diff[obj_side=='true'])+mean(diff[obj_side=='opposite'])) %>%
                                                         pivot_longer(cols=ends_with('evidence'), names_to='contrast', values_to='evidence') %>%
                                                         group_by(contrast,time)%>%
                                                         summarise(se=se(evidence, na.rm=T),
                                                                   evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');
  
  ggsave(paste('./figures/RC',exp_label,'discrimination_confidence_objective_sum_rel.png',sep='/'),
         width=3.5,height=3)

  exp$discrimination_confidence_incorrect_plot <- ggplot(exp$discrimination_confidence_kernel_incorrect%>%
                                                 group_by(side,time, subj_id) %>%
                                                 summarise(evidence=mean(diff)) %>%
                                                 group_by(side,time)%>%
                                                 summarise(se=se(evidence, na.rm=T),
                                                           evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=discrimination_colors)+
    scale_fill_manual(values=discrimination_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');
  
  ggsave(paste('./figures/RC',exp_label,'discrimination_confidence_incorrect.png',sep='/'),
         width=3.5,height=3)
  
  
  exp$discrimination_confidence_incorrect_sum_rel_plot <- ggplot(exp$discrimination_confidence_kernel_incorrect%>%
                                                         group_by(time, subj_id) %>%
                                                         summarise(relative_evidence=mean(diff[side=='chosen'])-mean(diff[side=='unchosen']),
                                                                   sum_evidence = mean(diff[side=='chosen'])+mean(diff[side=='unchosen'])) %>%
                                                         pivot_longer(cols=ends_with('evidence'), names_to='contrast', values_to='evidence') %>%
                                                         group_by(contrast,time)%>%
                                                         summarise(se=se(evidence, na.rm=T),
                                                                   evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits=xlim) +
    scale_y_continuous(limits=ylim) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');
  
  ggsave(paste('./figures/RC',exp_label,'discrimination_confidence_incorrect_sum_rel.png',sep='/'),
         width=3.5,height=3);
  
  return(exp)
}


plotDetectionSignalKernels <- function(exp, exp_label, xlim, ylim) {

  exp$det_decision_plot <- ggplot(exp$signal_decision_kernel%>%
                                   group_by(side,time, subj_id) %>%
                                   summarise(evidence = mean(evidence)) %>%
                                   group_by(side,time) %>%
                                   summarise(se=se(evidence),
                                             evidence=mean(evidence)),
                                 aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0) +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)+
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'detection_decision.png',sep='/'),
         width=3.5,height=3)

  exp$det_decision_sum_rel_plot <- ggplot(exp$signal_decision_kernel%>%
                                            group_by(subj_id,side,time) %>%
                                            summarise(evidence=mean(evidence)) %>%
                                            group_by(subj_id,time)%>%
                                            summarise(rel_evidence = evidence[side=='true']-evidence[side=='opposite'],
                                                      sum_evidence = evidence[side=='true']+evidence[side=='opposite']) %>%
                                            pivot_longer(cols = ends_with('evidence'),names_to='contrast',values_to='evidence')%>%
                                           group_by(contrast,time) %>%
                                           summarise(se=se(evidence),
                                                     evidence=mean(evidence)),
                                         aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0) +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)+
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'detection_decision_sum_rel.png',sep='/'),
         width=3.5,height=3)

  exp$conf_yes_plot <- ggplot(exp$signal_confidence_kernel%>%
                               filter(response==1)%>%
                               group_by(subj_id,side,time) %>%
                               summarise(evidence=mean(diff)) %>%
                               group_by(side,time) %>%
                               summarise(se=se(evidence),
                                         evidence=mean(evidence)),
                             aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0) +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)+
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'detection_conf_yes.png',sep='/'),
         width=3.5,height=3)

  exp$conf_yes_sum_rel_plot <- ggplot(exp$signal_confidence_kernel%>%
                                filter(response==1)%>%
                                group_by(subj_id,time) %>%
                                summarise(rel_evidence=mean(diff[side=='true'])-mean(diff[side=='opposite']),
                                          sum_evidence = mean(diff[side=='true'])+mean(diff[side=='opposite'])) %>%
                                  pivot_longer(cols = ends_with('evidence'),names_to='contrast',values_to='evidence')%>%
                                  group_by(contrast,time) %>%
                                  summarise(se=se(evidence),
                                            evidence=mean(evidence)),
                                aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0) +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)+
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'detection_conf_yes_sum_rel.png',sep='/'),
         width=3.5,height=3)


  exp$conf_no_plot <- ggplot(exp$signal_confidence_kernel%>%
                                filter(response==0)%>%
                                group_by(subj_id,side,time) %>%
                                summarise(evidence=mean(diff)) %>%
                                group_by(side,time) %>%
                                summarise(se=se(evidence),
                                          evidence=mean(evidence)),
                              aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0) +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)+
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'detection_conf_no.png',sep='/'),
         width=3.5,height=3)


  exp$conf_no_sum_rel_plot <- ggplot(exp$signal_confidence_kernel%>%
                                        filter(response==0)%>%
                                        group_by(subj_id,time) %>%
                                        summarise(rel_evidence=mean(diff[side=='true'])-mean(diff[side=='opposite']),
                                                  sum_evidence = mean(diff[side=='true'])+mean(diff[side=='opposite'])) %>%
                                        pivot_longer(cols = ends_with('evidence'),names_to='contrast',values_to='evidence')%>%
                                        group_by(contrast,time) %>%
                                        summarise(se=se(evidence),
                                                  evidence=mean(evidence)),
                                      aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0) +
    # annotate(geom = "rect", xmin=0, xmax=300, ymin=-0.6,ymax=-0.55,
    #          color="transparent", fill="black") +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim)+
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='contrast',
         x='time (ms.)')+
    theme(legend.position = 'none');

  ggsave(paste('./figures/RC',exp_label,'detection_conf_no_sum_rel.png',sep='/'),
         width=3.5,height=3)

 return(exp)

}
