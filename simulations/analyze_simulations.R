library('tidyverse')
library('papaja')
detection_colors = c('#377eb8', '#e41a1c');
discrimination_colors = c('#4daf4a','#984ea3')
evidence_colors = c('black','#ebac23')


load_simulation <- function(simulation_name, mu_signal, trials_per_subj) {

  sim = list();
  sim$name = simulation_name

  #load discrimination
  sim$discrimination <-  read_csv(paste('../simulations',simulation_name, '/discrimination.csv', sep = '/'),lazy=FALSE) %>%
    mutate(subj_id = floor(trial_id/trials_per_subj),
           evidence=ifelse(side==bright_side,
                           evidence-mu_signal,
                           evidence),
           time=(timepoint-1)*40,
           obj_side=factor(ifelse(side==bright_side,
                                  'true',
                                  'opposite'),levels=c('true','opposite')),
           side = factor(ifelse(decision==side,
                                'chosen',
                                'unchosen'),
                         levels=c('chosen','unchosen'))) %>%
    group_by(subj_id) %>%
    mutate(
      median_confidence=median(confidence, na.rm=T),
      binaryconf = ifelse(confidence>=median_confidence, 1, 0),
    );

  #load detection
  sim$detection <- read_csv(paste('../simulations',simulation_name, '/detection.csv',sep='/'),
                           lazy=FALSE) %>%
    mutate(subj_id = floor(trial_id/trials_per_subj),
           evidence=ifelse(side==bright_side & signal,
                           evidence-mu_signal,
                           evidence),
           time=(timepoint-1)*40,
           side=factor(ifelse(side==bright_side,
                                  'target',
                                  'foil'),levels=c('target','foil'))) %>%
    group_by(subj_id, decision) %>%
    mutate(median_confidence=median(confidence)) %>%
    ungroup() %>%
    mutate(
      binaryconf = ifelse(confidence>=median_confidence, 1, 0),
    );

  return(sim);

}

get_simulation_kernels <- function(sim) {

  # discrimination

  sim$discrimination_decision_kernel <- sim$discrimination %>%
    group_by (subj_id,side,time) %>%
    summarise(evidence=mean(evidence))

  sim$discrimination_accuracy_kernel <- sim$discrimination %>%
    drop_na()%>%
    group_by (subj_id,obj_side,time,correct) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,obj_side,time) %>%
    summarise(evidence=evidence[correct==1]-evidence[correct==0]) %>%
    group_by(subj_id,time) %>%
    summarise(relative_evidence = evidence[obj_side=='true']-evidence[obj_side=='opposite'],
              sum_evidence = evidence[obj_side=='true']+evidence[obj_side=='opposite']) %>%
    pivot_longer(cols=ends_with('evidence'),
                 names_to = 'contrast',
                 values_to = 'evidence')

  sim$discrimination_confidence_kernel <- sim$discrimination %>%
    group_by (subj_id,side,time,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,side,time) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])


  # detection
  sim$detection_decision_kernel <- sim$detection %>%
    group_by (subj_id,decision,time) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,time) %>%
    summarise(diff=evidence[decision==1]-evidence[decision==0]);

  sim$detection_confidence_kernel <- sim$detection %>%
    group_by (subj_id,decision,time,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,time,decision) %>%
    summarise(diff=evidence[binaryconf==1]-evidence[binaryconf==0]);


  # detection: signal trials only

  sim$signal <- sim$detection %>%
    filter(signal==1) %>%
    group_by(subj_id, decision) %>%
    mutate(median_confidence=median(confidence)) %>%
    ungroup() %>%
    mutate(
      binaryconf = ifelse(confidence>=median_confidence, 1, 0),
    );

  sim$signal_decision_kernel <- sim$signal %>%
    group_by (subj_id,side,time,decision) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,side,time) %>%
    summarise(evidence=evidence[decision==1]-evidence[decision==0])

  sim$signal_confidence_kernel <- sim$signal %>%
    group_by (subj_id,side,time,binaryconf,decision) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,side,time,decision) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])

  sim$hit_confidence_kernel <- sim$signal_confidence_kernel %>%
    filter(decision==1);

  sim$miss_confidence_kernel <- sim$signal_confidence_kernel %>%
    filter(decision==0)

  return(sim)

}

contrast_kernels <- function(sim) {

  sim$RC = list()

  # discrimination

  sim$RC$decision <- sim$discrimination_decision_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='chosen'])-mean(evidence[side=='unchosen'])) %>%
    pull(diff);

  sim$RC$decisionPEB <- sim$discrimination_decision_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='chosen'])+mean(evidence[side=='unchosen'])) %>%
    pull(diff)

  sim$RC$accuracy <- sim$discrimination_accuracy_kernel %>%
    filter(contrast=='relative_evidence') %>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence)) %>%
    pull(diff)

  sim$RC$accuracyPEB <- sim$discrimination_accuracy_kernel %>%
    filter(contrast=='sum_evidence') %>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence)) %>%
    pull(diff)

  sim$RC$confidence <- sim$discrimination_confidence_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])-mean(diff[side=='unchosen'])) %>%
    pull(diff)

  sim$RC$confidencePEB <- sim$discrimination_confidence_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])+mean(diff[side=='unchosen'])) %>%
    pull(diff)

  # detection: sum evidence

  sim$RC$detectionDecision <- sim$detection_decision_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff)) %>%
    pull(diff)

  sim$RC$detectionConfidenceYes <- sim$detection_confidence_kernel %>%
    filter(decision==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff)) %>%
    pull(diff)

  sim$RC$detectionConfidenceNo <- sim$detection_confidence_kernel %>%
    filter(decision==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff)) %>%
    pull(diff)

  # detection: signal trials

  sim$RC$signalDecision <- sim$signal_decision_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='target'])-mean(evidence[side=='foil'])) %>%
    pull(diff)

  sim$RC$signalDecisionPEB <- sim$signal_decision_kernel %>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='target'])+mean(evidence[side=='foil'])) %>%
    pull(diff)

  sim$RC$signalConfidenceYes <- sim$signal_confidence_kernel %>%
    filter(decision==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='target'])-mean(diff[side=='foil'])) %>%
    pull(diff)

  sim$RC$signal_confidenceNo <- sim$signal_confidence_kernel %>%
    filter(decision==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='target'])-mean(diff[side=='foil'])) %>%
    pull(diff)

  sim$RC$signalConfidencePEBYes <- sim$signal_confidence_kernel %>%
    filter(decision==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='target'])+mean(diff[side=='foil'])) %>%
    pull(diff)

  sim$RC$signalConfidencePEBNo <- sim$signal_confidence_kernel %>%
    filter(decision==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='target'])+mean(diff[side=='foil'])) %>%
    pull(diff)

  return(sim)
}

plot_RC <- function(sim) {

  # discrimination

  accuracy_plot <- ggplot(sim$discrimination_accuracy_kernel%>%
                 group_by(contrast,time,subj_id) %>%
                 summarise(evidence=mean(evidence)) %>%
                 group_by(contrast,time)%>%
                 summarise(se=se(evidence),
                           evidence=mean(evidence)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  confidence_plot <- ggplot(sim$discrimination_confidence_kernel%>%
                 group_by(side,time, subj_id) %>%
                 summarise(evidence=mean(diff, na.rm=T)) %>%
                 group_by(side,time)%>%
                 summarise(se=se(evidence, na.rm=T),
                           evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=discrimination_colors)+
    scale_fill_manual(values=discrimination_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  confidence_evidence_plot <- ggplot(sim$discrimination_confidence_kernel%>%
                              group_by(time, subj_id) %>%
                              summarise(relative_evidence=mean(diff[side=='chosen'])-mean(diff[side=='unchosen']),
                                        sum_evidence=mean(diff[side=='chosen'])+mean(diff[side=='unchosen'])) %>%
                              pivot_longer(cols = ends_with('evidence'), names_to = 'contrast', values_to = 'evidence')%>%
                              group_by(contrast,time)%>%
                              summarise(se=se(evidence, na.rm=T),
                                        evidence=mean(evidence, na.rm=T)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  p<- grid.arrange(accuracy_plot+labs(title='Accuracy:discrimination'), confidence_plot+labs(title='Confidence: discrimination'), confidence_evidence_plot, ncol=1)
  ggsave(paste('../docs/figures/simulations/RC-discrimination-',sim$name,'.pdf', sep=''),p,width=3,height=5)

  # detection

  detection_plot <- ggplot(sim$signal_decision_kernel%>%
                             group_by(side,time, subj_id) %>%
                             summarise(evidence=mean(evidence)) %>%
                             group_by(side,time)%>%
                             summarise(se=se(evidence),
                                       evidence=mean(evidence)),aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)',
         fill= 'side',
         color='side')+
    theme(legend.position = 'none');

  confidence_yes_plot <- ggplot(sim$signal_confidence_kernel%>%
                                        filter(decision==1)%>%
                                        group_by(side,time, subj_id) %>%
                                        summarise(evidence=mean(diff)) %>%
                                        group_by(side,time)%>%
                                        summarise(se=se(evidence),
                                                  evidence=mean(evidence)),aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  confidence_no_plot <- ggplot(sim$signal_confidence_kernel%>%
                                  filter(decision==0)%>%
                                  group_by(side,time, subj_id) %>%
                                  summarise(evidence=mean(diff)) %>%
                                  group_by(side,time)%>%
                                  summarise(se=se(evidence),
                                            evidence=mean(evidence)),aes(x=time,y=evidence,color=side)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=side),alpha=0.5) +
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)')+
    theme(legend.position = 'none');

  detection_evidence_plot <- ggplot(sim$signal_decision_kernel%>%
                             group_by(time, subj_id) %>%
                             summarise(relative_evidence=mean(evidence[side=='target'])-mean(evidence[side=='foil']),
                                       sum_evidence=mean(evidence[side=='target'])+mean(evidence[side=='foil'])) %>%
                             pivot_longer(cols = ends_with('evidence'), names_to = 'contrast', values_to = 'evidence')%>%
                             group_by(contrast,time)%>%
                             summarise(se=se(evidence),
                                       evidence=mean(evidence)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)',
         fill= 'side',
         color='side')+
    theme(legend.position = 'none');

  confidence_yes_evidence_plot <- ggplot(sim$signal_confidence_kernel%>%
                                          filter(decision==1)%>%
                                          group_by(time, subj_id) %>%
                                          summarise(relative_evidence=mean(diff[side=='target'])-mean(diff[side=='foil']),
                                                    sum_evidence=mean(diff[side=='target'])+mean(diff[side=='foil'])) %>%
                                          pivot_longer(cols = ends_with('evidence'), names_to = 'contrast', values_to = 'evidence')%>%
                                          group_by(contrast,time)%>%
                                          summarise(se=se(evidence),
                                                    evidence=mean(evidence)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)',
         fill= 'side',
         color='side')+
    theme(legend.position = 'none');

  confidence_no_evidence_plot <- ggplot(sim$signal_confidence_kernel%>%
                                           filter(decision==0)%>%
                                           group_by(time, subj_id) %>%
                                           summarise(relative_evidence=mean(diff[side=='target'])-mean(diff[side=='foil']),
                                                     sum_evidence=mean(diff[side=='target'])+mean(diff[side=='foil'])) %>%
                                           pivot_longer(cols = ends_with('evidence'), names_to = 'contrast', values_to = 'evidence')%>%
                                           group_by(contrast,time)%>%
                                           summarise(se=se(evidence),
                                                     evidence=mean(evidence)),aes(x=time,y=evidence,color=contrast)) +
    geom_hline(yintercept=0)  +
    geom_line() +
    geom_ribbon(aes(ymin = evidence-se, ymax = evidence+se, fill=contrast),alpha=0.5) +
    scale_color_manual(values=evidence_colors)+
    scale_fill_manual(values=evidence_colors) +
    theme_minimal()+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    labs(y='evidence',
         x='time (ms.)',
         fill= 'side',
         color='side')+
    theme(legend.position = 'none');

  p<- grid.arrange(detection_plot+labs(title='Decision: detection'),
                   confidence_yes_plot+labs(title='Confidence: "yes"'),
                   confidence_no_plot+labs(title='Confidence: "no"'),
                   detection_evidence_plot,
                   confidence_yes_evidence_plot,
                   confidence_no_evidence_plot, ncol=3)
  ggsave(paste('../docs/figures/simulations/RC-detection-',sim$name,'.pdf', sep=''),p,width=8,height=5)

  return(sim)
}

uv <- load_simulation('unequal_variance', 0.5, 100) %>%
  get_simulation_kernels() %>%
  contrast_kernels()

ra <- load_simulation('random_attention', 0.5, 100) %>%
  get_simulation_kernels() %>%
  contrast_kernels()

gda <- load_simulation('goal_directed_attention', 0.5, 100) %>%
  get_simulation_kernels() %>%
  contrast_kernels()

ev <- load_simulation('equal_variance', 0.5, 100) %>%
  get_simulation_kernels() %>%
  contrast_kernels()
