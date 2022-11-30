# The purpose of this analysis is to ask whether decision and confidence
# kernels are more correlated within tasks, or between tasks. To make sure
# that any differences are not driven by the fact that between-task correlations 
# are of the same trials, I compute separate kernels for the first and second
# halves of the experiment, and compute correlations for the cross-over.
# Overall, correlations seem to be higher within a task that between tasks.

getKernelsByPart <- function(exp) {

  discrimination_confidence_kernel_by_block <- exp$discRCdf %>%
    filter(correct==1) %>%
    group_by(subj_id) %>%
    mutate(part=ifelse(trial_id>median(trial_id),
                       1,2),
           side=ifelse(side=='chosen',1,2),
           # this i is just to make sure that the order is preserved.
           # I average over eccentricity levels.
           i = side*1000+time)%>%
    group_by (subj_id,part,i,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,i,part) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0]) %>%
    spread(part,diff,sep='_')
  
  discrimination_accuracy_kernel_by_block <- exp$discRCdf %>%
    drop_na()%>%
    group_by(subj_id) %>%
    mutate(part=ifelse(trial_id>median(trial_id),
                       1,2),
           obj_side=ifelse(obj_side=='true',1,2),
           i = obj_side*1000+time)%>%
    group_by (subj_id,i,correct, part) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,i, part) %>%
    summarise(diff=evidence[correct==1]-evidence[correct==0]) %>%
    spread(part,diff,sep='_') %>%
    drop_na()
  
  signal_decision_kernel_by_block <- exp$signalRCdf %>%
    group_by(subj_id) %>%
    mutate(part=ifelse(trial_id>median(trial_id),
                       1,2),
           side=ifelse(side=='true',1,2),
           i = side*1000+time)%>%
    group_by (subj_id,part,i) %>%
    summarise(evidence=mean(evidence[response==1]) -
                mean(evidence[response==0])) %>%
    spread(part,evidence,sep='_')
  
  signal_confidence_kernel_by_block <- exp$signalRCdf %>%
    filter(correct==1)%>%
    group_by(subj_id) %>%
    mutate(part=ifelse(trial_id>median(trial_id),
                       1,2),
           side=ifelse(side=='true',1,2),
           i = side*1000+time)%>%
    group_by (subj_id,part,i) %>%
    summarise(diff = mean(evidence[binaryconf==1])-
                mean(evidence[binaryconf==0])) %>%
    spread(part,diff,sep='_')
  
  # aggregate everything within one df, to make sure kernels are aligned.
  exp$kernels_by_block <- discrimination_confidence_kernel_by_block %>%
    rename(disc_conf_1=part_1,
           disc_conf_2 = part_2) %>%
    merge(discrimination_accuracy_kernel_by_block %>%
            rename(disc_dec_1=part_1,
                   disc_dec_2 = part_2)) %>%
    merge(signal_decision_kernel_by_block %>%
            rename(det_dec_1=part_1,
                   det_dec_2 = part_2)
    ) %>%
    merge(signal_confidence_kernel_by_block %>%
            rename(det_conf_1=part_1,
                   det_conf_2=part_2))
  
  # extract correlations
  exp$kernel_correlations <- exp$kernels_by_block %>%
    group_by(subj_id) %>%
    summarise(
      #note that the order is block 1, block 2 for decisions, but block 2, 
      #block 1 for confidence. This way, within-task correlations
      #don't enjoy the advantage of a correlation between the same trials. 
      dec_det_conf_det = cor(c(det_dec_1,det_dec_2),c(det_conf_2,det_conf_1)),
      dec_det_conf_dis = cor(c(det_dec_1,det_dec_2),c(disc_conf_2,disc_conf_1)),
      dec_dis_conf_det = cor(c(disc_dec_1,disc_dec_2),c(det_conf_2,det_conf_1)),
      dec_dis_conf_dis = cor(c(disc_dec_1,disc_dec_2),c(disc_conf_2,disc_conf_1)),
    )
  return(exp)
  
}

e1 <- e1 %>%
  getKernelsByPart()

e2 <- e2 %>%
  getKernelsByPart()

e3 <- e3 %>%
  getKernelsByPart()

e4 <- e4 %>%
  getKernelsByPart()