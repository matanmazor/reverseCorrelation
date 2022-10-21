getDiscriminationKernels <- function(exp) {

  exp$discrimination_accuracy_kernel <- exp$discRCdf %>%
    drop_na()%>%
    group_by (subj_id,obj_side,time,eccentricity,correct) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,obj_side,time,eccentricity) %>%
    summarise(diff_evidence=evidence[correct==1]-evidence[correct==0]) %>%
    group_by(subj_id,time,eccentricity) %>%
    summarise(relative_evidence = diff_evidence[obj_side=='true']-diff_evidence[obj_side=='opposite'],
              sum_evidence = diff_evidence[obj_side=='true']+diff_evidence[obj_side=='opposite']) %>%
    pivot_longer(cols = ends_with('evidence'), names_to='contrast',values_to='evidence');

  exp$discrimination_confidence_kernel <- exp$discRCdf %>%
    group_by (subj_id,side,time,eccentricity,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,side,time,eccentricity) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])

  exp$discrimination_confidence_kernel_correct <- exp$discRCdf %>%
    filter(correct==1) %>%
    group_by (subj_id,side,time,eccentricity,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,side,time,eccentricity) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])
  
  exp$discrimination_confidence_kernel_incorrect <- exp$discRCdf %>%
    filter(correct==0) %>%
    group_by (subj_id,side,time,eccentricity,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,side,time,eccentricity) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])

  exp$discrimination_confidence_kernel_objective <- exp$discRCdf %>%
    group_by (subj_id,obj_side,time,eccentricity,binaryconf) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,obj_side,time,eccentricity) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])
  
  return(exp)
}

getDetectionKernels <- function(exp) {

  exp$detection_decision_kernel <- exp$detectionRCdf %>%
    group_by (subj_id,time,eccentricity, response) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,time,eccentricity) %>%
    summarise(evidence=evidence[response==1]-evidence[response==0])

  exp$detection_confidence_kernel <- exp$detectionRCdf %>%
    group_by (subj_id,time,eccentricity,binaryconf,response) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,time,eccentricity,response) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])

  return(exp)

}

getDetectionSignalKernels <- function(exp) {

  exp$signal_decision_kernel <- exp$signalRCdf %>%
    group_by (subj_id,side,time,eccentricity, response) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by(subj_id,side,time,eccentricity) %>%
    summarise(evidence=evidence[response==1]-evidence[response==0])

  exp$signal_confidence_kernel <- exp$signalRCdf %>%
    group_by (subj_id,side,time,eccentricity,binaryconf,response) %>%
    summarise(evidence=mean(evidence)) %>%
    group_by (subj_id,side,time,eccentricity,response) %>%
    summarise(diff = evidence[binaryconf==1]-evidence[binaryconf==0])

  return(exp)

}

contrastDiscriminationKernels <- function(exp) {

  exp$RC$accuracy_rel300 <- exp$discrimination_accuracy_kernel %>%
    filter(time<300 & contrast=='relative_evidence')%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence));

  exp$RC$accuracy_sum300 <- exp$discrimination_accuracy_kernel %>%
    filter(time<300 & contrast=='sum_evidence')%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence));

  exp$RC$confidence_pos300 <- exp$discrimination_confidence_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen']));

  exp$RC$confidence_neg300 <- exp$discrimination_confidence_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='unchosen']));

  exp$RC$confidence_rel300 <- exp$discrimination_confidence_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])-mean(diff[side=='unchosen']));

  exp$RC$confidence_sum300 <- exp$discrimination_confidence_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])+mean(diff[side=='unchosen']));

  exp$RC$confidence_pos_correct300 <- exp$discrimination_confidence_kernel_correct %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen']));

  exp$RC$confidence_neg_correct300 <- exp$discrimination_confidence_kernel_correct %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='unchosen']));

  exp$RC$confidence_rel_correct300 <- exp$discrimination_confidence_kernel_correct %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])-mean(diff[side=='unchosen']));

  exp$RC$confidence_sum_correct300 <- exp$discrimination_confidence_kernel_correct %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])+mean(diff[side=='unchosen']));
  
  exp$RC$confidence_pos_incorrect300 <- exp$discrimination_confidence_kernel_incorrect %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen']));
  
  exp$RC$confidence_neg_incorrect300 <- exp$discrimination_confidence_kernel_incorrect %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='unchosen']));
  
  exp$RC$confidence_rel_incorrect300 <- exp$discrimination_confidence_kernel_incorrect %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])-mean(diff[side=='unchosen']));
  
  exp$RC$confidence_sum_incorrect300 <- exp$discrimination_confidence_kernel_incorrect %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='chosen'])+mean(diff[side=='unchosen']));
  
  exp$RC$confidence_pos_objective300 <- exp$discrimination_confidence_kernel_objective %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[obj_side=='true']));
  
  exp$RC$confidence_neg_objective300 <- exp$discrimination_confidence_kernel_objective %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[obj_side=='opposite']));
  
  exp$RC$confidence_rel_objective300 <- exp$discrimination_confidence_kernel_objective %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[obj_side=='true'])-mean(diff[obj_side=='opposite']));
  
  exp$RC$confidence_sum_objective300 <- exp$discrimination_confidence_kernel_objective %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[obj_side=='true'])+mean(diff[obj_side=='opposite']));

  return(exp)
}

contrastDetectionKernels <- function(exp) {

  exp$RC$detection_decision_sum300 <- exp$detection_decision_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence));

  exp$RC$detection_confidenceYes_sum300 <- exp$detection_confidence_kernel %>%
    filter(time<300 & response==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff));

  exp$RC$detection_confidenceNo_sum300 <- exp$detection_confidence_kernel %>%
    filter(time<300 & response==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff));

  return(exp)

}

contrastDetectionSignalKernels <- function(exp) {

  exp$RC$signal_decision_pos300 <- exp$signal_decision_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='true']));

  exp$RC$signal_decision_neg300 <- exp$signal_decision_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='opposite']));

  exp$RC$signal_decision_rel300 <- exp$signal_decision_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='true']-evidence[side=='opposite']));

  exp$RC$signal_decision_sum300 <- exp$signal_decision_kernel %>%
    filter(time<300)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(evidence[side=='true']+evidence[side=='opposite']));

  exp$RC$signal_confidenceYes_pos300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='true']));

  exp$RC$signal_confidenceYes_neg300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='opposite']));

  exp$RC$signal_confidenceYes_rel300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='true'])-mean(diff[side=='opposite']));

  exp$RC$signal_confidenceYes_sum300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==1)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='true'])+mean(diff[side=='opposite']));

  exp$RC$signal_confidenceNo_pos300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='true']));

  exp$RC$signal_confidenceNo_neg300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='opposite']));

  exp$RC$signal_confidenceNo_rel300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='true'])-mean(diff[side=='opposite']));

  exp$RC$signal_confidenceNo_sum300 <- exp$signal_confidence_kernel %>%
    filter(time<300 & response==0)%>%
    group_by(subj_id) %>%
    summarise(diff=mean(diff[side=='true'])+mean(diff[side=='opposite']));

  return(exp)

}


aggregateEffects <- function(exp) {
  
  exp$RC$pos <- exp$RC$confidence_pos300 %>%
    rename(discrimination_confidence=diff) %>%
    merge(exp$RC$signal_decision_pos300 %>%
            rename(detection_decision=diff)) %>%
    merge(exp$RC$signal_confidenceYes_pos300 %>%
            rename(confidence_yes=diff), by='subj_id') %>%
    merge(exp$RC$signal_confidenceNo_pos300 %>%
            rename(confidence_no=diff), by='subj_id')
  
  exp$RC$neg <- exp$RC$confidence_neg300 %>%
    rename(discrimination_confidence=diff) %>%
    merge(exp$RC$signal_decision_neg300 %>%
            rename(detection_decision=diff)) %>%
    merge(exp$RC$signal_confidenceYes_neg300 %>%
            rename(confidence_yes=diff), by='subj_id') %>%
    merge(exp$RC$signal_confidenceNo_neg300 %>%
            rename(confidence_no=diff), by='subj_id')
  
  exp$RC$rel <- exp$RC$confidence_rel300 %>%
    rename(discrimination_confidence=diff) %>%
    merge(exp$RC$signal_decision_rel300 %>%
            rename(detection_decision=diff)) %>%
    merge(exp$RC$signal_confidenceYes_rel300 %>%
            rename(confidence_yes=diff), by='subj_id') %>%
    merge(exp$RC$signal_confidenceNo_rel300 %>%
            rename(confidence_no=diff), by='subj_id')
  
  exp$RC$sum <- exp$RC$confidence_sum300 %>%
    rename(discrimination_confidence=diff) %>%
    merge(exp$RC$signal_decision_sum300 %>%
            rename(detection_decision=diff)) %>%
    merge(exp$RC$signal_confidenceYes_sum300 %>%
            rename(confidence_yes=diff), by='subj_id') %>%
    merge(exp$RC$signal_confidenceNo_sum300 %>%
            rename(confidence_no=diff), by='subj_id')
  
  return(exp)
}