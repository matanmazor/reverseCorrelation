# get basic stats
generalStats <- function(df) {

  stats = list();

  # GENERAL STATS
  stats$general <- df %>%
    group_by(subj_id) %>%
    summarise(
      bias = mean(ifelse(response==1,1,0)),
      acc = mean(ifelse(correct==1,1,0)),
      hit_rate = sum(correct==1 & stimulus==1)/sum(stimulus==1),
      false_alarm_rate=sum(correct==0 & stimulus==0)/sum(stimulus==0),
      dprime=qnorm(hit_rate)-qnorm(false_alarm_rate),
      confidence=mean(confidence),
    )

  # STATS AS A FUNCTION OF ACCURACY
  stats$by_acc <-
    df %>%
    group_by(subj_id, correct) %>%
    summarise(
      RT = mean(RT),
      confidence = mean(confidence),
      logRT = mean(logRT)
    )

  # STATS PER RESPONSE
  stats$by_response <-
    df %>%
    group_by(subj_id, response) %>%
    summarise(
      cor = cor(x=logRT, y=confidence), #correlation between RT and confidence
      RT = mean(RT),
      logRT = mean(logRT),
      confidence = mean(confidence),
      acc=mean(correct),
      count = n()
    )

  # A DIRECT CONTRAST BETWEEN THE TWO RESPONSES
  stats$contrast_responses <-
    stats$by_response %>%
    group_by(subj_id) %>%
    summarise(
      RT=RT[response==1]-RT[response==0],
      logRT = logRT[response==1]-logRT[response==0],
      confidence=confidence[response==1]-confidence[response==0],
      cor = cor[response==1]-cor[response==0],
      acc = acc[response==1]-acc[response==0])

  return(stats)

}

# get stats for both tasks

generalStats2Tasks <- function(e) {

  e$detection_stats <- generalStats(e$detection_df);

  e$discrimination_stats <- generalStats(e$discrimination_df);

  e$task_stats <- rbind(e$detection_stats$general%>%mutate(detection=1),
                        e$discrimination_stats$general%>%mutate(detection=0))

  return(e)
}


# Compare area under the response conditional curve
testAUC <- function(e) {

  e$made_errors <- e$trial_df %>%
    group_by(subj_id, detection, response, correct, .drop=FALSE) %>%
    tally() %>%
    group_by(subj_id) %>%
    summarise(min_per_cell=min(n))%>%
    filter(min_per_cell>1) %>%
    pull(subj_id);

  e$conf_counts <- e$trial_df %>%
    filter(subj_id %in% e$made_errors) %>%
    mutate(subj_id=factor(subj_id)) %>%
    group_by(subj_id, detection, response, correct, confidence, .drop=FALSE) %>%
    tally() %>%
    spread(correct, n, sep='', fill=0) %>%
    arrange(desc(confidence), by_group=TRUE) %>%
    group_by(subj_id, detection, response)%>%
    mutate(cs_correct=cumsum(correct1)/sum(correct1),
           cs_incorrect=cumsum(correct0)/sum(correct0));

  e$conf_counts <-e$conf_counts %>%
    group_by(subj_id, detection, response,.drop=TRUE) %>%
    summarise(
      cs_correct=c(0,1),
      cs_incorrect=c(0,1)) %>%
    bind_rows(e$conf_counts,.) %>%
    group_by(subj_id, detection, response,cs_incorrect) %>%
    summarise(cs_correct=max(cs_correct)) %>%
    merge(e$task_stats%>%dplyr::select(subj_id,detection,dprime, hit_rate, false_alarm_rate))%>%
    mutate(miss_rate=1-hit_rate,
           cr_rate=1-false_alarm_rate,
           cs_correct_from_sdt= ifelse(response==1,
                                       pnorm(qnorm(false_alarm_rate*cs_incorrect), mean=-dprime)/hit_rate,
                                       pnorm(qnorm(miss_rate*cs_incorrect), mean=-dprime)/cr_rate));

  e$AUC <- e$conf_counts %>%
    group_by(subj_id, detection, response,.drop=TRUE) %>%
    summarise(AUC = auc(cs_incorrect, cs_correct)) %>%
    spread(response, AUC, sep='')%>%
    mutate(metacognitive_asymmetry=(response1-response0),
           average_AUC=response1/2+response0/2);

  e$sdtAUC <- e$conf_counts %>%
    group_by(subj_id, detection, response,.drop=TRUE) %>%
    summarise(AUC = auc(cs_incorrect, cs_correct_from_sdt)) %>%
    spread(response, AUC, sep='')%>%
    mutate(metacognitive_asymmetry_from_sdt=(response1-response0))

  e$AUC <- e$AUC %>%
    merge(e$sdtAUC%>%dplyr::select(subj_id,detection,metacognitive_asymmetry_from_sdt)) %>%
    mutate(metacognitive_asymmetry_control = metacognitive_asymmetry-metacognitive_asymmetry_from_sdt)

  return(e)

};

testzROC <- function(df) {

  conf_bi_counts <- df %>%
    mutate(subj_id=factor(subj_id)) %>%
    group_by(subj_id, stimulus, conf_bi, .drop=FALSE) %>%
    tally() %>%
    spread(stimulus, n, sep='', fill=0) %>%
    arrange(desc(conf_bi), by_group=TRUE) %>%
    mutate(cs_1=cumsum(stimulus1)/sum(stimulus1),
           cs_0=cumsum(stimulus0)/sum(stimulus0)) %>%
    filter(cs_1 >0 & cs_1<1 & cs_0>0 & cs_0<1) %>%
    mutate(z_1 = qnorm(cs_1),
           z_0 = qnorm(cs_0)) %>%
    ungroup()

  zROC_slopes_tse <- conf_bi_counts%>%
    group_by(subj_id) %>%
    filter(n()>1) %>%
    do(model=odregress(.$z_0, .$z_1)) %>%
    rowwise() %>%
    mutate(slope = model$coeff[1],
           ssq = model$ssq,
           logslope = log(slope)) %>%
    dplyr::select('subj_id','slope','ssq','logslope') %>%
    filter(is.finite(logslope))
    
    
  zROC_slopes1 <- conf_bi_counts %>%
    group_by(subj_id) %>%
    do(model=lm(z_1~z_0,data=.)) %>%
    mutate(tidys=list(broom::tidy(model))) %>%
    unnest(tidys) %>%
    filter(term=='z_0')%>%
    mutate(slope1=estimate)%>%
    dplyr::select('subj_id','slope1') %>%
    drop_na();

  # To control for regression to the mean, fit the opposite model and average the two slopes
  # (see Wickens, p. 56)
  zROC_slopes2 <- conf_bi_counts %>%
    group_by(subj_id) %>%
    do(model=lm(z_0~z_1,data=.)) %>%
    mutate(tidys=list(broom::tidy(model))) %>%
    unnest(tidys) %>%
    filter(term=='z_1')%>%
    mutate(slope2=1/estimate)%>%
    dplyr::select('subj_id','slope2') %>%
    drop_na();

  zROC_fit <- conf_bi_counts %>%
    group_by(subj_id) %>%
    do(glance(lm(z_1~z_0,data=.))) %>%
    dplyr::select(subj_id,r.squared);

  zROC_slopes <- merge(
    zROC_slopes1 %>%
      dplyr::select(subj_id, slope1),
    zROC_slopes2 %>%
      dplyr::select(subj_id, slope2),
  ) %>%
    merge(zROC_fit) %>%
    rowwise()%>%
    mutate(
      logslope = log(slope1)/2+log(slope2)/2
    )

  return(zROC_slopes_tse)
}

testzROC2tasks <- function(e) {

  e$detection_zROC<- testzROC(e$detection_df);

  e$discrimination_zROC <- testzROC(e$discrimination_df);

  return(e)
}
