# Example preprocessing script.
id6<-cyber.security.6_enrolments%>%
  select(learner_id)
id7<-cyber.security.7_enrolments%>%
  select(learner_id)

uni_ids<-dplyr::union(id6,id7)
nrow(id7)
repeat_students<-intersect(id6, id7)

nrow(uni_ids)-sum( uni_ids$learner_id %in% repeat_students$learner_id)
uni_ids<-uni_ids[!uni_ids$learner_id %in% repeat_students$learner_id,]


nrow(uni_ids)
nrow(repeat_students)

pass_6<-cyber.security.6_enrolments$fully_participated_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()
pass_7<-cyber.security.7_enrolments$fully_participated_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()

pass_6_df<-data.frame(learner_id=cyber.security.6_enrolments$learner_id, pass_date=pass_6)
pass_7_df<-data.frame(learner_id=cyber.security.7_enrolments$learner_id, pass_date=pass_7)

nrow(uni_ids)
uni_ids<-left_join(uni_ids, union(pass_6_df, pass_7_df), by=c("learner_id"), copy=T)
nrow(uni_ids)

unenroll_6<-cyber.security.6_enrolments$unenrolled_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()
unenroll_7<-cyber.security.7_enrolments$unenrolled_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()
unenroll_df_6<-data.frame(learner_id=cyber.security.6_enrolments$learner_id, unenroll_date=unenroll_6)
unenroll_df_7<-data.frame(learner_id=cyber.security.7_enrolments$learner_id, unenroll_date=unenroll_7)
nrow(uni_ids)
uni_ids<-left_join(uni_ids, union(unenroll_df_6, unenroll_df_7), by=c("learner_id"), copy=T)
nrow(uni_ids)
uni_ids<-uni_ids %>%
mutate(pass=ifelse(!is.na(pass_date),T,ifelse(!is.na(unenroll_date), F, NA)))

hold<-cyber.security.6_question.response %>%
  select(learner_id, correct)

n_uni<-length(unique(hold$learner_id))
uniques_6<-unique(hold$learner_id)
mean_vector<-numeric(length=n_uni)
count_vector<-numeric(length=n_uni)
for (i in 1:n_uni){
  tmp<-filter(hold, learner_id==unique(hold$learner_id)[i])%>%
   select(correct)
  t_or_f<-ifelse(tmp=="true", T, ifelse(tmp=="false",F,NA))
  mean_vector[i]<-mean(t_or_f)
  count_vector[i]<-length(which(!is.na(t_or_f)))
}

hold<-cyber.security.7_question.response %>%
  select(learner_id, correct)
n_uni<-length(unique(hold$learner_id))
uniques_7<-unique(hold$learner_id)
mean_vector_7<-numeric(length=n_uni)
count_vector_7<-numeric(length=n_uni)
for (i in 1:n_uni){
  tmp<-filter(hold, learner_id==unique(hold$learner_id)[i])%>%
    select(correct)
  t_or_f<-ifelse(tmp=="true", T, ifelse(tmp=="false",F,NA))
  mean_vector_7[i]<-mean(t_or_f)
  count_vector_7[i]<-length(which(!is.na(t_or_f)))
}
h6<-data.frame(learner_id=uniques_6, mean=mean_vector, Q_count=count_vector)
h7<-data.frame(learner_id=uniques_7,mean=mean_vector_7, Q_count=count_vector_7)

uni_ids<-left_join(uni_ids, union(h6, h7), by=c("learner_id"))

hold<-union(cyber.security.6_step.activity, cyber.security.7_step.activity)
uniques_6_7<-unique(hold$learner_id)
hold$last_completed_at<- hold$last_completed_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()

tmp_vector<-numeric(length=length(uniques_6_7))
step_position<-numeric(length=length(uniques_6_7))
for_colour<-numeric(length = length(uniques_6_7))
end_date<-numeric(length = length(uniques_6_7))
for(i in 1:length(uniques_6_7)){
  tmp<-select(hold, learner_id, last_completed_at) %>%
    filter(learner_id==uniques_6_7[i]) %>%
    select(last_completed_at)
  if(sum(!is.na(tmp[,1]))>0){
    max_tmp<-which(hold$last_completed_at==max(tmp, na.rm = T) & hold$learner_id==uniques_6_7[i])
    tmp_vector[i]<-paste(hold$week_number[max_tmp], hold$step_number[max_tmp],sep=".")
    step_position[i]<-paste(letters[hold$week_number[max_tmp]], letters[hold$step_number[max_tmp]], sep="")
    for_colour[i]<-as.integer(hold$week_number[max_tmp])
    end_date[i]<-hold$last_completed_at[max_tmp]
    }
  else{tmp_vector[i]<-NA
  step_position[i]<-NA
  for_colour[i]<-NA
  end_date[i]<-NA}
  }
h6_7<-data.frame(learner_id=uniques_6_7, last_step_completed=tmp_vector, letter_code=step_position, week_number=for_colour, date_of_last=end_date )
uni_ids<-left_join(uni_ids, h6_7, by=c("learner_id"))

uni_ids<-uni_ids %>% arrange(letter_code)
uni_ids<-uni_ids %>%
mutate(question_score=mean*Q_count)
#To force uni_ids$last_step_completed into a particular order in graphs we set the levels
#after it has been arranged
uni_ids$last_step_completed<-factor(uni_ids$last_step_completed, levels = unique(uni_ids$last_step_completed))



general_leaving_reason<- left_join(uni_ids[,1], union(cyber.security.7_leaving.survey.responses, cyber.security.6_leaving.survey.responses), by=c("learner_id"))
reasons<-unique(general_leaving_reason$leaving_reason)
frequency<-sapply(reasons, function(i){length(which(general_leaving_reason$leaving_reason==i))})
reasons[2]<- "I don't have enough time"
reasons[4]<- "The course wasn't what I expected"
reasons[6]<- " The course won't help me reach my goals"
frequency<-as.numeric(frequency)
frequency[length(frequency)+1]<- sum(frequency)
general_leaving_reason<-arrange(data.frame(reasons=c(reasons, "Total"), frequency=frequency), frequency)
general_leaving_reason$reasons<-factor(general_leaving_reason$reasons, levels = general_leaving_reason$reasons)

