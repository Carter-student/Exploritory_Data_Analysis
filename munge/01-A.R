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

pass_6_df<-data.frame(learner_id=cyber.security.6_enrolments$learner_id, pass_date=pass_6, purchase=as.POSIXct(cyber.security.6_enrolments$purchased_statement_at,format="%Y-%m-%d %H:%M:%S"))
pass_7_df<-data.frame(learner_id=cyber.security.7_enrolments$learner_id, pass_date=pass_7, purchase=as.POSIXct(cyber.security.7_enrolments$purchased_statement_at,format="%Y-%m-%d %H:%M:%S"))

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

hold<- union(cyber.security.7_step.activity, cyber.security.6_step.activity)
hold<-mutate(hold, real_step= paste(week_number, step_number, sep="."))
hold$TF_complete<- ifelse(hold$last_completed_at=="",F, T)
completions<-sapply(unique(hold$real_step), function(i){sum(filter(hold, real_step==i)$TF_complete)})
started<-sapply(unique(hold$real_step), function(i){length(filter(hold, real_step==i)$TF_complete)})

hold$real_step<-factor(hold$real_step, levels = unique(uni_ids$last_step_completed))
step_completions<-data.frame(names(completions), completions)
row.names(step_completions)<-NULL
#the below helps order the steps when plotting by designating explicit levels
step_completions$names.completions.<-factor(step_completions$names.completions., levels=step_completions$names.completions.)
step_completions$week<-ifelse(substr(step_completions$names.completions.,1,1)=="1", 1, ifelse(substr(step_completions$names.completions.,1,1)=="2",2,3))

step_completions$started<-started


another<-uni_ids%>%
  left_join(union(cyber.security.6_enrolments, cyber.security.7_enrolments), by=c("learner_id"))
if(mean(uni_ids$learner_id==another$learner_id)==1){
  uni_ids$retention_time_days<-(uni_ids$date_of_last/86400)-(as.numeric(as.POSIXct(another$enrolled_at ,format="%Y-%m-%d %H:%M:%S"))/86400)} else(print("INVALID LEARNER ID ORDER"))# A quality control mechanism to ensure the learner IDs line up in each dataset
new_boxplot_graph<- ggplot(data=uni_ids[!is.na(uni_ids$retention_time_days),], aes(x=ifelse(is.na(pass), F, pass), y=retention_time_days))
pass_graph<-new_boxplot_graph+geom_boxplot(aes(group=ifelse(is.na(pass), F, pass)))
#length(which(ifelse(is.na(uni_ids$pass), F, uni_ids$pass==T)==F & !is.na(uni_ids$retention_time_days)))
#length(which(ifelse(is.na(uni_ids$pass), F, uni_ids$pass==T)==T & !is.na(uni_ids$retention_time_days)))
#commented out code to find sample sizes

uni_ids$country<-another$detected_country
uni_ids$country<-ifelse(uni_ids$country=="--", NA, uni_ids$country)

#Retention by detected country needs to be created
#filter out NA and "--" from the country data we have already and find unique countries
country_list<-unique(uni_ids$country)[which(!is.na(unique(uni_ids$country)) & unique(uni_ids$country)!="--")]
#find the number of students from each country that we have a retention time for 
retention_time<-sapply(country_list, function(i){
  x<-filter(uni_ids, country==i & !is.na(retention_time_days))$retention_time_days
  length(x)})
retention_time_students<-sapply(country_list, function(i){
  x<-filter(uni_ids, country==i )$retention_time_days
  length(x)})
#feeds data from sapply function into dataframe 
country_retention<-data.frame(country_name=country_list, n=retention_time, n_All=retention_time_students )





#RUBBISH REMOVE IF NOT USEFUL
#retention_time<-sapply(country_list, function(i){
#  x<-filter(uni_ids, country==i & !is.na(retention_time_days))$retention_time_days
#  c(median(x), quantile(x)[2], quantile(x)[4], length(x))})
#country_retention<-data.frame(country_name=country_list, median=retention_time[seq(1,length(country_list)*4, 4)], LQ=retention_time[seq(2,length(country_list)*4, 4)], UQ=retention_time[seq(3,length(country_list)*4, 4)], n=retention_time[seq(4,length(country_list)*4, 4)] )
#country_retention<-country_retention[complete.cases(country_retention),]