#preprocessing script
#we select the learner ids from run 6 and run 7 into id6 and id 7 respectively
id6<-cyber.security.6_enrolments%>%
  select(learner_id)
id7<-cyber.security.7_enrolments%>%
  select(learner_id)

uni_ids<-dplyr::union(id6,id7)#combines rows from id6 and id7
repeat_students<-intersect(id6, id7)#repeat students identified

nrow(uni_ids)-sum( uni_ids$learner_id %in% repeat_students$learner_id)#a quality check in creation
uni_ids<-uni_ids[!uni_ids$learner_id %in% repeat_students$learner_id,]#We remove repeat students


nrow(uni_ids)#quality check we compare to previous to ensure they are the same number
nrow(repeat_students)#quality check 

#Dates for fully participated is converted to a number for both runs
pass_6<-cyber.security.6_enrolments$fully_participated_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()
pass_7<-cyber.security.7_enrolments$fully_participated_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()
#Dates are recombined in a dataframe with their learner ids so we can go back to uni_ids
pass_6_df<-data.frame(learner_id=cyber.security.6_enrolments$learner_id, pass_date=pass_6, purchase=as.POSIXct(cyber.security.6_enrolments$purchased_statement_at,format="%Y-%m-%d %H:%M:%S"))
pass_7_df<-data.frame(learner_id=cyber.security.7_enrolments$learner_id, pass_date=pass_7, purchase=as.POSIXct(cyber.security.7_enrolments$purchased_statement_at,format="%Y-%m-%d %H:%M:%S"))

#uni_ids joined to dates of full participation
uni_ids<-left_join(uni_ids, union(pass_6_df, pass_7_df), by=c("learner_id"), copy=T)

#unenroll dates converted to numeric for run 6 and 7
unenroll_6<-cyber.security.6_enrolments$unenrolled_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()
unenroll_7<-cyber.security.7_enrolments$unenrolled_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()

#unenroll  dates are recombined in a dataframe with their learner ids so we can go back to uni_ids
unenroll_df_6<-data.frame(learner_id=cyber.security.6_enrolments$learner_id, unenroll_date=unenroll_6)
unenroll_df_7<-data.frame(learner_id=cyber.security.7_enrolments$learner_id, unenroll_date=unenroll_7)

#unenroll dates joined to uni_ids
uni_ids<-left_join(uni_ids, union(unenroll_df_6, unenroll_df_7), by=c("learner_id"), copy=T)

#Boolean fully participated created in pass column NA is left as NA
uni_ids<-uni_ids %>%
mutate(pass=ifelse(!is.na(pass_date),T,ifelse(!is.na(unenroll_date), F, NA)))

#temporary dataframe holding question response learner_id and correct columns
hold<-cyber.security.6_question.response %>%
  select(learner_id, correct)

#the number of unique learner_ids in hold
n_uni<-length(unique(hold$learner_id))
#the unique learner ids in uniques_6
uniques_6<-unique(hold$learner_id)
#mean_vector is created with length of the number of unique learner_ids
mean_vector<-numeric(length=n_uni)
#count_vector is created with length of the number of unique learner_ids
count_vector<-numeric(length=n_uni)
for (i in 1:n_uni){#for loop determines the percentage of correct answers to number of answers for each learner_id as well as number of questions answered run 6
  tmp<-filter(hold, learner_id==unique(hold$learner_id)[i])%>%
   select(correct)
  t_or_f<-ifelse(tmp=="true", T, ifelse(tmp=="false",F,NA))
  mean_vector[i]<-mean(t_or_f)
  count_vector[i]<-length(which(!is.na(t_or_f)))
}
#temporary dataframe holding question response learner_id and correct columns for run 7
hold<-cyber.security.7_question.response %>%
  select(learner_id, correct)
#the number of unique learner_ids in hold
n_uni<-length(unique(hold$learner_id))
#the unique learner ids in uniques_7
uniques_7<-unique(hold$learner_id)
#mean_vector_7 is created with length of the number of unique learner_ids
mean_vector_7<-numeric(length=n_uni)
#count_vector_7 is created with length of the number of unique learner_ids
count_vector_7<-numeric(length=n_uni)
#for loop determines the percentage of correct answers to number of answers for each learner_id as well as number of questions answered run 7
for (i in 1:n_uni){
  tmp<-filter(hold, learner_id==unique(hold$learner_id)[i])%>%
    select(correct)
  t_or_f<-ifelse(tmp=="true", T, ifelse(tmp=="false",F,NA))
  mean_vector_7[i]<-mean(t_or_f)
  count_vector_7[i]<-length(which(!is.na(t_or_f)))
}
#dataframes h6 and h7 store the accuracy and number of questions answered from for loops
h6<-data.frame(learner_id=uniques_6, mean=mean_vector, Q_count=count_vector)
h7<-data.frame(learner_id=uniques_7,mean=mean_vector_7, Q_count=count_vector_7)

#h6 and h7 join uni_ids
uni_ids<-left_join(uni_ids, union(h6, h7), by=c("learner_id"))

#temporary dataframe holding step activity in run 6 and run 7
hold<-union(cyber.security.6_step.activity, cyber.security.7_step.activity)
#unique learner ids determined
uniques_6_7<-unique(hold$learner_id)
#last_completed_at date converted to numeric
hold$last_completed_at<- hold$last_completed_at %>%
  as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>%
  as.numeric()

#4 vectors initialized with the length of uniques_6_7
tmp_vector<-numeric(length=length(uniques_6_7))
step_position<-numeric(length=length(uniques_6_7))
for_colour<-numeric(length = length(uniques_6_7))
end_date<-numeric(length = length(uniques_6_7))
#for loop for the length of the unique learner ids
for(i in 1:length(uniques_6_7)){
  tmp<-select(hold, learner_id, last_completed_at) %>%
    filter(learner_id==uniques_6_7[i]) %>%
    select(last_completed_at)#tmp dataframe selects all data referring to a given learner_id (indexed by i)
  if(sum(!is.na(tmp[,1]))>0){#checks that there is at least one step completed
    #max_tmp becomes the maximum date (the most recent date) position in the larger hold dataframe hold
    max_tmp<-which(hold$last_completed_at==max(tmp, na.rm = T) & hold$learner_id==uniques_6_7[i])
    #creates a step number using week number and a step number as the given step number considers 1.1 and 1.10 as the same as they are considered floats
    tmp_vector[i]<-paste(hold$week_number[max_tmp], hold$step_number[max_tmp],sep=".")
    #uses the letters (alphabet) to create a sortable vector of letter_codes which refer to step positons
    step_position[i]<-paste(letters[hold$week_number[max_tmp]], letters[hold$step_number[max_tmp]], sep="")
    #week is added to for_colour vector
    for_colour[i]<-as.integer(hold$week_number[max_tmp])
    #The most recent date completed added to end_date vector
    end_date[i]<-hold$last_completed_at[max_tmp]
    }
  else{tmp_vector[i]<-NA#if no steps completed set all vectors at position i to NA
  step_position[i]<-NA
  for_colour[i]<-NA
  end_date[i]<-NA}
}
#vectors collected from for loop are added to a dataframe
h6_7<-data.frame(learner_id=uniques_6_7, last_step_completed=tmp_vector, letter_code=step_position, week_number=for_colour, date_of_last=end_date )
#dataframe added to uni_ids
uni_ids<-left_join(uni_ids, h6_7, by=c("learner_id"))

#uni_idsis arranged by letter_code
uni_ids<-uni_ids %>% arrange(letter_code)
#The number of questions answered calculated
uni_ids<-uni_ids %>%
mutate(question_score=mean*Q_count)
#To force uni_ids$last_step_completed into a particular order in graphs we set the levels
#after it has been arranged
uni_ids$last_step_completed<-factor(uni_ids$last_step_completed, levels = unique(uni_ids$last_step_completed))


#leaving survey responses added from run 6 and 7
general_leaving_reason<- left_join(uni_ids[,1], union(cyber.security.7_leaving.survey.responses, cyber.security.6_leaving.survey.responses), by=c("learner_id"))
#unique leaving reasons
reasons<-unique(general_leaving_reason$leaving_reason)
#frequency for each leaving reason determined
frequency<-sapply(reasons, function(i){length(which(general_leaving_reason$leaving_reason==i))})
#correction of apostrophe error 
reasons[2]<- "I don't have enough time"
reasons[4]<- "The course wasn't what I expected"
reasons[6]<- " The course won't help me reach my goals"
#set frquency as numeric
frequency<-as.numeric(frequency)
#Adds a total to frequency
frequency[length(frequency)+1]<- sum(frequency)
#creates and arranges dataframe by frequency for leaving reasons 
general_leaving_reason<-arrange(data.frame(reasons=c(reasons, "Total"), frequency=frequency), frequency)
#sets explicit levels for colouring and ordering reasons
general_leaving_reason$reasons<-factor(general_leaving_reason$reasons, levels = general_leaving_reason$reasons)

#temp dataframe with step ativity from run 6 and 7
hold<- union(cyber.security.7_step.activity, cyber.security.6_step.activity)
#real_step column added with a paste of week and step number
hold<-mutate(hold, real_step= paste(week_number, step_number, sep="."))
#TF_complete is a boolean column of if the last complete is empty or has a date
hold$TF_complete<- ifelse(hold$last_completed_at=="",F, T)
#completions is each steps frequency if completed
completions<-sapply(unique(hold$real_step), function(i){sum(filter(hold, real_step==i)$TF_complete)})
#started is each steps frequency if started
started<-sapply(unique(hold$real_step), function(i){length(filter(hold, real_step==i)$TF_complete)})

#explicit level created at the levels
hold$real_step<-factor(hold$real_step, levels = unique(uni_ids$last_step_completed))
##dataframe of completions
step_completions<-data.frame(names(completions), completions)
#removes names associated with step_completions
row.names(step_completions)<-NULL
#the below helps order the steps when plotting by designating explicit levels
step_completions$names.completions.<-factor(step_completions$names.completions., levels=step_completions$names.completions.)
step_completions$week<-ifelse(substr(step_completions$names.completions.,1,1)=="1", 1, ifelse(substr(step_completions$names.completions.,1,1)=="2",2,3))
#adds the number of people that started that step to the step names
step_completions$started<-started

#another a temporary dataframe joins enrollments information by learner ids
another<-uni_ids%>%
  left_join(union(cyber.security.6_enrolments, cyber.security.7_enrolments), by=c("learner_id"))
#checks that another and uni_ids learner ids line up 100%
if(mean(uni_ids$learner_id==another$learner_id)==1){
  #retention time in days creaeted by finding the difference from the last completed and the enrollment divided by 56400
  uni_ids$retention_time_days<-(uni_ids$date_of_last/86400)-(as.numeric(as.POSIXct(another$enrolled_at ,format="%Y-%m-%d %H:%M:%S"))/86400)} else(print("INVALID LEARNER ID ORDER"))# A quality control mechanism to ensure the learner IDs line up in each dataset

#country is detectied country
uni_ids$country<-another$detected_country
#specifies -- is NA
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

#retention infromation for logistic regression
uni_ids$na_pass<-ifelse(is.na(uni_ids$pass), F, uni_ids$pass)
uni_ids<-mutate(uni_ids, retention_21= ifelse(retention_time_days>=21,1,0))




#RUBBISH REMOVE IF NOT USEFUL
#retention_time<-sapply(country_list, function(i){
#  x<-filter(uni_ids, country==i & !is.na(retention_time_days))$retention_time_days
#  c(median(x), quantile(x)[2], quantile(x)[4], length(x))})
#country_retention<-data.frame(country_name=country_list, median=retention_time[seq(1,length(country_list)*4, 4)], LQ=retention_time[seq(2,length(country_list)*4, 4)], UQ=retention_time[seq(3,length(country_list)*4, 4)], n=retention_time[seq(4,length(country_list)*4, 4)] )
#country_retention<-country_retention[complete.cases(country_retention),]