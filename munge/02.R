library(ProjectTemplate)
#load.project()
#tmp<-inner_join(cyber.security.1_enrolments,cyber.security.1_question.response, by=c("learner_id"))
steps<-cyber.security.1_step.activity

names_to_change<-colnames(steps)
names_to_change[2]<-"step_precise"#step is a function in R and so is harder to manipulate
colnames(steps)<-names_to_change
steps$step_precise<-as.character(steps$step_precise)
unique(steps$step_precise)
steps$step_precise<-ifelse(steps$step_number==10 & steps$week_number==1,"1.10",steps$step_precise )
steps$step_precise<-ifelse(steps$step_number==10 & steps$week_number==2,"2.10",steps$step_precise )

steps$step_precise<-ifelse(steps$step_number==10 & steps$week_number==3,"3.10",steps$step_precise )
steps$step_precise<-ifelse(steps$step_number==20 & steps$week_number==2,"2.20",steps$step_precise )
steps$step_precise<-ifelse(steps$step_number==20 & steps$week_number==3,"3.20",steps$step_precise )

steps$last_completed_at<-as.numeric(as.POSIXct(steps$last_completed_at, format="%Y-%m-%d %H:%M:%S"))/86400
steps$first_visited_at<-as.numeric(as.POSIXct(steps$first_visited_at, format="%Y-%m-%d %H:%M:%S"))/86400

time_taken<-data.frame(learner_id=unique(steps$learner_id))
for (i in unique(steps$step_precise)){
  time_taken<- left_join(time_taken, filter(steps, step_precise==i)[,c("learner_id","first_visited_at","last_completed_at")], by=c("learner_id"))
  print(dim(time_taken))
  print(i)
  }

dim(time_taken)
unique_length<-length(unique(steps$step_precise))
new_time<-time_taken[,seq(4, 121,2)]-time_taken[,seq(3,120,2)]
dim(new_time)

variance<-apply(new_time, MARGIN=1, function(i){var(i,na.rm=T)})
mean_record<-apply(new_time, MARGIN=1, function(i){mean(i,na.rm=T)})
length(variance)
id_variance<-data.frame(learner_id=unique(time_taken[,"learner_id"]), Var_reported=variance, mean= mean_record)
enrolment<-cyber.security.1_enrolments
enrolment$fully_participated_at<-ifelse(cyber.security.1_enrolments$fully_participated_at=="",0,1)
id_variance<-id_variance %>%
  left_join(enrolment[,c("learner_id", "fully_participated_at")], by=c("learner_id") )
View(id_variance)
#id_variance$Var_reported<-ifelse(is.na(id_variance$Var_reported),-500, id_variance$Var_reported)
id_variance$ind<-id_variance$Var_reported/id_variance$mean

hist(id_variance$ind[id_variance$fully_participated_at==1], breaks=100, ylim=c(0,30))
hist(id_variance$ind[id_variance$fully_participated_at==0], breaks=100, ylim=c(0,30))

plot(id_variance$ind, id_variance$fully_participated_at)
summary(id_variance$Var_reported[id_variance$fully_participated_at==1])
summary(id_variance$Var_reported[id_variance$fully_participated_at==0])
hist(id_variance$Var_reported[id_variance$fully_participated_at==1])
hist(id_variance$Var_reported[id_variance$fully_participated_at==0])
boxplot(id_variance$Var_reported[id_variance$fully_participated_at==1])
boxplot(id_variance$Var_reported[id_variance$fully_participated_at==0])
