#Perform Analysis and Create plots
#Figure 2 created removing students with incomplete last_step_completed information
bar_step<-ggplot(data=uni_ids[!is.na(uni_ids$last_step_completed),], aes(last_step_completed, fill=as.character(week_number)))
#Specified all the information in for the plot
stage_complete<-bar_step+ggtitle("Stage Completed Most Recently vs Number of Students") + labs(x="Stage Completed Most Recently", y="Number of Students")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_fill_discrete(name="Week:")+geom_bar()

#correlation matrix created for Table 1
correlation_matrix<-uni_ids[,c("pass", "question_score", "Q_count", "mean")]%>%
  as.matrix(nrow=nrow(uni_ids), ncol=3)%>%
  cor(use = "complete.obs", method="pearson")
corr_caption<-"A correlation matrix of the whether or somone fully participated (pass), the number of right questions they answered (question_score), the number of questions they answered (Q_count) and the number of questions they got right out of the number of questions they answered (mean)."

#Data processed for video data
t_video<-data.frame(t(cyber.security.7_video.stats[,9:15]))#dataframe containing percentage still watching
t_video$percent<-c(5,10,25,50,75,95,100)#percent created specifying the percentage watched
colnames(t_video)<-paste(c(rep("X",13), "percent"), c(1:13, ""), sep="")#renames columns
t_video<-pivot_longer(t_video, colnames(t_video)[-14])#pivots the data for graphing

#Figure 5 created and labels and title created
boxplot_graph<- ggplot(data=t_video, aes(x=percent, y=value))#coordinates created 
video_boxplot<-boxplot_graph+geom_boxplot(aes(group=percent))+ labs(title= "7th Dataset Video Stats Watchtime vs Students Still Watching Boxplot and \nLinear Regression",
                                                     x="Percentage of Video Watched- %",
                                                     y="Percentage still Viewing- %")+geom_smooth(method=lm, formula=y~x)
#finishes the graph

#joins repeat student learner ids to their enrollments in runs 6 and 7 respectively
first_attempt<-left_join(repeat_students,cyber.security.6_enrolments, by=c("learner_id"))
second_attempt<-left_join(repeat_students,cyber.security.7_enrolments, by=c("learner_id"))


#Creates statement specifiying how many Repeated their studies 
statmentm2<-paste(nrow(first_attempt), " Repeated their studies (total)")
#Specifies how many Completed the course twice
statementm1<-second_attempt$learner_id[second_attempt$fully_participated_at!=""] %in% first_attempt$learner_id[first_attempt$fully_participated_at!=""] %>%
  sum() %>%
  paste(" Completed the course twice") 
#Specifies how many Unenrolled in their first and second attempt
statment0<-second_attempt$learner_id[second_attempt$unenrolled_at!=""] %in% first_attempt$learner_id[first_attempt$unenrolled_at!=""] %>%
  sum() %>%
  paste(" Unenrolled in their first and second attempt", sep="")
#Specifies how many fully participated the first time
statement1<-paste(sum(first_attempt$fully_participated_at!=""), "fully participated the first time", sep=" ")
#Specifies how many fully participated the second time
statment2<-sum(second_attempt$fully_participated_at!="") %>%
  paste("fully participated the second time", sep=" ")
#Specifies how many unenrolled the first time
statment3<-sum(first_attempt$unenrolled_at!="")%>%
  paste("unenrolled the first time", sep= " ")
#Specifies how many unenrolled the second time
statment4<-sum(second_attempt$unenrolled_at!="") %>%
  paste("unenrolled the second time", sep= " ")
#Specifies how many Assumed did not finish the course the first time
statement5<-sum(first_attempt$unenrolled_at=="") %>%
  paste("Assumed did not finish the course the first time", sep= " ")
#Specifies how many Assumed did not finish the course the second time
statment6<-sum(second_attempt$unenrolled_at=="") %>%
  paste("Assumed did not finish the course the second time", sep= " ")
#we find the percentage of people who as finished the course (/100)
store_score<-mean(ifelse(is.na(uni_ids$pass)| uni_ids$pass==F, 0,1 )) %>%
  round(4)
#percentage of people who finished the course created into statement and multiplied by 100
statment7<-paste(store_score*100,"% of the non repeat students finished the course", sep="")
#percentage of people who unenrolled from the course
unenroll_score<-mean(ifelse(uni_ids$unenroll_date==""|is.na(uni_ids$unenroll_date), 0,1 )) %>%
  round(4)
#creates statement out of people who unenrolled from the course
statment8<-paste(unenroll_score*100, "% of the non repeat students unenrolled from the course", sep="" )
#Number of people who purchased twice
duplicate_purchases<-sum(first_attempt$learner_id[first_attempt$purchased_statement_at!=""] %in% second_attempt$learner_id[second_attempt$purchased_statement_at!=""])
#determines whole data purchases
purchases<-(sum(cyber.security.7_enrolments$purchased_statement_at!="")+sum(cyber.security.6_enrolments$purchased_statement_at!=""))%>%
  paste("Purchases of a certificate including", duplicate_purchases, "repeat students who bought twice", sep=" ")

#We remove total from the bar chart as it is not relevant, NA has no occurrences due to
#the way we made this dataset so it can be ignored as well
total<-which(general_leaving_reason[,1]=="Total"| is.na(general_leaving_reason[,1]))#total position determined from general leaving reason
#creates ggplot and graph total is removed for plotting Figure 6
bar_step_reason<-ggplot(data=general_leaving_reason[-total,], aes(reasons, frequency, fill=reasons))
single_reason<-bar_step_reason+scale_fill_brewer(palette = "Blues")+ theme(legend.position = "none",axis.text.x = element_text(debug = NULL))+ geom_col()+coord_flip()+ labs(title="Single Attempt Students Reasons for Leaving" ,x="Reasons", y="Frequency")

#creates ggplot and graph Figure 7
new_boxplot_graph<- ggplot(data=uni_ids[!is.na(uni_ids$retention_time_days) & !is.na(uni_ids$country),], aes(x=ifelse(country=="GB", "Great Britain", "International"), y=retention_time_days))
student_country_plot<-new_boxplot_graph+geom_boxplot(aes(group= ifelse(country=="GB", "Great Britain", "International")))+labs(title="Boxplots For Retention Time Split by student Inter or Intra Nationality",y="Retention Time in Days", x="Student detected to be in Great Britain or International")
#length(which(ifelse(uni_ids$country=="GB", "Great Britain", "International")=="Great Britain" & !is.na(uni_ids$country)))
#length(which(ifelse(uni_ids$country=="GB", "Great Britain",
#"International")=="International" & !is.na(uni_ids$country)))
#Commented out code to find the sample sizes

#boolean purchase variable created (Has the student purchased the certificate)
uni_ids$purchase_TF<-ifelse(is.na(uni_ids$purchase), F, T)
#number of students who purchased and fully participated 
purchase_and_pass<-length(which(uni_ids$purchase_TF==T & uni_ids$pass==T))
#number of students who purchased and did not fully participated 
purchase_not_pass<-length(which(uni_ids$purchase_TF==T & uni_ids$pass==F))
#number of students who purchased and fully participated is NA 
purchase_na<-length(which(uni_ids$purchase_TF==T & is.na(uni_ids$pass)))
#the number of people who purchased and were retained for 21 or more days
twenty_one_days<-length(which(uni_ids$purchase_TF==T & uni_ids$retention_time_days>=21))
#number of people who purchased and did not pass determined including NA
purchase_not_pass<- purchase_na+purchase_not_pass
#total purchases is determined
total_purchases<-length(which(uni_ids$purchase_TF==T))
#bar data dataframe created out of all the purchase information captured
bar_data<-data.frame(purchased=c(purchase_and_pass, purchase_not_pass, twenty_one_days,total_purchases), subsection=c("Fully Participated", "Incomplete Participation", "Retained for 21+ days", "Total Purchases"))
bar_data<-arrange(bar_data, purchased)#arranged by purchase number
#sets explicit levels for bar_data
bar_data$subsection<-factor(bar_data$subsection, levels=bar_data$subsection)
plot_bar<-ggplot(data=bar_data, aes(subsection,purchased))#ggplt created
#graph created for Figure 9
purchase_bar<-plot_bar + geom_col(fill="#ADD8E6") + theme(legend.position = "none")+geom_text(aes(label = purchased), vjust = 2)+labs(title="Groups by Certificate purchases", x="Groups", y="Number of Certificate Purchases")

#Logisitic regressions to show increase in odds ratio of buying certificate

fully_p<-glm(purchase_TF~na_pass, data = uni_ids, family="binomial")#logistic regression for all that fully participated
retention_time<-glm(purchase_TF~retention_time_days, data = uni_ids, family="binomial")#logistic regression against time retained in days (only Boolean version is shown)
retention_TF<-glm(purchase_TF~retention_21, data = uni_ids, family="binomial")#logistic regression using Boolean retention (greater or equal to 21 days)
fully_paticipated_p<-logit_link(fully_p)[1]#Probability determined using the logit_link function found in the helpers.R file to get teh p value for the coeficients from the first
DNF_p<-logit_link(fully_p)[2]#probability determined for those who did not fully participate
retained_21_plus<-logit_link(retention_TF)[1]#probability determined for all those who were retained for 21 days or more
retained_short<-logit_link(retention_TF)[2]#probability determined for all those who were retained less than 21 days

#probs_data created for the creation of Table 5
probs_data<- data.frame(Condition=c("Fully Participated", "Retained >= 21 days"), 'Probability When Not'=c(DNF_p, retained_short), 'Probability Given'=c(fully_paticipated_p, retained_21_plus))
colnames(probs_data)<- c("Condition", "Probability When Not", "Probability Given Conditon")#sets column names
#Barchart of steps completed Figure 10
steps_completed_chart_ggplot<-ggplot(data= step_completions, aes(names.completions.))##ggplot
steps_completed_chart<-steps_completed_chart_ggplot + labs(title="Students who Started and Students who Finished Each Step", x="Step Number", y="Number of Students") + geom_col(alpha=0.2, aes(y=started, fill=as.character(week)), colour="#082321")+geom_col(alpha=1,aes(y=completions, fill=as.character(week)), width=0.5)+ scale_fill_discrete(name="Week:")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))#graph created 

#Language boxplot Figure 11
#These countries were determined to speak English as a first language in the majority.
maj_ENG_speak<-c("GB","NZ", "US", "AU", "CA" ,"IE")
#Figure 11 created
language_boxplot_graph<- ggplot(data=uni_ids[!is.na(uni_ids$retention_time_days) & !is.na(uni_ids$country),], aes(x=ifelse(country %in% maj_ENG_speak, "Majority Speak English as 1st Language ", "Other Languages Spoken 1st"), y=retention_time_days))
student_language_plot<-language_boxplot_graph+geom_boxplot(aes(group= ifelse(country %in% maj_ENG_speak, "Majority Speak English as 1st Language ", "Other Languages Spoken 1st")))+labs(title="Retention Time by Whether the Student's Detected Country \nPrimarily Speaks English", x="Groups", y="Retention Time in Days")

#Remove countries that had less than 25 students as their boxplots are misleading for Figure 12
country_positions_to_consider<-which(uni_ids$country%in%country_retention[country_retention$n>=25,"country_name"] & !is.na(uni_ids$retention_time_days))
language_boxplot_graph_25<- ggplot(data=uni_ids[country_positions_to_consider,], aes(x=country, y=retention_time_days, fill=(ifelse(country %in% maj_ENG_speak,1,"none"))))
student_language_plot_25<-language_boxplot_graph_25+geom_boxplot(aes(country, retention_time_days))+ theme(legend.position = "none")+labs(title="Retention Time by Student's Detected Country", x="Detected Country", y="Retention Time in Days")
#Figure 13
plot_countries<-ggplot(data = country_retention[country_retention$n_All>25,], aes(country_name, y=n_All))+geom_col()+ coord_flip()+labs(title="Number of Students Enrolled by Country", y="Number of Students Enrolled", x="Detected Country")

#+geom_text(data = country_retention, aes(x=country_name, label = n), position = position_dodge(width = .75),show.legend = FALSE)

#Rubbish REMOVE LATER
#ggplot_country_data<-ggplot(country_retention,aes(country_name,median,fill=(ifelse(country_name %in% maj_ENG_speak,1,0))))
#ggplot_country_data+ geom_col()+ theme(legend.position = "none")+ geom_errorbar(data=country_retention,aes(x=country_name, ymin=LQ, ymax=UQ))

#language_boxplot_graph<- ggplot(data=uni_ids[!is.na(uni_ids$retention_time_days) & !is.na(uni_ids$country),], aes(country, y=retention_time_days,fill=(ifelse(country %in% maj_ENG_speak,1,"none"))))
#student_language_plot<-language_boxplot_graph+geom_boxplot()+ theme(legend.position = "none")+ coord_flip()+theme(axis.text=element_text(size=6))
#student_language_plot

#odds_increase<-c(exp(coef(retention_time)[2]), exp(coef(retention_time)[2]))
#odds_increase<-odds_increase-1
#odds_increase<-odds_increase*100
#odds_increase<-paste(round(odds_increase,2), c("%", "%"), sep="")
