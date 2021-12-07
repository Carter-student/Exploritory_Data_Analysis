#Figure 7 created removing those that did not have a retention time in days
new_boxplot_graph<- ggplot(data=uni_ids[!is.na(uni_ids$retention_time_days),], aes(x=ifelse(is.na(pass), F, pass), y=retention_time_days))
pass_graph<-new_boxplot_graph+geom_boxplot(aes(group=ifelse(is.na(pass), F, pass)))+ labs(title="Boxplot of Retention Time Split by Student Participation", x="Fully Participated?", y="Retention time in Days")

#length(which(ifelse(is.na(uni_ids$pass), F, uni_ids$pass==T)==F & !is.na(uni_ids$retention_time_days)))
#length(which(ifelse(is.na(uni_ids$pass), F, uni_ids$pass==T)==T & !is.na(uni_ids$retention_time_days)))
#commented out code to find sample sizes