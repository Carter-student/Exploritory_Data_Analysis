logit_link <- function(model)
{
  ln_p<-coef(model)[1]+coef(model)[2]
  prob_given_coef<-exp(ln_p)/(1+exp(ln_p))
  p_no<-exp(coef(model)[1])
  prob_given_no_coef<-p_no/(p_no+1)
  
  return(c(prob_given_coef, prob_given_no_coef))
}
