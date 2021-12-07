logit_link <- function(model)#logit link function
{
  ln_p<-coef(model)[1]+coef(model)[2]#take the intercept and coefficient
  prob_given_coef<-exp(ln_p)/(1+exp(ln_p))#exp(p)/exp(p)+1 logit link function for positive
  p_no<-exp(coef(model)[1])#take the intercept without the coefficient
  prob_given_no_coef<-p_no/(p_no+1)#exp(p)/exp(p)+1 logit link function for negative
  
  return(c(prob_given_coef, prob_given_no_coef))#return values as vector
}
