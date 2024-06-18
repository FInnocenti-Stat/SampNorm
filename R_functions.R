################################################################################
#############  R functions for sample size calculation in test norming
################################################################################


########## Univariate norming: Innocenti et al. (2023, Psychological Methods)

#### Determine the sample size given "Type I and Type II error rates and effect size" / "Confidence level and desired margin of error"

# (1) - Sample size for testing hypothesis about a Z-score under the optimal design, equation (14) in Psyc Met paper

# alpha= Type I error rate, gamma= Type II error rate, k= number of predictors in the norming model, 
# Zc= cut-off for classification = Z-score under H0, delta= effect size = Z-score under H0 - Z-score under H1

N_star_HypTest_Z<-function(alpha,gamma,k,Zc,delta){ 
  
  if(Zc<0){
    
    Nstar<-((qnorm(1-alpha)*sqrt(k+1+((Zc^2)/2))+qnorm(1-gamma)*sqrt(k+1+(((Zc-delta)^2)/2)))/(delta))^2 # Ha: Zt<Zc
    
  }else{
    
    Nstar<-((qnorm(1-alpha)*sqrt(k+1+((Zc^2)/2))+qnorm(1-gamma)*sqrt(k+1+(((Zc+delta)^2)/2)))/(delta))^2 # Ha: Zt>Zc
    
  }
  return(Nstar)
}

# (2) - Sample size for confidence interval about a Z-score under the optimal design, equation (16) in Psyc Met paper

# alpha= 1-confidence level, k= number of predictors in the norming model, Z0= expected Z-score, delta= desired margin of error

N_star_CI_Z<-function(alpha,k,Z0,delta){ 
  
    Nstar<-((qnorm(1-(alpha/2))*sqrt(k+1+((Z0^2)/2)))/(delta))^2 # Ha: Zt<Zc
  
  return(Nstar)
}

# (3) - Sample size for testing hypothesis about a Percentile Rank score (PR) under the optimal design, equation (15) in Psyc Met paper

# alpha= Type I error rate, gamma= Type II error rate, k= number of predictors in the norming model, 
# PRc= cut-off for classification = PR-score under H0, delta= effect size = PR-score under H0 - PR-score under H1
N_star_HypTest_PR<-function(alpha,gamma,k,PRc,delta){ 
  
  if(PRc<50){
    # Ha: PRt<PRc
    Nstar<-((qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(k+1+((qnorm(p=(PRc)/100))^2/2))+qnorm(1-gamma)*100*dnorm(qnorm(p=(PRc-delta)/100))*sqrt(k+1+((qnorm(p=(PRc-delta)/100))^2/2)))/delta)^2 
    
  }else{
    # Ha: PRt>PRc
    Nstar<-((qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(k+1+((qnorm(p=(PRc)/100))^2/2))+qnorm(1-gamma)*100*dnorm(qnorm(p=(PRc+delta)/100))*sqrt(k+1+((qnorm(p=(PRc+delta)/100))^2/2)))/delta)^2 
    
  }
  return(Nstar)
}

# (4) - Sample size for confidence interval about a PR-score under the optimal design, equation (17) in Psyc Met paper

# alpha= 1-confidence level, k= number of predictors in the norming model, PR0= expected PR-score, delta= desired margin of error

N_star_CI_PR<-function(alpha,k,PR0,delta){ 

Nstar<-((qnorm(1-(alpha/2))*100*dnorm(qnorm(p=(PR0)/100))*sqrt(k+1+((qnorm(p=(PR0)/100))^2/2)))/delta)^2 
    
  return(Nstar)
}

#### Determine the statistical power given "Type I error rate, effect size, and sample size"

# (5) - Power for testing hypothesis about a Z-score under the optimal design, rewriting of equation (14) in Psyc Met paper

# alpha= Type I error rate, N= sample size, k= number of predictors in the norming model, 
# Zc= cut-off for classification = Z-score under H0, delta= effect size = Z-score under H0 - Z-score under H1

Power_Z<-function(alpha,N,k,Zc,delta){ 
  
  if(Zc<0){
    
    Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*sqrt(k+1+((Zc^2)/2)))/sqrt(k+1+(((Zc-delta)^2)/2)),lower.tail = T) # Ha: Zt<Zc
    
  }else{
    
    Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*sqrt(k+1+((Zc^2)/2)))/sqrt(k+1+(((Zc+delta)^2)/2)),lower.tail = T) # Ha: Zt>Zc
    
  }
  return(Power)
}

# (6) - Power for testing hypothesis about a Percentile Rank score (PR) under the optimal design, rewriting of equation (15) in Psyc Met paper

# alpha= Type I error rate, N= sample size, k= number of predictors in the norming model, 
# PRc= cut-off for classification = PR-score under H0, delta= effect size = PR-score under H0 - PR-score under H1

Power_PR<-function(alpha,N,k,PRc,delta){ 
  
  if(PRc<50){
    # Ha: PRt<PRc
    Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(k+1+((qnorm(p=(PRc)/100))^2/2)))/100*dnorm(qnorm(p=(PRc-delta)/100))*sqrt(k+1+((qnorm(p=(PRc-delta)/100))^2/2))) 
    
  }else{
    # Ha: PRt>PRc
    Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(k+1+((qnorm(p=(PRc)/100))^2/2)))/100*dnorm(qnorm(p=(PRc+delta)/100))*sqrt(k+1+((qnorm(p=(PRc+delta)/100))^2/2))) 
  }
  return(Power)
}
#### Determine the precision (i.e. margin of error) given "Confidence level and sample size"

# (7) - Margin of error of a confidence interval for a Z-score under the optimal design, rewriting of equation (16) in Psyc Met paper

# alpha= 1-confidence level, k= number of predictors in the norming model, Z0= expected Z-score, N= sample size

Delta_CI_Z<-function(alpha,k,Z0,N){ 
  
  Delta<-qnorm(1-(alpha/2))*sqrt((k+1+((Z0^2)/2))/N)
  
  return(Delta)
}

# (8) - Margin of error of a confidence interval for a PR-score under the optimal design, rewriting of equation (17) in Psyc Met paper

# alpha= 1-confidence level, k= number of predictors in the norming model, PR0= expected PR-score, N= sample size

Delta_CI_PR<-function(alpha,k,PR0,N){ 
  
  Delta<-qnorm(1-(alpha/2))*100*dnorm(qnorm(p=(PR0)/100))*sqrt((k+1+((qnorm(p=(PR0)/100))^2/2))/N) 
  
  return(Delta)
}

########## Multivariate norming: Innocenti et al. (2023, JEBS)

# (9) -	Sample size for testing hypothesis about a Mahalanobis distance under the optimal design, equation (17) in JEBS paper

# alpha= Type I error rate, gamma= Type II error rate, k= number of predictors in the norming model, 
# Mahala_c= cut-off for classification = Mahalanobis-score under H0, delta_Mahala= effect size = Mahalanobis-score under H0 - Mahalanobis-score under H1

N_star_HypTest_Mahala<-function(alpha,gamma,k,Mahala_c,delta_Mahala){
  
  N<-((qnorm(1-alpha)*sqrt(k+1+((Mahala_c^2)/2))+qnorm(1-gamma)*sqrt(k+1+(((Mahala_c+delta_Mahala)^2)/2)))/delta_Mahala)^2
  return(N)
}

# (10) -	Sample size confidence interval for a Mahalanobis distance under the optimal design, equation (18) in JEBS paper:

# alpha= 1-confidence level, k= number of predictors in the norming model, Mahala_0= expected Mahalanobis-score, delta= desired margin of error

N_star_CI_Mahala<-function(alpha,k,Mahala_0,delta_Mahala){
  
  N<-((qnorm(1-(alpha/2))*sqrt(k+1+((Mahala_0^2)/2)))/delta_Mahala)^2
  return(N)
}

# (11) - Power for testing hypothesis about a Mahalanobis distance under the optimal design, rewriting of equation (17) in JEBS paper

# alpha= Type I error rate, gamma= Type II error rate, k= number of predictors in the norming model, 
# Mahala_c= cut-off for classification = Mahalanobis-score under H0, delta_Mahala= effect size = Mahalanobis-score under H0 - Mahalanobis-score under H1

Power_Mahala<-function(alpha,N,k,Mahala_c,delta_Mahala){ 
  
Power<-pnorm((delta_Mahala*sqrt(N)-qnorm(1-alpha)*sqrt(k+1+((Mahala_c^2)/2)))/sqrt(k+1+(((Mahala_c+delta_Mahala)^2)/2)),lower.tail = T) 
    
  return(Power)
}

# (12) - Margin of error of a confidence interval for a Mahalanobis distance under the optimal design, rewriting of equation (18) in JEBS paper

# alpha= 1-confidence level, k= number of predictors in the norming model, Mahala_0= expected Mahalanobis-score, N= sample size

Delta_CI_Mahala<-function(alpha,k,Mahala_0,N){ 
  
  Delta<-qnorm(1-(alpha/2))*sqrt((k+1+((Mahala_0^2)/2))/N)
  
  return(Delta)
}