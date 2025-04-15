## R function to compute the worst-case scenario variance under a sub-optimal design

MaxPredVar<-function(model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, q_edu, g_edu){
  
  args<-match.call()  
  ### Predictors 
  # Age: Always assumed in the model
  if(all(Age_levels=="Equidistant")){
    Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
  }else if(all(Age_levels=="Uniform")){
    Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
  }else{Age<-Age_levels}  # Any levels
  # Sex
  if("q_sex" %in% names(args)){
    Sex<-1:q_sex-1
  } else {Sex<-1} # No sex 
  #Education
  if("q_edu" %in% names(args)){ #categorical   
    Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
      if(all(Edu_levels=="Equidistant")){Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
      else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
      else {Education<-Edu_levels}  # Any levels
    } else{Education<-1} #No education
  
  ### Sub-optimal Design: all possible combinations of predictors levels with equal design weights
  data<-expand.grid(Age,Sex,Education) 
  names(data)<-c("Age","Sex","Education")  
  
  ### Model:  
  X_sub<-model.matrix(object=model,data=data) 
  MAX_predvar_sub<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))
  
  Output<-list(MAX_predvar_sub)
  names(Output)<-c("Maximum of the standardized prediction variance over the design region")
  return(Output)
}


## Example:
MaxPredVar(model=~Age+I(Age^2)+factor(Education)+Sex, min_age=18, max_age=64,Age_levels=c("Uniform"), q_sex=2, q_edu=3)
