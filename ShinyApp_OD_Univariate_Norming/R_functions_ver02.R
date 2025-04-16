###############################################################################################################################
#####################  R functions for sample size calculation in test norming under any design    ###########################
###############################################################################################################################

#################### Z-score: Sample size calculation formulas for different goals under any design ###########################

# (1) - Sample size for hypothesis testing
N_star_HypTest_Z<-function(alpha,gamma,Zc,delta, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}

### Sample size calculation    
if(Zc<0){
    
Nstar<-((qnorm(1-alpha)*sqrt(d_Xxi+((Zc^2)/2))+qnorm(1-gamma)*sqrt(d_Xxi+(((Zc-delta)^2)/2)))/(delta))^2 # Ha: Zt<Zc
    
}else{
    
Nstar<-((qnorm(1-alpha)*sqrt(d_Xxi+((Zc^2)/2))+qnorm(1-gamma)*sqrt(d_Xxi+(((Zc+delta)^2)/2)))/(delta))^2 # Ha: Zt>Zc
    
}
return(Nstar)
}

# (2) - Sample size for interval estimation
N_star_CI_Z<-function(alpha,Z0,delta, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Sample size calculation      
Nstar<-((qnorm(1-(alpha/2))*sqrt(d_Xxi+((Z0^2)/2)))/(delta))^2 
  
return(Nstar)
}

# (3) - Sample size for interval estimation with assurance probability 1-beta
Samp_Z_AP<-function(Z0,alpha,tau,beta, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Sample size calculation  
f_Z0<-d_Xxi+((Z0^2)/2)
  
N<-((sqrt(f_Z0)+sqrt(f_Z0+(2*(tau/qnorm(1-(alpha/2))))*(qnorm(1-beta)*abs(Z0))))/(2*(tau/qnorm(1-(alpha/2)))))^2
  
return(N)   
}

# (4) - Power for hypothesis testing
Power_Z<-function(alpha,N,Zc,delta, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Power calculation      
if(Zc<0){
    
Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*sqrt(d_Xxi+((Zc^2)/2)))/sqrt(d_Xxi+(((Zc-delta)^2)/2))) # Ha: Zt<Zc
    
}else{
    
Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*sqrt(d_Xxi+((Zc^2)/2)))/sqrt(d_Xxi+(((Zc+delta)^2)/2))) # Ha: Zt>Zc
    
}
return(Power)
}

# (5) - Margin of error of a confidence interval with 50% assurance probability
Tau_CI_Z<-function(alpha,Z0,N, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
# Determination of the margin of error    
Tau<-qnorm(1-(alpha/2))*sqrt((d_Xxi+((Z0^2)/2))/N)
  
return(Tau)
}

# (6) - Margin of error of a confidence interval with assurance probability
Tau_CI_AP_Z<-function(alpha,Z0,N,beta, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
# Determination of the margin of error    
Tau<-qnorm(1-(alpha/2))*(sqrt((d_Xxi+((Z0^2)/2))/N)+qnorm(1-beta)*(abs(Z0)/(2*N)))
  
return(Tau)
}

# (7) - Assurance probability
AP_Z<-function(alpha,N,Z0,tau, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### AP calculation      
  
AP<-pnorm(((tau/qnorm(1-(alpha/2)))*sqrt(N)-sqrt(d_Xxi+((Z0^2)/2)))/((abs(Z0)/(2*sqrt(N))))) # Ha: Zt<Zc
    
return(AP)
}


################# PR-score: Sample size calculation formulas for different goals under any design ############################

# (1) - Sample size for hypothesis testing
N_star_HypTest_PR<-function(alpha,gamma,PRc,delta, design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
#Sample size calculation
if(PRc<50){
# Ha: PRt<PRc
Nstar<-((qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(d_Xxi+((qnorm(p=(PRc)/100))^2/2))+qnorm(1-gamma)*100*dnorm(qnorm(p=(PRc-delta)/100))*sqrt(d_Xxi+((qnorm(p=(PRc-delta)/100))^2/2)))/delta)^2 
  
}else{
# Ha: PRt>PRc
Nstar<-((qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(d_Xxi+((qnorm(p=(PRc)/100))^2/2))+qnorm(1-gamma)*100*dnorm(qnorm(p=(PRc+delta)/100))*sqrt(d_Xxi+((qnorm(p=(PRc+delta)/100))^2/2)))/delta)^2 
    
}
return(Nstar)
}

# (2) - Sample size for interval estimation
N_star_CI_PR<-function(alpha,PR0,tau,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Sample size calculation  
  
Nstar<-((qnorm(1-(alpha/2))*100*dnorm(qnorm(p=(PR0)/100))*sqrt(d_Xxi+((qnorm(p=(PR0)/100))^2/2)))/tau)^2 
    
return(Nstar)
}

# (3) - Sample size for interval estimation with assurance probability 1-beta
Samp_PR_AP<-function(PR0,alpha,tau,beta,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu, q_edu){
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Sample size calculation  
  
f_PR<-(d_Xxi+((qnorm(p=PR0/100)^2)/2))*(100*dnorm(qnorm(p=PR0/100)))^2
  
der<-qnorm(p=PR0/100)*((((100*dnorm(qnorm(p=PR0/100)))^2)-2*f_PR)/(2*sqrt(f_PR)))
  
N<-((sqrt(f_PR)+sqrt(f_PR+4*(tau/qnorm(1-(alpha/2)))*qnorm(1-beta)*abs(der)*sqrt(d_Xxi+((qnorm(p=PR0/100)^2)/2))))/(2*(tau/qnorm(1-(alpha/2)))))^2
  
return(N)    
}

# (4) - Power for hypothesis testing
Power_PR<-function(alpha,N,PRc,delta,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
# Power calculation
if(PRc<50){
# Ha: PRt<PRc
Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(d_Xxi+((qnorm(p=(PRc)/100))^2/2)))/(100*dnorm(qnorm(p=(PRc-delta)/100))*sqrt(d_Xxi+((qnorm(p=(PRc-delta)/100))^2/2)))) 
    
}else{
# Ha: PRt>PRc
Power<-pnorm((delta*sqrt(N)-qnorm(1-alpha)*100*dnorm(qnorm(p=(PRc)/100))*sqrt(d_Xxi+((qnorm(p=(PRc)/100))^2/2)))/(100*dnorm(qnorm(p=(PRc+delta)/100))*sqrt(d_Xxi+((qnorm(p=(PRc+delta)/100))^2/2)))) 
}
  
return(Power)
  
}

# (5) - Margin of error of a confidence interval with 50% assurance probability
Tau_CI_PR<-function(alpha,PR0,N,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
# Calculation of the margin of error  
Tau<-qnorm(1-(alpha/2))*100*dnorm(qnorm(p=(PR0)/100))*sqrt((d_Xxi+((qnorm(p=(PR0)/100))^2/2))/N) 
  
return(Tau)
}

# (6) - Margin of error of a confidence interval with assurance probability 1-beta
Tau_AP_CI_PR<-function(alpha,PR0,N,beta,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}

f_PR<-(d_Xxi+((qnorm(p=PR0/100)^2)/2))*(100*dnorm(qnorm(p=PR0/100)))^2

der<-qnorm(p=PR0/100)*((((100*dnorm(qnorm(p=PR0/100)))^2)-2*f_PR)/(2*sqrt(f_PR)))

# Calculation of the margin of error  
Tau<-qnorm(1-(alpha/2))*(sqrt(f_PR/N)+qnorm(1-beta)*abs(der)*(sqrt(d_Xxi+((qnorm(p=PR0/100)^2)/2))/N)) 
  
return(Tau)
}

# (7) - Assurance probability
AP_PR<-function(alpha,N,PR0,tau,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
f_PR<-(d_Xxi+((qnorm(p=PR0/100)^2)/2))*(100*dnorm(qnorm(p=PR0/100)))^2

der<-qnorm(p=PR0/100)*((((100*dnorm(qnorm(p=PR0/100)))^2)-2*f_PR)/(2*sqrt(f_PR)))

# AP calculation
AP<-pnorm(((tau/qnorm(1-(alpha/2)))*sqrt(N)-sqrt(f_PR))/(abs(der)*(sqrt(d_Xxi+((qnorm(p=PR0/100)^2)/2))/sqrt(N)))) 

return(AP)
  
}

################################################################################################################################
################################### Multivariate norming: Innocenti et al. (2023, JEBS) ########################################
################################################################################################################################

###################### MD-score: Sample size calculation formulas for different goals under any design

# (1) -	Sample size for hypothesis testing
N_star_HypTest_Mahala<-function(alpha,gamma,MD_c,tau,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels,g_edu,  q_edu){

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Sample size calculation  
N<-((qnorm(1-alpha)*sqrt(d_Xxi+((MD_c^2)/2))+qnorm(1-gamma)*sqrt(d_Xxi+(((MD_c+tau)^2)/2)))/tau)^2
return(N)

}

# (2) -	Sample size for interval estimation
N_star_CI_Mahala<-function(alpha,MD0,tau,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Sample size calculation  
  
N<-((qnorm(1-(alpha/2))*sqrt(d_Xxi+((MD0^2)/2)))/tau)^2
return(N)
}

# (3) - Sample size for interval estimation with assurance probability 1-beta
Samp_MD_AP<-function(MD0,alpha,tau,beta,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
  
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}

f_MD<-d_Xxi+((MD0^2)/2)
### Sample size calculation  
  
N<-((sqrt(f_MD)+sqrt(f_MD+(2*(tau/qnorm(1-(alpha/2))))*(qnorm(1-beta)*MD0)))/(2*(tau/qnorm(1-(alpha/2)))))^2
  
return(N)    
}


# (4) - Power for hypothesis testing
Power_Mahala<-function(alpha,N,MD_c,tau,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### Power calculation  
  
Power<-pnorm((tau*sqrt(N)-qnorm(1-alpha)*sqrt(d_Xxi+((MD_c^2)/2)))/sqrt(d_Xxi+(((MD_c+tau)^2)/2))) 
    
return(Power)
}

# (5) - Margin of error of a confidence interval with 50% assurance probability
Tau_CI_Mahala<-function(alpha,MD0,N,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 

args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
# Determination of the margin of error  
Tau<-qnorm(1-(alpha/2))*sqrt((d_Xxi+((MD0^2)/2))/N)
  
return(Tau)
}

# (6) - Margin of error of a confidence interval with 1-beta assurance probability
Tau_CI_AP_Mahala<-function(alpha,MD0,N,beta,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
# Determination of the margin of error  
Tau<-qnorm(1-(alpha/2))*(sqrt((d_Xxi+((MD0^2)/2))/N)+qnorm(1-beta)*(MD0/(2*N)))
  
return(Tau)
}


# (7) - Assurance probability
AP_Mahala<-function(alpha,N,MD0,tau,design=c("Optimal"),k,model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu, q_edu){ 
  
args<-match.call() 
### Design    
if(design=="Optimal"){
    
d_Xxi<-k+1 
    
} else{    
    
# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels
    
# Sex
if("q_sex" %in% names(args)){Sex<-1:q_sex-1} else {Sex<-1} # No sex
    
#Education
if("q_edu" %in% names(args)){ #categorical
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
} else{Education<-1} #No education
    
data<-expand.grid(Age,Sex,Education) 
names(data)<-c("Age","Sex","Education")  
X_sub<-model.matrix(object=model,data=data) 
    
# maximum of the standardized prediction variance    
d_Xxi<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))}
  
### assurance probability calculation  
  
AP<-pnorm(((tau/qnorm(1-alpha/2))*sqrt(N)-sqrt(d_Xxi+((MD0^2)/2)))/(MD0/(2*sqrt(N)))) 
  
return(AP)
}
