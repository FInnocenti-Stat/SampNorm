############################## RELATIVE EFFICIENCY ###########################################
# This function allows for designs with at most 3 predictors, with Education that can be categorical
# or continuous, and the distribution of age and education can be with equidistant levels, uniform, 
# and the age and education levels can be selected freely by the user.

RelEff<-function(model=~Age, min_age=20, max_age=80, g_age, Age_levels=c("Uniform"), age_granularity=100, q_sex, min_edu, max_edu, Edu_levels, g_edu,edu_granularity, q_edu, Z, PR, Mahala){

args<-match.call()  
### Predictors

# Age: Always assumed in the model
if(all(Age_levels=="Equidistant")){
Age<-seq(from=min_age, to=max_age, length.out=g_age) # Equidistant age groups
}else if(all(Age_levels=="Uniform")){
Age<-seq(from=min_age, to=max_age, by=1/age_granularity) # Uniform distribution
}else{Age<-Age_levels}  # Any levels

# Sex
if("q_sex" %in% names(args)){
  Sex<-1:q_sex-1
} else {Sex<-1} # No sex

#Education
if("q_edu" %in% names(args)){ #categorical
  
Education<-1:q_edu-1} else if("Edu_levels" %in% names(args)){ # continuous
  
if(all(Edu_levels=="Equidistant")){
Education<-seq(from=min_edu, to=max_edu, length.out=g_edu) } # Equidistant edu groups
else if(all(Edu_levels=="Uniform")){Education<-seq(from=min_edu, to=max_edu, by=1/edu_granularity) } # Uniform distribution
else {Education<-Edu_levels}  # Any levels
    
} else{Education<-1} #No education

### Sub-optimal Design: all possible combinations of predictors levels with equal design weights

data<-expand.grid(Age,Sex,Education) # Any possible combination: Check if works even without a variable
names(data)<-c("Age","Sex","Education")  

 
### Model:  
X_sub<-model.matrix(object=model,data=data) 
  
MAX_predvar_sub<-max(length(Sex)*length(Age)*length(Education)*diag(X_sub%*%solve(t(X_sub)%*%X_sub)%*%t(X_sub)))

#  
if("Z" %in% names(args)){NormStat=Z} else if("PR" %in% names(args)){NormStat=qnorm(PR/100)}else if("Mahala" %in% names(args)){NormStat=Mahala}else{print("Warning: Choose a norm statistic and set its target value")}
  

RE<-(dim(X_sub)[2]+((NormStat^2)/2))/(MAX_predvar_sub+((NormStat^2)/2))
  

N_increase<-round((RE^(-1)-1)*100)
  
Output<-list(RE,  N_increase)
names(Output)<-c("Relative Efficiency", "Increase the sample size by this percentage")
  
  return(Output)
}

REplot<-function(model=~Age, min_age=20, max_age=80, agegroup, q_sex, q_edu, Z, PR, Mahala,radius){#,quadraticAge){
  
  args<-match.call()
  
  ## start ##
  ## check <- "I(Age^2)" %in% attr(terms(model), "term.labels")
  ## print(check)
  ## ends  ##
  
  #if(quadraticAge==T){start<-3}else{start<-2}
  quadraticAge<-"I(Age^2)" %in% attr(terms(model), "term.labels")
  if(quadraticAge==T){start<-3}else{start<-2}
  xvec<-start
  if((agegroup-radius)>start){start=agegroup-radius}
  revec<-1
  labelvec<-"0%"
  
  # Age: Always assumed in the model, always Equidistant ; Education: Always categorical
  
  # Sex
  if("q_sex" %in% names(args)){
    
    if("Z" %in% names(args)){
      for(i in start:(agegroup+radius)){
        xvec=c(xvec,i)
        aux=RelEff(model=model, min_age=min_age, max_age=max_age, g_age=i, Age_levels="Equidistant", q_sex=q_sex, q_edu=q_edu, Z=Z)
        revec<-c(revec,aux[[1]])
        labelvec<-c(labelvec,paste(aux[[2]],"%"))
      }
      xvec=c(xvec,(agegroup+radius+1))
      auxout<-RelEff(model=model, min_age=min_age, max_age=max_age, age_granularity=2, Age_levels="Uniform", q_sex=q_sex, q_edu=q_edu, Z=Z)
      revec<-c(revec,auxout[[1]])
      labelvec<-c(labelvec,paste(auxout[[2]],"%"))
      
    } else if("PR" %in% names(args)){
      for(i in start:(agegroup+radius)){
        xvec=c(xvec,i)
        aux=RelEff(model=model, min_age=min_age, max_age=max_age, g_age=i, Age_levels="Equidistant", q_sex=q_sex, q_edu=q_edu, PR=PR)
        revec<-c(revec,aux[[1]])
        labelvec<-c(labelvec,paste(aux[[2]],"%"))
      }
      xvec=c(xvec,(agegroup+radius+1))
      auxout<-RelEff(model=model, min_age=min_age, max_age=max_age, age_granularity=2, Age_levels="Uniform", q_sex=q_sex, q_edu=q_edu, PR=PR)
      revec<-c(revec,auxout[[1]])
      labelvec<-c(labelvec,paste(auxout[[2]],"%"))
    }else{
      for(i in start:(agegroup+radius)){
        xvec=c(xvec,i)
        aux=RelEff(model=model, min_age=min_age, max_age=max_age, g_age=i, Age_levels="Equidistant", q_sex=q_sex, q_edu=q_edu, Mahala=Mahala)
        revec<-c(revec,aux[[1]])
        labelvec<-c(labelvec,paste(aux[[2]],"%"))
      }
      xvec=c(xvec,(agegroup+radius+1))
      auxout<-RelEff(model=model, min_age=min_age, max_age=max_age, age_granularity=2, Age_levels="Uniform", q_sex=q_sex, q_edu=q_edu, Mahala=Mahala)
      revec<-c(revec,auxout[[1]])
      labelvec<-c(labelvec,paste(auxout[[2]],"%"))
    }
    
  }else{
    if("Z" %in% names(args)){
      for(i in start:(agegroup+radius)){
        xvec=c(xvec,i)
        aux=RelEff(model=model, min_age=min_age, max_age=max_age, g_age=i, Age_levels="Equidistant", q_edu=q_edu, Z=Z)
        revec<-c(revec,aux[[1]])
        labelvec<-c(labelvec,paste(aux[[2]],"%"))
      }
      xvec=c(xvec,(agegroup+radius+1))
      auxout<-RelEff(model=model, min_age=min_age, max_age=max_age, age_granularity=2, Age_levels="Uniform", q_edu=q_edu, Z=Z)
      revec<-c(revec,auxout[[1]])
      labelvec<-c(labelvec,paste(auxout[[2]],"%"))
    } else if("PR" %in% names(args)){
      for(i in start:(agegroup+radius)){
        xvec=c(xvec,i)
        aux=RelEff(model=model, min_age=min_age, max_age=max_age, g_age=i, Age_levels="Equidistant", q_edu=q_edu, PR=PR)
        revec<-c(revec,aux[[1]])
        labelvec<-c(labelvec,paste(aux[[2]],"%"))
      }
      xvec=c(xvec,(agegroup+radius+1))
      auxout<-RelEff(model=model, min_age=min_age, max_age=max_age, age_granularity=2, Age_levels="Uniform", q_edu=q_edu, PR=PR)
      revec<-c(revec,auxout[[1]])
      labelvec<-c(labelvec,paste(auxout[[2]],"%"))
    }else{
      for(i in start:(agegroup+radius)){
        xvec=c(xvec,i)
        aux=RelEff(model=model, min_age=min_age, max_age=max_age, g_age=i, Age_levels="Equidistant", q_edu=q_edu, Mahala=Mahala)
        revec<-c(revec,aux[[1]])
        labelvec<-c(labelvec,paste(aux[[2]],"%"))
      }
      xvec=c(xvec,(agegroup+radius+1))
      auxout<-RelEff(model=model, min_age=min_age, max_age=max_age, age_granularity=2, Age_levels="Uniform", q_edu=q_edu, Mahala=Mahala)
      revec<-c(revec,auxout[[1]])
      labelvec<-c(labelvec,paste(auxout[[2]],"%"))
    }
  }
  
  #cat("xvec=",xvec,"revec=",revec,"labelvec=",labelvec)
  
  if(xvec[1]==xvec[2]){xvec=xvec[-1];revec=revec[-1];labelvec=labelvec[-1]}
  
  out=plot(xvec,revec,main="Sample size percentage increase",sub="Required sample size increase (percentage)",xlab="Age levels",ylab="Relative Efficiency",ylim=c(0,1.1),type="b",lwd=1.5,xaxt="n")
  axis(1,at=xvec,labels=c("Optimal",xvec[2:(length(xvec)-1)],"Worst"))
  points(x=xvec[1],1,col="red",lwd=1.5,pch=19)
  points(x=max(xvec),revec[length(revec)],col="darkgrey",lwd=1.5,pch=19)
  points(x=agegroup,revec[which(xvec==agegroup)],col="blue",lwd=1.5,pch=19)
  text(xvec+0.05,revec+0.05,labelvec)
  #text(xvec[-1] + 0.05, revec[-1] + 0.05, labelvec[-1])  # Skip the first element for standard text placement
  #text(xvec[1], revec[1] - 0.05, labelvec[1], col = "red")  # Position the first label separately
  if(revec[length(revec)]>0.5){
    legend("bottomright",legend=c("Optimal Design","Proposed Design","Worst Design"),col=c("red","blue","darkgrey"),pch=c(19,19,19),box.lty=0)
  }else{
    legend("topright",legend=c("Optimal Design","Proposed Design","Worst Design"),col=c("red","blue","darkgrey"),pch=c(19,19,19),box.lty=0)
  }
  return(out)
  
}

