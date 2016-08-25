adjust <- function(data,var,adjust,adjust2,showc=FALSE,surv=FALSE,all=FALSE,fac=T){
  library(survival)
  
  if(surv==FALSE){
    
    #Crude model
    clog <- clogit(as.formula(paste("fall~strata(set)+",var)),data=data,method="approximate")
    v <- numeric(length(coef(clog)))
    for(i in 1:length(coef(clog))){v[i] <- (clog$var^(1/2))[i,i]}
    c <- summary(clog) #Hämta P-values
    crude <- cbind(round(exp(cbind(OR = coef(clog), confint(clog))),2),coef(clog),v,c$coefficients[,5])  
    
    #ADjusted 1
    clog <- clogit(as.formula(paste("fall~strata(set)+",var,"+",adjust)),data=data,method="approximate")
    v <- numeric(length(coef(clog)))
    for(i in 1:length(coef(clog))){v[i] <- (clog$var^(1/2))[i,i]}
    c <- summary(clog) #Hämta P-values
    adjusted <- cbind(round(exp(cbind(OR = coef(clog), confint(clog))),2),coef(clog),v,c$coefficients[,5])    
    
    #Adjusted 2
    clog <- clogit(as.formula(paste("fall~strata(set)+",var,"+",adjust2)),data=data,method="approximate")
    v <- numeric(length(coef(clog)));for(i in 1:length(coef(clog))){v[i] <- (clog$var^(1/2))[i,i]}
    c <- summary(clog) #Hämta P-values
    adjusted2 <- cbind(round(exp(cbind(OR = coef(clog), confint(clog))),2),coef(clog),v,c$coefficients[,5])  
    
    xn <- substring(names(coef(clog))[1],1,nchar(names(coef(clog))[1])-1)
    colnames(crude) <- colnames(adjusted) <- colnames(adjusted2) <- c("OR","2.5%","97.5%","coef","se coef","p-value")
    
    #om man kör tertiler/kvartiler, eller kontinuerlig:
    if(is.factor(data[,var])==T){
      x <- length(levels(data[,xn]))-1
      if(showc==TRUE){
        return(list(Model1=crude,Model2=adjusted,Model3=adjusted2))
      }else{
        return(list(Model1=crude[1:x,],Model2=adjusted[1:x,],Model3=adjusted2[1:x,]))}
    }else{ #Kontinuerlig
      if(showc==TRUE){return(list(Model1=crude,Model2=adjusted,Model3=adjusted2))
      }else{return(list(Model1=crude[1,],Model2=adjusted[1,],Model3=adjusted2[1,]))}
    }
    
  }else{
    if(all==TRUE){surv <- Surv(data$datdiff_km2, data$dead)}else{surv <- Surv(data$datdiff_km2, data$dead.cancer)}
    coxc <- coxph(as.formula(paste("surv~",var)),data=data)
    crude <- round(exp(cbind(HR = coef(coxc), confint(coxc))),2)
    adjusted <- round(exp(cbind(HR = coef(coxa), confint(coxa))),2)
    
    if(fac==TRUE){
      xn <- substring(names(coef(coxa))[1],1,nchar(names(coef(coxa))[1])-1)
      #print(xn)
      x <- length(levels(data[,xn]))-1}else{x <- 1}
    if(showc==TRUE){return(list(crude=coxc,adjusted=coxa))
    }else{return(list(crude=crude[1:x,1:3],adjusted=adjusted[1:x,1:3]))}
    
  }
  
}