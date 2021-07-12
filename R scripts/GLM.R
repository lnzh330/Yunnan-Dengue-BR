library("splines")

data<-read.table(header=T,file="glm.csv",sep=",")
newdata<-read.table(header=T,file="glm-pre.csv",sep=",")


# FUNCTION TO COMPUTE THE Q-AIC IN QUASI-POISSON MODELS
fqaic <- function(model1) {
  loglik <- sum(dpois(model1$y,model1$fitted.values,log=TRUE))
  phi <- summary(model1)$dispersion
  qaic <- -2*loglik + 2*summary(model1)$df[3]*phi
  return(qaic)
  print(qaic)
}

# FUNCTION TO COMPUTE THE Residual square IN QUASI-POISSON MODELS
fr2 <- function(model1) {
  pred <- predict(model1,type="response",df)
  sst <- sum((df$dengue_case - mean(df$dengue_case))^2)
  sse <- sum((pred - df$dengue_case)^2 , na.rm=TRUE)
  #find R-Squared
  rsq <- 1 - sse/sst
  return(rsq)
  print(rsq)
}

#overdispersion
library(qcc)
qcc.overdispersion.test(data$Yunnan, type="poisson")
#P<0.5, overdispersion, Using quasipoisson


#Yunnan
#model climate+border restriction
#selection variables
vars <- c("tm","tm1","tm2",'tm3',"hm","hm1","hm2",'hm3',"pre","pre1","pre2",'pre3','BD')
result <- c()
for(i in 1:13){
  model <- glm(substitute(Yunnan ~  x + Month.1 + Year + offset(log(pop)), list(x=as.name(vars[i]))), family="quasipoisson", data)
  result <- rbind(result,c(vars[i],fqaic(model)))
}
result

vars <- c("tm","tm1","tm2",'tm3',"hm","hm1","hm2",'hm3','BD')
result <- c()
for(i in 1:9){
  model <- glm(substitute(Yunnan ~  x + Month.1 + Year + pre2 + offset(log(pop)), list(x=as.name(vars[i]))), family="quasipoisson", data)
  result <- rbind(result,c(vars[i],fqaic(model)))
}
result

vars <- c("tm","tm1","tm2",'tm3',"hm","hm1","hm2",'hm3')
result <- c()
for(i in 1:8){
  model <- glm(substitute(Yunnan ~  x + Month.1 + Year + pre2 + BD + offset(log(pop)), list(x=as.name(vars[i]))), family="quasipoisson", data)
  result <- rbind(result,c(vars[i],fqaic(model)))
}
result

vars <- c("tm","tm1","tm2",'tm3')
result <- c()
for(i in 1:4){
  model <- glm(substitute(Yunnan ~  x + Month.1 + Year + pre2 + BD + hm1 + offset(log(pop)), list(x=as.name(vars[i]))), family="quasipoisson", data)
  result <- rbind(result,c(vars[i],fqaic(model)))
}
result

modely<-glm(Yunnan ~  Month.1 + Year + pre2 + BD + hm1 + tm1 + offset(log(pop)),family = quasipoisson(link = 'log'),data = data)
summary(modely)

par(mar = c(5, 5, 5, 2) + 0.2)
plot((fitted(modely)), residuals(modely, type= "pearson"), xlab = "Fitted values",   ylab = "Pearson residual")

fqaic(modely)


##Fit and predict
modely2<-glm(Yunnan ~  Month.1 + Year + BD + hm1 + tm1+ offset(log(pop)),family = quasipoisson(link = 'log'),data = data)
summary(modely2)
fqaic(modely2)

valid<-predict(modely2,newdata = data,type = 'response')
datavalid<-cbind(data,valid)
cor.test(datavalid$Yunnan,datavalid$valid)

pre<-predict(modely2,newdata = newdata,type = 'response')
