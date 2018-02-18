rm(list=ls())
setwd("D:/BANA_Renga/Coursework/Stats_Models")
library(dplyr)
library(readxl)
library(MASS)

#1 Read the files and get the dataset
d1<-read_excel("FAA1.xls")
d2<-read_excel("FAA2.xls")


d2$duration <- NA
d <- rbind(d1,d2)
dup <-duplicated(d[,-2])
df <- d[!dup,]


df1<- df[which((df$duration>40 | is.na(df$duration)) & 
                 (df$speed_ground >30 & df$speed_ground < 140) &
                 (df$speed_air >30  & df$speed_air < 140 | is.na(df$speed_air)) & 
                 df$height >6 & df$distance < 6000),]


#Step-1: Adding Columns:
df1$long.landing[df1$distance > 2500] <- 1
df1$long.landing[df1$distance <= 2500] <- 0

df1$risky.landing[df1$distance > 3000] <- 1
df1$risky.landing[df1$distance <= 3000] <- 0

table(df1$long.landing)
table(df1$risky.landing)

head(df1)

d <- df1[,-8]
names(d)

#2 Distribution of Long Landing:
par(mfrow = c(1,1))
hist(d$long.landing, main = "Distribution of Long Landing", xlab = "Long Landing (Y/N)", ylab = "Count")


pct <- round(table(d$long.landing)/length(d$long.landing)*100,1)
labs<-c("No","Yes")
labs <- paste(labs,pct)
labs <- paste(labs,"%",sep = "")
pie(table(d$long.landing),labels = labs,col = rainbow(length(labs)),main = "Distribution of Long Landing")

#3 Regression with individual Variables:

names(d)
m1<-glm(long.landing~aircraft, family = binomial, data = d)
m2<-glm(long.landing~duration, family = binomial, data = d)
m3<-glm(long.landing~no_pasg, family = binomial, data = d)
m4<-glm(long.landing~speed_ground, family = binomial, data = d)
m5<-glm(long.landing~speed_air, family = binomial, data = d)
m6<-glm(long.landing~height, family = binomial, data = d)
m7<-glm(long.landing~pitch, family = binomial, data = d)


summary(m1)$coef
summary(m2)$coef
summary(m3)$coef
summary(m4)$coef
summary(m5)$coef
summary(m6)$coef
summary(m7)$coef


#4 Visualize the significant variables:

#Visualize:
attach(d)
#Consider the Speed_Ground Variable
plot(long.landing~height)
#We see there is no clear evidence of a relationship
names(d)
#Since the respose is binary, we can use a jitter plot
wcgs$y <- ifelse(long.landing == "no",0,1)
#windows()
plot(jitter(long.landing)~jitter(height), data = d, pch = "*", main = "Jitter plot between Long Landing and Height", xlab = "Height", ylab = " Long Landing")
plot(jitter(long.landing)~jitter(speed_air), data = d, pch = "*", main = "Jitter plot between Long Landing and Speed Air", xlab = "Speed Air", ylab = " Long Landing")
plot(jitter(long.landing)~jitter(speed_ground), data = d, pch = "*", main = "Jitter plot between Long Landing and Speed Ground", xlab = "Speed Ground", ylab = " Long Landing")
plot(jitter(long.landing)~jitter(pitch), data = d, pch = "*", main = "Jitter plot between Long Landing and Pitch", xlab = "Pitch", ylab = " Long Landing")
plot(jitter(long.landing)~jitter(duration), data = d, pch = "*", main = "Jitter plot between Long Landing and Duration", xlab = "Duration", ylab = " Long Landing")

#plot(jitter(long.landing)~jitter(aircraft), data = d, pch = ".")

library(ggplot2)
#Histograms
ggplot(d, aes(x=speed_air, fill =as.factor(long.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=speed_ground, fill = as.factor(long.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=height, fill = as.factor(long.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=pitch, fill = as.factor(long.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=duration, fill = as.factor(long.landing))) + geom_histogram(position = "dodge",binwidth = 1)

#ggplot(d, aes(x=aircraft, fill = as.factor(long.landing))) + geom_histogram(position = "dodge",binwidth = 1)

#5 Full Model:

fm <- glm(long.landing~ speed_ground + pitch + aircraft, family = binomial)
summary(fm)


#6 Stepwise: Forward Selection Using AIC 
nullmodel<- glm(long.landing~1,data=d)
fullmodel<- fm

step1<- stepAIC(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')
summary(step1)

#7 Backward Selection
n<-dim(d)[1]
step2<- step(fullmodel, direction='backward', k = log(n))
summary(step2)

#8 Presentation
barplot(table(long.landing,aircraft), main = "Distribution of Long Landing", ylab = "count of occurence", ylim = c(0,500))

#9 
#####################################################################################
#####################################################################################
#####################################################################################

# Distribution of Risky Landing:
par(mfrow = c(1,1))
hist(d$risky.landing, main = "Distribution of risky Landing", xlab = "risky Landing (Y/N)", ylab = "Count")


pct <- round(table(d$risky.landing)/length(d$risky.landing)*100,1)
labs<-c("No","Yes")
labs <- paste(labs,pct)
labs <- paste(labs,"%",sep = "")
pie(table(d$risky.landing),labels = labs,col = rainbow(length(labs)),main = "Pie Chart")

#3 Regression with individual Variables:

names(d)
m8<-glm(risky.landing~aircraft, family = binomial, data = d)

m9<-glm(risky.landing~duration, family = binomial, data = d)

m10<-glm(risky.landing~no_pasg, family = binomial, data = d)

m11<-glm(risky.landing~speed_ground, family = binomial, data = d)

m12<-glm(risky.landing~speed_air, family = binomial, data = d)

m13<-glm(risky.landing~height, family = binomial, data = d)

m14<-glm(risky.landing~pitch, family = binomial, data = d)


summary(m8)$coef
summary(m9)$coef
summary(m10)$coef
summary(m11)$coef
summary(m12)$coef
summary(m13)$coef
summary(m14)$coef


#4 Visualize the significant variables:

#Visualize:
attach(d)
#Consider the Speed_Ground Variable
plot(risky.landing~height)
#We see there is no clear evidence of a relationship

#Since the respose is binary, we can use a jitter plot
wcgs$y <- ifelse(risky.landing == "no",0,1)
windows()
plot(jitter(risky.landing)~jitter(height), data = d, pch = "*", main = "Jitter plot between Risky Landing and Height", xlab = "Height", ylab = "Risky Landing")
plot(jitter(risky.landing)~jitter(speed_air), data = d, pch = "*", main = "Jitter plot between Risky Landing and Speed Air", xlab = "speed_air", ylab = " Risky Landing")
plot(jitter(risky.landing)~jitter(speed_ground), data = d, pch = "*",main = "Jitter plot between Risky Landing and Speed Ground", xlab = "speed_ground", ylab = " Risky Landing")
plot(jitter(risky.landing)~jitter(pitch), data = d, pch = "*",main = "Jitter plot between Risky Landing and Pitch", xlab = "Pitch", ylab = " Risky Landing")
#plot(jitter(risky.landing)~jitter(aircraft), data = d, pch = ".")

library(ggplot2)
#Histograms
ggplot(d, aes(x=speed_air, fill =as.factor(risky.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=speed_ground, fill = as.factor(risky.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=height, fill = as.factor(risky.landing))) + geom_histogram(position = "dodge",binwidth = 1)
ggplot(d, aes(x=pitch, fill = as.factor(risky.landing))) + geom_histogram(position = "dodge",binwidth = 1)
#ggplot(d, aes(x=aircraft, fill = as.factor(risky.landing))) + geom_histogram(position = "dodge",binwidth = 1)

#5 Full Model:

fm1<- glm(risky.landing~ speed_ground + aircraft, family = binomial)
summary(fm1)


#6 Stepwise: Forward Selection Using AIC 
nullmodel<- glm(risky.landing~1,data=d)
fullmodel<- fm1

step3<- stepAIC(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')
summary(step3)

#7 Backward Selection
n<-dim(d)[1]
step4<- step(fullmodel, direction='backward', k = log(n))
summary(step4)


#10 Presentation
barplot(table(risky.landing,aircraft), main = "Distribution of Risky Landing", ylab = "count of occurence", ylim = c(0,500))

#11 Comparison of models

#12 ROC Curves for the models:

#Model-1: Long Landing
predprob1<-predict(fm,type="response") ### predicted probabilities
predout1<-ifelse(predprob1<0.5,"no","yes") ### Predicted outcomes using 0.5 as the threshold
table(predout1)
d1<-data.frame(d,predprob1,predout1)
xtabs(~long.landing+predout1,d1)

#Model-2: Risky Landing
predprob2<-predict(fm1,type="response")
predout2<-ifelse(predprob2<0.5,"no","yes") ### Predicted outcomes using 0.5 as the threshold
table(predout2)
d2<-data.frame(d,predprob2,predout2)
xtabs(~long.landing+predout2,d2)



#library("verification")
#roc.plot(d$long.landing == "1", predprob1, main = "model-1 ROC")

#roc.plot(x = credit.test$Y == "1", pred = cbind(prob.glm0.outsample, prob.glm1.outsample), 
 #        legend = TRUE, leg.text = c("Full Model", "X_3, X_8, and X_11_2"))$roc.vol


library(ROCR)
pred1 <- prediction(predprob1, d$long.landing)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, col = "blue", main = "ROC Curves")

pred2 <- prediction(predprob2, d$risky.landing)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, add=TRUE, col = "red")

legend(0.7,0.6, legend=c("Long Landing","Risky Landing"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

#13 Prediction on New data:
#Boeing, duration=200, no_pasg=80, speed_ground=115, speed_air=120, height=40, pitch=4).
View(d)
str(d)

new_data<- data.frame(aircraft= "boeing", duration=200, no_pasg=80, speed_ground=115, speed_air=120, height=40, pitch=4)
View(new_data)
as.data.frame(new_data)

library(faraway)
#Prediction + CI
out1 <- predict(fm,newdata = new_data, type = "link", se =T)
ilogit(out1$fit)
val1 <- ilogit(c(out1$fit-1.96*out1$se.fit,out1$fit+1.96*out1$se.fit))
val1
out2 <- predict(fm1_1,newdata = new_data, type = "link", se = T)
ilogit(out2$fit)
val2 <- ilogit(c(out2$fit-1.96*out2$se.fit,out2$fit + 1.96*out2$se.fit))
val2
#out2
#out


#14 Compare models with different link functions
fm1_1<- glm(risky.landing~ speed_ground + aircraft, family = binomial)
fm1_2<- glm(risky.landing~ speed_ground + aircraft, family = binomial(link = "probit"))
fm1_3<- glm(risky.landing~ speed_ground + aircraft, family = binomial(link = "cloglog"))
summary(fm1_1)$coef
summary(fm1_2)$coef
summary(fm1_3)$coef

#15 
predprob1_1<-predict(fm1_1,type="response")
predprob1_2<-predict(fm1_2,type="response")
predprob1_3<-predict(fm1_3,type="response")


pred1_1 <- prediction(predprob1_1, d$risky.landing)
perf1_1 <- performance(pred1_1, "tpr", "fpr")
plot(perf1_1, lty = 2, main = "ROC Curve")

pred1_2 <- prediction(predprob1_2, d$risky.landing)
perf1_2 <- performance(pred1_2, "tpr", "fpr")
plot(perf1_2, add=T, col = "blue")

pred1_3 <- prediction(predprob1_3, d$risky.landing)
perf1_3 <- performance(pred1_3, "tpr", "fpr")
plot(perf1_3, col = "red", add = T)

legend(0.7,0.6, legend=c("Logit","Probit", "C-Log-Log"),
       col=c("black","blue", "red"), lty=1:2, cex=0.8)

#16 Top 5 risky landings
risk1<-predict(fm1_1,type="response")
risk2<-predict(fm1_2,type="response")
risk3<-predict(fm1_3,type="response")

#View(risk1)
#class(risk1)
r1<-as.data.frame(risk1)
col2<-c(no = 1:831)
r1<-cbind(risk1,col2)
r1<-as.data.frame(r1)
head(r1<-arrange(r1,desc(risk1)))


r2<-as.data.frame(risk2)
col2<-c(no = 1:831)
r2<-cbind(risk2,col2)
r2<-as.data.frame(r2)

head(r2<-arrange(r2,desc(risk2)))

r3<-as.data.frame(risk3)
col2<-c(no = 1:831)
r3<-cbind(risk3,col2)
r3<-as.data.frame(r3)

head(r3<-arrange(r3,desc(risk3)))


library(dplyr)
head(r1<-arrange(r1,desc(risk1)))
  
View(r1)
str(mtcars)

#17 Comparison of models

out3 <- predict(fm1_2,newdata = new_data, type = "link", se =T)
val3 <- pnorm(c(out3$fit-1.96*out3$se.fit,out3$fit+1.96*out3$se.fit))
val3
out4 <- predict(fm1_3,newdata = new_data, type = "link", se = T)
val4 <- ilogit(c(out4$fit-1.96*out4$se.fit,out4$fit + 1.96*out4$se.fit))
val4
