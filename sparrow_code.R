k = read.table("/Users/michaelchen/Desktop/survival_sparrow.txt", header=TRUE)
num=rep(0,dim(k)[1])
title =c("STATUS", "AG"  ,"TL", "AE",  "WT"  ,"BH", "HL" , "FL"  , "TT", "SK", "KL")
for (i in c(1:dim(k)[1])) {
  if (k[i,1]=="Survived") {
    num[i]=1
  }
}

k$AG = factor(k$AG)

#Figure 1: Correlation matrix
cor(k[,3:11])
# Figure 2: Scatterplot matrix
plot(k[,3:11])
# Figure 3: 
par(mfrow=c(1,1))
plot(k$AG, k$STATUS)

k$STATUS = factor(k$STATUS)
#Figure 5
title =c("STATUS", "AG"  ,"TL", "AE",  "WT"  ,"BH", "HL" , "FL"  , "TT", "SK", "KL")
a = 7
lower = c()
upper=c()
diff=c()
s=matrix(0,a,11)
for (j in c(3:11)) {
  lower[j] = min(k[,j])
  upper[j] = max(k[,j])
  diff[j] = (upper[j]-lower[j])/a
  s[,j] = seq(from=0, to =a-1)*diff[j] +lower[j]
}
#Regular Bin Plot
par(mfrow=c(3,3))
for (j in c(3:11)) {
  g = data.frame(k[,j], bin=cut(k[,j], a-1, include.lowest=TRUE))
  ans = data.frame(num,g[,2])
  ans = table(ans)[2,]/(table(ans)[1,]+table(ans)[2,])
  plot(seq(1:(a-1)),ans, xlab="Bin Number", ylab = "Proportion Survived", main=title[j])
  #points(loess.smooth(seq(1:(a-1)), ans),type='l' ,lty=2)
}

#Figure 6: Logit 
#Logit Tranform
par(mfrow=c(3,3))
for (j in c(3:11)) {
  g = data.frame(k[,j], bin=cut(k[,j], a-1, include.lowest=TRUE))
  ans = data.frame(num,g[,2])
  ans = table(ans)[2,]/(table(ans)[1,]+table(ans)[2,])
  plot(seq(1:(a-1)),log((ans)/(1-ans)), xlab="Bin Number", ylab = "Proportion Survived", main=title[j])
  #points(loess.smooth(seq(1:(a-1)), ans),type='l' ,lty=2)
}

#Figure 7
par(mfrow=c(3,3))
for (i in c(3:11)) {
  plot(k[,i], num, xlab = " ", ylab=" ", main=title[i])
  points(loess.smooth(k[,i], num),type='l',lty=2)
}


#Model Fitting:
#Model with first order terms
k[,3:11] = scale(k[,3:11])
# Figure 4: Histogram
par(mfrow=c(3,3))
hist(k$TL)
hist(k$AE)
hist(k$WT)
hist(k$BH)
hist(k$HL)
hist(k$FL)
hist(k$TT)
hist(k$SK)
hist(k$KL)


k=data.frame(k,num)
fit = glm(num~AG+TL+AE+WT+BH+HL+FL+TT+SK+KL, family=binomial(link=logit), data=k)
library("car")

#Figure 8
summary(fit)
vif(fit)

#Null model
k = k[,-1]
fitnull = glm(num~1, family=binomial(link=logit), data=k)
#Saturated Model
a = factor(1:length(k$AG))
fitsat = glm(num~a, family=binomial(link=logit), data=k)
#Figure 9
anova(fitnull, fit, test="Chisq")
anova(fit,fitsat, test="Chisq")

#Quadratic Model
#Figure 10
fitquad = glm(num~+AG+TL+AE+WT+BH+HL+FL+TT+SK+KL+I(TL^2)+I(AE^2)+I(WT^2)+I(BH^2)+I(HL^2)+I(FL^2)+I(TT^2)+I(SK^2)+I(KL^2), family=binomial(link=logit),data=k)
summary(fitquad)
anova(fit,fitquad, test="Chisq")
vif(fitquad)

#Model with all second order and interactions

fitall = glm(num~.^2+AG+TL+AE+WT+BH+HL+FL+TT+SK+KL+I(TL^2)+I(AE^2)+I(WT^2)+I(BH^2)+I(HL^2)+I(FL^2)+I(TT^2)+I(SK^2)+I(KL^2), family=binomial(link=logit),data=k)
summary(fitall)
anova(fitall, fit, test="Chisq")
library("MASS")
#Figure 11
stepAIC(fitnull,c(lower=formula(fitnull), upper= formula(fitall)), direction=c("both"), k=2)
#Figure 12
fitFL2 = glm(num~TL+HL+WT+KL+FL+I(FL^2)+BH, family=binomial(link=logit),data=k)
fitnoFL =  glm(num~TL+HL+WT+KL+BH, family=binomial(link=logit),data=k)
AIC(fitFL2)
AIC(fitnoFL)
vif(fitFL2)


#Final Model
#Figure 13
finalfit = glm(num~TL+HL+WT+KL+FL+I(FL^2)+BH, family=binomial(link=logit),data=k)
summary(finalfit)
vif(finalfit)
par(mfrow=c(2,2))

dev.off()
anova(finalfit, fitsat, test="Chisq")

#Observations 27 and 40 appear to be influential observations and possible outliers
k[27,c("HL","TL", "WT", "KL", "BH", "FL")]
k[56,c("HL","TL", "WT", "KL", "BH", "FL")]
#Figure 14
k[27,c("HL","TL", "WT", "KL", "BH", "FL")] - colMeans(subset(k[,c("HL","TL", "WT", "KL", "BH", "FL")], k$num==1))
k[56,c("HL","TL", "WT", "KL", "BH", "FL")] - colMeans(subset(k[,c("HL","TL", "WT", "KL", "BH", "FL")], k$num==0))
#Figure 15
plot(finalfit)
