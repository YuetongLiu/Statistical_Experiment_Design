getwd()
setwd("/Users/yuetongliu/Desktop")
th = read.table("grain.txt",header=T)  # 24 rows, 2 columns
th$grain = factor(th$grain)
attach(th)
# summary statistics by group, add SD to usual summary()
mysummary=function(x) 
{ out=c(mean(x),sd(x),summary(x)[c(1,2,3,5,6)]) 
names(out)=c("mean","SD","min","Q1","median","Q3","max") 
out
}
m=tapply(content,grain,mysummary)  # summary of content by grain
print(m,digits=3)
# set graphics for subplots in 2x2 grid
par(mfrow=c(2,2))
# boxplots by group
plot(grain,content,xlab="grain",ylab="content")
title("Plots for thiamin content by grain type")
# fit (linear) model and get ANOVA table
model1=lm(content~grain)
print(anova(model1))
# residual plots
resid=residuals(model1)
pred=fitted.values(model1)
plot(pred,resid,xlab="predicted",ylab="residual")
qqnorm(resid,ylab="residuals")
hist(resid,xlab="residuals", main="Histogram of residuals")

