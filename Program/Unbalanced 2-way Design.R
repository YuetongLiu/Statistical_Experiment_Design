# two-factor factorial design with two changes to make unbalanced
# check the anova tables that are produced in R
setwd("/Users/yuetongliu/Desktop")
ab=read.table("yield.txt",header=T)

attach(ab)
mntab=tapply(yield,list(alc,base),mean); print(mntab)
#       1      2
#1 90.63333 89.580
#2 89.26667 92.100
#3 88.92500 91.275

sdtab=tapply(yield,list(alc,base),sd); print(sdtab)
#          1        2
#1 0.7023769 1.861988
#2 1.1503623 1.573213
#3 1.2065792 1.400893
detach(ab)
ab$alc=factor(ab$alc)
ab$base=factor(ab$base)

options(contrasts=c("contr.sum","contr.poly"))

fit1=lm(yield~alc+base+alc:base,data=ab)
print(summary(fit1)) # what are the betas for the regression ?
print(anova(fit1))
#Analysis of Variance Table
#Response: yield
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#alc        2  5.396  2.6979  1.2896 0.29963  
#base       1 11.235 11.2350  5.3704 0.03246 *
#alc:base   2 16.942  8.4712  4.0493 0.03531 *
#Residuals 18 37.656  2.0920        

fit2=lm(yield~base+alc+base:alc,data=ab)
print(summary(fit2))  # where are the differences with fit1?
print(anova(fit2))
#Response: yield
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#base       1 11.833 11.8334  5.6565 0.02867 *
#alc        2  4.797  2.3987  1.1466 0.33986  
#base:alc   2 16.942  8.4712  4.0493 0.03531 *
#Residuals 18 37.656  2.0920   

mm1=model.matrix(fit1); 
print(mm1) # can you understand this?

# do this in order to get type III SS for alc by comparing SSE with fit1?
fit3=lm(yield~base+base:alc,data=ab)
print(anova(fit3))
#Response: yield
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#base       1 11.833  11.833  5.6565 0.02867 *
#base:alc   4 21.740   5.435  2.5979 0.07105 .
#Residuals 18 37.656   2.092        
# R combined two rows of the anova from fit2 so this cannot be used
# to get SSE with beta_j and (alpha beta)_{ij} in regression model.
# Can you figure out how to use mm1 and lsfit() to get this SSE?
m<-mm1[,c(1,4:6)] 
sum((ab$ yield - m%*%solve(t(m)%*%m)%*%t(m)%*%ab$ yield)^2)
sum(lsfit(m[,-1],ab$ yield)$ residuals^2) 
