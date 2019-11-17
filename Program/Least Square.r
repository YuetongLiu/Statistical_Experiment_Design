#take data for thiamin content but make unbalanced
# (n1,n2,n3,n4) = (4,4,3,5)

grain=c('b','b','b','b', 'm','m','m', 'o','o','o','o','o', 'w','w','w','w')
content=c(8.0,6.1,7.5,5.9, 4.7,6.4,4.9, 8.3,6.1,7.8,5.5,7.2, 5.2,4.5,6.0,6.1)

mnvec=tapply(content,grain,mean); print(mnvec)
#      b        m        o        w 
#6.875000 5.333333 6.980000 5.450000 

print(mean(content))
# 6.2625

ybar.av= mean(mnvec); print(ybar.av)
# 6.159583

fit0=lm(content~grain)  # parameter estimation based on default contr.treatment
print(summary(fit0))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.8750     0.5011  13.720 1.07e-08 ***
#grainm       -1.5417     0.7654  -2.014   0.0670 .  
#graino        0.1050     0.6723   0.156   0.8785    
#grainw       -1.4250     0.7086  -2.011   0.0674 .  

#============================================================
# for options(contrasts=c(,),  character vector of length 2
# first for unordered factors, second for ordered factors

options(contrasts=c("contr.treatment","contr.poly"))
# least squares parameter estimation based on barley as baseline
fit1=lm(content~grain)  
print(summary(fit1))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.8750     0.5011  13.720 1.07e-08 ***
#grainm       -1.5417     0.7654  -2.014   0.0670 .  
#graino        0.1050     0.6723   0.156   0.8785    
#grainw       -1.4250     0.7086  -2.011   0.0674 .  

print(anova(fit1))
#Response: content
#          Df  Sum Sq Mean Sq F value Pr(>F)  
#grain      3  9.3053  3.1018  3.0884 0.0679 .
#Residuals 12 12.0522  1.0044      

beta1=fit1$coefficient
# beta1=(muhat1, muhat2-muhat1, muhat3-muhat1, muhat4-muhat1)
cat(beta1[1]+c(0,beta1[2:4]),"\n")
# 6.875 5.333333 6.98 5.45   # matches vector of group sample means

#============================================================
options(contrasts=c("contr.sum","contr.poly"))
# least squares estimation based on zero sum constraint of treatment effects
fit2=lm(content~grain)   
print(summary(fit2))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.1596     0.2547  24.185  1.5e-11 ***
#grain1        0.7154     0.4364   1.640   0.1270    
#grain2       -0.8263     0.4819  -1.714   0.1121    
#grain3        0.8204     0.4066   2.018   0.0665 .  

print(anova(fit2))   # same as before
#Response: content
#          Df  Sum Sq Mean Sq F value Pr(>F)  
#grain      3  9.3053  3.1018  3.0884 0.0679 .
#Residuals 12 12.0522  1.0044   

beta2=fit2$coefficient  
# let ybar.av=(muhat1+...muhat4)/4,
# then beta2=(ybar.av, muhat1-ybar.av, muhat2-ybar.av, muhat3-ybar.av)
cat(beta2[1]+c(beta2[2:4],-sum(beta2[2:4])),"\n")
# 6.875 5.333333 6.98 5.45   # matches vector of group sample means

