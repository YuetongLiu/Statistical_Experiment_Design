setwd("/Users/yuetongliu/Desktop")
# Example from Tamhane's book
# seatht = height of seat (-1 or 1 for low/high)
# igen = indicator generator on or off (-1 or -1)
# press = tire pressure (-1 or 1 for low/high)
# time = response variable = uphill travel time
bike=read.table("time.txt",header=T)
bike2=bike
bike2$seatht=factor(bike2$seatht)
bike2$igen=factor(bike2$igen)
bike2$press=factor(bike2$press)
fit1=lm(time~ seatht*igen*press, data=bike)
summ1=summary(fit1)
print(summ1)
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        47.1875     0.5116  92.238 2.13e-13 ***
#seatht             -5.4375     0.5116 -10.629 5.37e-06 ***
#igen                1.5625     0.5116   3.054   0.0157 *  
#press              -1.5625     0.5116  -3.054   0.0157 *  
#seatht:igen        -0.3125     0.5116  -0.611   0.5583    
#seatht:press        0.5625     0.5116   1.100   0.3035    
#igen:press          0.0625     0.5116   0.122   0.9058    
#seatht:igen:press   0.4375     0.5116   0.855   0.4173  
print(anova(fit1))
#Analysis of Variance Table
#Response: time
#                  Df Sum Sq Mean Sq  F value    Pr(>F)    
#seatht             1 473.06  473.06 112.9701 5.374e-06 ***
#igen               1  39.06   39.06   9.3284   0.01572 *  
#press              1  39.06   39.06   9.3284   0.01572 *  
#seatht:igen        1   1.56    1.56   0.3731   0.55825    
#seatht:press       1   5.06    5.06   1.2090   0.30352    
#igen:press         1   0.06    0.06   0.0149   0.90578    
#seatht:igen:press  1   3.06    3.06   0.7313   0.41732    
#Residuals          8  33.50    4.19 

# Note that, for any effect, MS(effect)=SS(effect) = effect^2 * 2^p * n
# here p=3 and n=2
effects=summ1$coefficients[-1,1]; print(effects)
#           seatht              igen             press       seatht:igen 
#          -5.4375            1.5625           -1.5625           -0.3125 
#     seatht:press        igen:press seatht:igen:press 
#           0.5625            0.0625            0.4375 

print(effects^2 *8*2)
#           seatht              igen             press       seatht:igen 
#         473.0625           39.0625           39.0625            1.5625 
#     seatht:press        igen:press seatht:igen:press 
#           5.0625            0.0625            3.0625 

options(contrasts=c("contr.sum","contr.poly"))
fit2=lm(time~ seatht*igen*press, data=bike2)
print(summary(fit2))
#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           47.1875     0.5116  92.238 2.13e-13 ***
#seatht1                5.4375     0.5116  10.629 5.37e-06 ***
#igen1                 -1.5625     0.5116  -3.054   0.0157 *  
#press1                 1.5625     0.5116   3.054   0.0157 *  
#seatht1:igen1         -0.3125     0.5116  -0.611   0.5583    
#seatht1:press1         0.5625     0.5116   1.100   0.3035    
#igen1:press1           0.0625     0.5116   0.122   0.9058    
#seatht1:igen1:press1  -0.4375     0.5116  -0.855   0.4173 
# Do you understand the sign changes?

print(anova(fit2))

