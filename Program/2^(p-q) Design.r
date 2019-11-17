# magazine data, fractional factorial experiment 

ff=read.table("factor.txt",header=T)
fit=lm(y~A+B+C+D+E+F+ A:B+A:C+A:E+A:F+C:E+C:F+A:C:E+A:C:F,data=ff)
print(summary(fit))

fit2=lm(y~A+B+C+D+E+F+ A:B+A:C+A:D+A:E+A:F+C:E+C:F,data=ff)
print(summary(fit2))

# Do you understand the difference of fit1 and fit2?

fitall=lm(y~A*B*C*D*E*F,data=ff)
print(summary(fitall))
# Do you understand the difference of output for fit1 and fitall?
# How does R determine which effect to list for an aliased set of effects?

# Exercise: check that you can calculate some of the estimated effects from appropriate contrasts

