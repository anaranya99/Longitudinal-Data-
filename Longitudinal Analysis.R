library(nlme)
library(MASS)
Orthodont
dist= Orthodont$distance
age=Orthodont$age
sex=as.factor(Orthodont$Sex)
ols=lm(dist~age+sex)
summary(ols)
ols
X=model.matrix(dist~age+sex)
t=t(X)%*%X
t1=X%*%t(X)
solve(t)
r=resid(ols)
V=r%*%t(r)
diag(V)
v=diag(diag(V))
var_beta=solve(t)%*%t(X)%*%v%*%X%*%solve(t)
var_beta
c=ols$coefficients
c=as.matrix(c)
std_err=sqrt(diag(var_beta))
test_beta=c/std_err
test_beta=as.matrix(test_beta)
beta_0=test_beta[1,1]
beta_1=test_beta[2,1]
beta_2=test_beta[3,1]
pnorm(test_beta,0,1)

pos_weights=sample(100:1000,108,replace=F)
wls_mod=lm(dist~X-1,data=Orthodont,weights = pos_weights)
summary(wls_mod)



















