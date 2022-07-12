#### PROBABILITY COURSE EXAM ####

###First of all let us import the csv file in R###

data=read.csv2("C:\\Users/Dionigi Bennet/Desktop/QRM UniBO/Probability/exam/bennet.csv")
data
data <- data[,2:3]
x=data[,1]
y=data[,2]
x
y
str(data)
#descr stat 

summary(x)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
 0.01683  0.29504  0.57539  0.95051  1.14890 22.20850 
sd(x)  [1] 1.235697
var(x) [1] 1.526946

summary(y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -5.688  -2.681  -2.034  -2.036  -1.363   1.135 
sd(y)  [1] 0.9986242
var(y)  [1] 0.9972502

library(TSA) #FOR SKEWNESS AND KURTOSIS 
kurtosis(x) 63.78407
skewness(x) 5.730699

kurtosis(y)  0.002772206
skewness(y)  -0.02914288

#Graphical inspection
library(graphics)

par(mfrow=c(2,1))
plot(x, type="l", col="BLUE", xlab="x", ylab="y")
title("x variable")
plot(y, type="l", col="GREEN", xlab="x", ylab="y")
title("y variable")

dx=density(x,kernel=c("gaussian"))

dy=density(y,kernel=c("gaussian"))

par(mfrow=c(2,3))
hist(x)
qqnorm(x)
plot(dx)
hist(y)
qqnorm(y)
plot(dy)

##COVARIANCE AND CORRELATION##

cov(x,y)
[1] 0.4424398

#Pearson corr index
cor(x,y,,method=c("pearson"))
          V1        V2
V1 1.0000000 0.3585421
V2 0.3585421 1.0000000

#Kendall corr index, measure of rank correlation 
cor(x,y,method=c("kendall"))
    V1        V2
V1 1.0000000 0.4270126
V2 0.4270126 1.0000000

#Spearman corr index
cor(x, y ,method=c("spearman"))
          V1        V2
V1 1.0000000 0.5953125
V2 0.5953125 1.0000000

#estimation of marginals

library(MASS) #ESTIMATION AND GOODNESS OF FIT, KS TEST

#Gaussian?
par_1x=fitdistr(x,"normal")
par_1x
  mean          sd    
  0.95050858   1.23549070 
 (0.02255687) (0.01595012)

god_of_fit1_x=ks.test(x,"pnorm",par_1x$estimate[1],par_1x$estimate[2])
god_of_fit1_x

 One-sample Kolmogorov-Smirnov test

data:  x
D = 0.22828, p-value < 2.2e-16
alternative hypothesis: two-sided



par_1y=fitdistr(y,"normal")
par_1y
mean           sd     
  -2.03562338    0.99845771 
 ( 0.01822926) ( 0.01289003)

god_of_fit1_y=ks.test(y,"pnorm",par_1y$estimate[1],par_1y$estimate[2])
god_of_fit1_y
   One-sample Kolmogorov-Smirnov test

data:  y
D = 0.0096435, p-value = 0.943
alternative hypothesis: two-sided   

#T student?

par_2x=fitdistr(x,"t", method="Nelder-Mead")
par_2x
      m            s            df    
  0.52910389   0.36515659   1.48795408 
 (0.01063901) (0.01085119) (0.06233254)

god_of_fit2_x=ks.test(x,"pt",par2_x$estimate[1],par2_x$estimate[2],par2_x$estimate[3])
god_of_fit2_x

   One-sample Kolmogorov-Smirnov test

data:  x
D = 0.36548, p-value < 2.2e-16
alternative hypothesis: two-sided


par_2y=fitdistr(y,"t", method="Nelder-Mead")
par_2y

   m               s               df      
  -2.035783e+00    9.978496e-01    1.549915e+03 
 ( 1.822612e-02) ( 2.589975e-04) (          NaN)

god_of_fit2_y=ks.test(y,"pt",par_2y$estimate[1],par_2y$estimate[2],par_2y$estimate[3])
god_of_fit2_y

#warnings()

#gamma

par_3x <- fitdistr(x, "gamma", method="Nelder-Mead")
par_3x
   shape       rate   
  1.1406893   1.1999797 
 (0.0262306) (0.0343833)

goodoffit_3x <- ks.test(x, "pgamma", par_3x$estimate[1], par_3x$estimate[2]) 
goodoffit_3x

     One-sample Kolmogorov-Smirnov test

data:  x
D = 0.078895, p-value < 2.2e-16
alternative hypothesis: two-sided


#y presents negative values so cannot be a gamma


#lognorm?

par_4x <- fitdistr(x, "lognormal")
par_4x

 meanlog        sdlog   
  -0.54928611    1.00215003 
 ( 0.01829667) ( 0.01293770)

goodoffit_4x <- ks.test(x, "plnorm", par_4x$estimate[1], par_4x$estimate[2])
goodoffit_4x

 One-sample Kolmogorov-Smirnov test

data:  x
D = 0.0088842, p-value = 0.9719
alternative hypothesis: two-sided


#y pressents negative values that cannot be evaluated in a log func

#RESULTS: The ks test for x variable under the H0 Hypothesis of being a log-norm
# returns a p-value of 0.9719, the unique value acceptable among the listed
# distribution. Hence, the variable x can be approximated to a log-normal
#variable. The p-values of the tests for y, differently from x, return
#the biggest p-value under the normall null hypothesis of gaussian distribution,
# i.e. 0.943. The distributions of the variables are different, it means
# that probably are data of different nature.

par(mfrow=c(1,2))
plot(dx, ylim=c(0,1.3),xlim=c(0,4), lwd=1.5)
curve(dlnorm(x=x, meanlog = -0.54928611, sdlog = 1.00215003 , log = FALSE), add=TRUE, col="red", lwd=1.5)
legend("topright", legend=c("True X","Log Normal"), col=c("black", "red"), lwd=1.5, cex=0.8)
plot(dy, ylim=c(0,0.5),xlim=c(-6,2), lwd=1.5)
curve(dnorm(x=y, mean = -2.03562338, sd = 0.99845771), xname="y", col="red", add=TRUE, lwd=1.5)
legend("topright", legend=c("True Y","Normal"), col=c("black", "red"), lwd=1.5, cex0.8)

#We can be satisfied about the graphical comparison 

lnu1 <- plnorm(x, par_4x$estimate[1], par_4x$estimate[2])
lnu1
max(lnu1)
min(lnu1)


nu2 <- pnorm(y, par_1y$estimate[1], par_1y$estimate[2])
nu2
max(nu2)
min(nu2)
###pobs function###

pobsdata <- pobs(data)
pobsdata
plot(pobsdata)
u1 <- pobsdata[,1]
u2 <- pobsdata[,2]
#clayton?

###FITTED lnu1 nu2###

library(copula)

#gaussian copula?

gausscop <- normalCopula(dim=2)

modelgauss <- fitCopula(gausscop, cbind(lnu1,nu2), method="ml")
summary(modelgauss)
rho.1   0.6177       0.01

logLikgauss=loglikCopula(modelgauss@estimate, cbind(lnu1,nu2), gausscop)

BIC=-2*logLikgauss+1*log(length(x))
BIC
[1] -1433.655

#Frank Copula?

frankcop=frankCopula(dim=2)
modelfrank=fitCopula(frankcop, cbind(lnu1,nu2), method="ml")
summary(modelfrank)
alpha    4.578      0.129

logLikfrank=loglikCopula(modelfrank@estimate, cbind(lnu1,nu2), frankcop)

BIC=-2*logLikfrank+1*log(length(x))
BIC
[1] -1321.186

#Clayton copula?

claycop=claytonCopula(dim=2)
modelclay <- fitCopula(claycop, cbind(lnu1, nu2), method="ml")
summary(modelclay)
      Estimate Std. Error
alpha     1.52      0.043

LogLikclay <- loglikCopula(modelclay@estimate, cbind(lnu1,nu2), claycop)
BIC <- -2*LogLikclay+1*log(length(x))
BIC
[1] -1927.391

#Gumbel copula?

gumcopula <- gumbelCopula(dim=2)
gummodel <- fitCopula(gumcopula, cbind(lnu1, nu2), method="ml")
summary(gummodel)
      Estimate Std. Error
alpha    1.576      0.023

LogLikgum <- loglikCopula(gummodel@estimate, cbind(lnu1, nu2), gumcopula) 
BIC <- -2*LogLikgum+1*log(length(x))
BIC
[1] -1008.558

#best is clayton with with a BIC of [1] -1927.391 as assumed

############################################################################
###########################################################################
##########################################################################

###If any distributions had fitted with our data, we could have compute the 
# empirical distribution for each variable, that would led us in a non-parametric
#scenario, but it does not gives us problems because our important parameter
# is the copula parameter 

F1_hat=ecdf(x)
F2_hat=ecdf(y)

##x
u1=F1_hat(x)
u1 #sono i valori mentre F1_hat è la funz
F1_hat

#controllo che non ci siano 1 o 0
min(u1)
max(u1)
which(u1==1)
#there a value "1", let us remove it
#è il 714 valore, rimuoveremo la 714esima osservazione sia da x che da y, in
#quanto outlier

u1 <- u1[-which(u1==u1[714])]
u1 
min(u1)
max(u1)
#OK

##y
u2=F2_hat(y)
u2 #sono i valori mentre F1_hat è la funz
F2_hat

min(u2)
max(u2) 
which(u2==1) #c'è un 1 in posizione 659
#ora dobbiamo rimuovere la posizione 714 e la pos 659
#poi anche la 659 in x

u2 <- u2[-which(u2==u2[659])]
u2
which(u2==0.794) #it is just one observation with the value of y in 714

u2 <- u2[-which(u2==u2[714])]
u2

#ora rimuoviamo la 659 da u1 
u1
which(u1==0.631) #it is just the observation number 659

u1 <- u1[-which(u1==u1[659])]
u1

#now we have two vectors of u1 and u2 of probabilities generated by the two
#empirical dist function F1 and F2 without outliers with prob 0 or 1

plot(u1,u2, "p", xlab="u", ylab="v")

###now let us find the copula for the x fitted in a gamma dist and the y in a 
#lognormal dist, then for the empirical ones. 
####empirical dist####

#gaussian copula?

gausscop <- normalCopula(dim=2)

modelgaussE <- fitCopula(gausscop, cbind(u1,u2), method="ml")
modelgaussE
summary(modelgaussE)

logLikgaussE=loglikCopula(modelgaussE@estimate, cbind(u1,u2), gausscop)

BIC=-2*logLikgaussE+1*log(length(x))
BIC

#Frank Copula?

frankcop=frankCopula(dim=2)
modelfrankE=fitCopula(frankcop, cbind(u1,u2), method="ml")
summary(modelfrankE)

logLikfrankE=loglikCopula(modelfrankE@estimate, cbind(u1,u2), frankcop)

BIC=-2*logLikfrankE+1*log(length(x))
BIC


#Clayton copula?

claycop=claytonCopula(dim=2)
modelclayE <- fitCopula(claycop, cbind(u1, u2), method="ml")
summary(modelclayE)
LogLikclayE <- loglikCopula(modelclayE@estimate, cbind(u1,u2), claycop)
BIC <- -2*LogLikclayE+1*log(length(x))
BIC


#Gumbel copula?

gumcopula <- gumbelCopula(dim=2)
gummodelE <- fitCopula(gumcopula, cbind(u1, u2), method="ml")
summary(gummodelE)
LogLikgumE <- loglikCopula(gummodelE@estimate, cbind(u1, u2), gumcopula) 
BIC <- -2*LogLikgumE+1*log(length(x))
BIC

#ALSO WITH EMPIRICAL CDF THE CLAYTON IS THE BEST COPULA, WITH A BIC OF -1302.877



