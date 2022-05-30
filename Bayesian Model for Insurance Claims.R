##Initial Set-up Part A (Question 1 to Question 3)
##read simulated data for number of claims for each policy, 183,999 policies in total
Pclaim<- read.csv("norauto.csv")

#list given variables of data sets
names(Pclaim)

#check the structure of data sets
str(Pclaim)

#create a new data set of first 100 policies
Pclaim100 <- Pclaim[1:100,]
Pclaim100

#valuate alpha and beta which are calculated in the report
alpha <- 0.018
beta <- 5/3

#Question 1: evaluate mean & sd for 183,999 policies and first 100 policies
#valuate alpha_n and beta_n 
alpha_n <- sum(Pclaim$NbClaim)+alpha
alpha_n 
beta_n <-  beta/(1+beta*sum(Pclaim$Expo))
beta_n

#in the case of 183,999 policies
mu1 <- alpha_n*beta_n
mu1
sd1 <- beta_n * sqrt(alpha_n)
sd1

#valuate alpha_100 and beta_100
alpha_100 <- sum(Pclaim100$NbClaim)+alpha
alpha_100
beta_100 <-  beta/(1+beta*sum(Pclaim100$Expo))
beta_100

#in the case of first 100 policies
mu2 <- alpha_100*beta_100
mu2
sd2 <- beta_100 * sqrt(alpha_100)
sd2

#Question 2: Plot the Bayesian posterior density for 183,999 policies and 100 policies
c1 <- seq(mu1-3*sd1, mu1+3*sd1, 0.01*sd1)
pdf1<- dgamma(c1, shape = alpha_n, scale = beta_n, log = FALSE)

c2 <- seq(mu2-3*sd2, mu2+3*sd2, 0.01*sd2)
pdf2<- dgamma(c2, shape = alpha_100, scale = beta_100, log = FALSE)


par(mfrow=c(1,2))

plot(c1, pdf1, col="blue",xlab="", ylab="Prob. Density", type="l",main="PDF for all policies")
plot(c2, pdf2, col="purple",xlab="", ylab="Prob. Density", type="l",main="PDF for first 100 policies")

#Compute 90% Bayesian confidence interval for λ
#in the case of 183,999 policies
BCILower1<- mu1 - qnorm(0.95)*sd1
BCIUpper1<- mu1 + qnorm(0.95)*sd1
BCI1<-c(BCILower1, BCIUpper1)
BCI1

#in the case of first 100 policies
BCILower2<- mu2 - qnorm(0.95)*sd2
BCIUpper2<- mu2 + qnorm(0.95)*sd2
BCI2<-c(BCILower2, BCIUpper2)
BCI2

#Question 3: evaluate the credibility estimator of λ and credibility factor for 183,999 policies and 100 policies

pmu1 <- 0.03
psd1 <- 0.05

##in the case of 183,999 policies
z1 <- beta_n*sum(Pclaim$Expo)
z1
lamda_mle1 <- sum(Pclaim$NbClaim)/sum(Pclaim$Expo)
lamda_mle1

CRlamda1 <- pmu1*(1-z1)+lamda_mle1*z1
CRlamda1

##in the case of 100 policies
z2 <- beta_100*sum(Pclaim100$Expo)
z2
lamda_mle2 <- sum(Pclaim100$NbClaim)/sum(Pclaim100$Expo)
lamda_mle2
CRlamda2 <- pmu1*(1-z2)+lamda_mle2*z2
CRlamda2

##Initial Set-up Part B (Question 4 to Question 6)
#Obmit null data based on Claim Amount
Pclaim1 <- Pclaim[Pclaim$ClaimAmount != 0,]

#create new dataframe for first 100 policies
Pclaim1_100 <- Pclaim1[1:100,]

#calculate the log of claim amount
log_ClaimAmount1 <- log(Pclaim1$ClaimAmount)
log_ClaimAmount2 <- log(Pclaim1_100$ClaimAmount)

sd3 <- 1.2
pmu2 <- 6.0
tau <- 4.0^2

#Question 4: Compute the posterior mean and standard deviation of the unknown mean parameter Θ
#in case of 8,444 policies
n1 <- length(Pclaim1$ClaimAmount)
taupost1<-1/(n1/(sd3^2) + 1/tau)
sd_taupost1 <- sqrt(taupost1)
sd_taupost1
mupost1 <- taupost1*(pmu2/tau + n1*mean(log_ClaimAmount1)/sd3^2)
mupost1

#in case of 100 policies
n2 <- length(Pclaim1_100$ClaimAmount)
taupost2 <-1/(n2/(sd3^2) + 1/tau)
sd_taupost2 <- sqrt(taupost2)
sd_taupost2
mupost2 <- taupost2*(pmu2/tau + n2*mean(log_ClaimAmount2)/sd3^2)
mupost2
                  
#Question 5: Plot the posterior and to compute a 90% Bayesian confidence interval for Θ

#in case of 8,444 policies
c3<-seq(mupost1-3*sqrt(taupost1),mupost1+3*sqrt(taupost1), 0.01*sqrt(taupost1))
pdf3<-dnorm(c3, mupost1, sqrt(taupost1))

#in case of 100 policies
c4<-seq(mupost2-3*sqrt(taupost2),mupost2+3*sqrt(taupost2), 0.01*sqrt(taupost2))
pdf4<-dnorm(c4, mupost2, sqrt(taupost2))

#plot the posterior for 2 cases
par(mfrow=c(1,2))

plot(c3, pdf3, col="green",xlab="", ylab="Prob. Density", type="l", main="PDF for 8444 policies")
plot(c4, pdf4, col="orange",xlab="", ylab="Prob. Density", type="l",main="PDF for first 100 policies")

#Compute a 90% Bayesian confidence interval for 8444 pocilies and 100 first policies
#in case of 8,444 policies
BCILower3<- mupost1 - qnorm(0.95)*sqrt(taupost1)
BCIUpper3<- mupost1 + qnorm(0.95)*sqrt(taupost1)
BCI3<-c(BCILower3, BCIUpper3)
BCI3

#in case of 100 policies
BCILower4<- mupost2 - qnorm(0.95)*sqrt(taupost2)
BCIUpper4<- mupost2 + qnorm(0.95)*sqrt(taupost2)
BCI4<-c(BCILower4, BCIUpper4)
BCI4

#Question 6: evaluate the credibility estimator of λ and credibility factor for 183,999 policies and 100 policies

sd3 <- 1.2
pmu2 <- 6.0
tau <- 4.0^2

##in the case of 8,444 policies
z3 <- (n1*tau)/(n1*tau+sd3^2)
z3
CRlamda3 <- z3*mean(log_ClaimAmount1) + (1-z3)*pmu2
CRlamda3

##in the case of 100 policies
z4 <- (n2*tau)/(n2*tau+sd3^2)
z4
CRlamda4 <- z4*mean(log_ClaimAmount2) + (1-z4)*pmu2
CRlamda4
