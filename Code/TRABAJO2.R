#--------parametros del modelo
n=6000
sigma=1
u=0
#--------simulacion del modelo
set.seed(123)
Un = rnorm(n,u,sigma)
#Ho: mu=0
Y <- sample(Un,n)
install.packages("geoR")
library(geoR)

for (i in 1:2500){
U=Y %*% Y
lamda <- (1/2)*(u %*% u)
X2 <- qchisq(0.95, n, ncp = lamda)
res [i,] <- 
}

mu=0
for (i in 1:2500){
  X2_obs <- Y %*% Y
  lamda <- (1/2)*(mu %*% mu)
  X2 <- qchisq(0.95, n, ncp = lamda, lower.tail = TRUE, log.p = FALSE)
  Resultado[i,"Mu"] <- mu
  Resultado[i,"Xobs"] <- X2_obs
  Resultado[i,"Xteo"] <- X2
  Resultado[i,"Rechaza"] <- X2_obs > X2
  mu=mu+1
}


#Xn = integer(n) #puedo poner double

An = rgamma(n,shape=a,scale=b) #iid Gamma
Zn = rnorm(n,u,sigma)

Xn[1] = 2 # es un valor arbitrario
# Uno puede hacer calculos y pueden ser solicitados en el trabajo.
#Xn = AnXn-1 +Zn E[Xn] = E[An]E[Xn-1]+E[Zn]

for(j in 2:n){
  Xn[j] = An[j]*Xn[j-1]+Zn[j]
}

alpha=0.25
colors <- c("blue","red","green","yellow")
j <- 1
while (alpha>=0.05){
  Zc <- qchisq(1-alpha,1)
  mu_1 <- -1
  i <- 1
  pi <- NA
  mu <- NA
  while (mu_1<=1){
    delta=n*(mu_1-mu_0)^2/sd(Xn)
    mu[i] <- mu_1
    pi[i] <- 1-pchisq(Zc, 1, ncp=delta)
    i <- i + 1
    mu_1 <- mu_1 + 0.1
  }
  plot(mu,pi,type = "l", col = colors[j])
  axis(side=2, at=seq(0, 1, by=0.2))
  par(new=TRUE)
  j <- j + 1
  alpha <- alpha - 0.05
}


