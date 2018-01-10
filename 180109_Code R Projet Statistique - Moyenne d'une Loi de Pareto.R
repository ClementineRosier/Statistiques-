#### Question 1
simu<-runif(10000) #Tirage de 10000observation d'une loi uniforme standard

teta=3
c=0.33
simpar<-c/((1-simu)^(1/teta))
hist(simpar)
mean(simpar)

teta=3
c=4
simpar<-c/((1-simu)^(1/teta))
hist(simpar)
mean(simpar)

teta=10
c=4
simpar<-c/((1-simu)^(1/teta))
hist(simpar)
mean(simpar)

##########################
####Simulation des données
n=10000
teta=3
c=1
q=qnorm(0.975) #quantile d'ordre 0.975 d'une loi normale centrée réduite 
set.seed(28)
simu<-runif(10000)
simpar<-c/((1-simu)^(1/teta))
hist(simpar)
mean(simpar)
##########################

##########################
#######Cadre fréquentiste
##########################

#### Maximum de Vraisemblance
#### Question 4
lnsimpar<-log(simpar)
memv<-n/(n-sum(lnsimpar))
# Calcul des bornes de l'intervalle de confiance asymptotique : 
a<-(1/(1-sum(lnsimpar)/n))*(1-q/(sqrt(n)*(n/sum(lnsimpar)-1)))
b<-(1/(1-sum(lnsimpar)/n))*(1+q/(sqrt(n)*(n/sum(lnsimpar)-1)))

#### Méthode des moments
#### Question 4
mean(simpar)
# Calcul des bornes de l'intervalle de confiance asymptotique : 
c<-mean(simpar)-q*sqrt((mean(simpar)*(mean(simpar)-1)^2)/(2*n-sum(simpar)))
d<-mean(simpar)+q*sqrt((mean(simpar)*(mean(simpar)-1)^2)/(2*n-sum(simpar)))


#### Tentative 1

#Question 2
posteriorfunction <- function(m,mu,sigma2){
  return(n*log(m/(m-1))-(m/(m-1))*sum(lnsimpar)-(m-mu)^2/(2*sigma2))
}

#Question 3
proposalfunction <- function(param){
  return(rnorm(1,mean = param, sd= 0.01))
}



###Question 4

mu=1.7
sigma2=0.2

#nous avons exprim� notre fonction en ln, nous pouvons ainsi remplacer le f(Zn)/f()
run_metropolis_MCMC <- function(startvalue, iterations){
  X = array(dim = c(iterations+1,1))
  X[1,] = startvalue
  for (i in 1:iterations){
    Z = proposalfunction(X[i,])
    probab = exp(posteriorfunction(m=Z,mu=mu,sigma2=sigma2) - posteriorfunction(m=X[i,],mu=mu,sigma2=sigma2))
    print(probab)
    if (runif(1) < probab){
      X[i+1,] = Z
    }else{
      X[i+1,] = X[i,]
    }
  }
  return(X)
}

X0<-rnorm(1,mean = mu, sd= sigma2)

X = run_metropolis_MCMC(X0, 10000)

mean(X)
