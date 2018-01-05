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

