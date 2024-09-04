#Thème n°2-ACT 7102              Étudiants: Stanislas et David

#?getwd

library("MASS")
  #Pima.
#Pima.te
library(data.table)
DM.dt<-as.data.table(Pima.te)

##### Y'a-t-il une relation linéaire entre bmi et ped?

verif_relation <- lm(bmi~ped,data=DM.dt)
summary(verif_relation)


#Cherchons les marginales

library(dplyr)
var1=DM.dt%>%pull(5)  #création d'un vecteur contenant toutes les valeurs de bmi
var2=DM.dt%>%pull(6)  #création d'un vecteur contenant toutes les valeurs de ped

par(mfrow=c(1,2))  

plot(ecdf(var1))
plot(ecdf(var2))
#title(main="Fonction de répartition empirique du body mass index (bmi) ", cex=0.9)

#il faut la librairie MASS pour pouvoir utiliser fitdistr
library("MASS")

                                                            #########POUR BMI############
{

(pargamma_var1=fitdistr(var1,"gamma")$estimate)   #on trouve les paramètres alpha et bêta. Attention bêta=rate=1/scale
(parexpo_var1=fitdistr(var1,"exponential")$estimate)
(parlognorm_var1=fitdistr(var1,"lognormal")$estimate)
(parnorm_var1=fitdistr(var1,"normal")$estimate)
(parweib_var1=fitdistr(var1,"weibull")$estimate)
(parcauchy_var1=fitdistr(var1,"cauchy")$estimate)

x <- var1

x11() #affichage dans une nouvelle fenêtre
par(mfrow=c(2,3))  #paratger l'écran

plot(ecdf(var1))
curve(pgamma(x,pargamma_var1[1],pargamma_var1[2]),add=TRUE,lwd=2,col="blue")
legend(x=30,y=0.2,legend="Comparaison avec la Fr de la loi gamma",lty=1,lwd=2,col="blue")

plot(ecdf(var1))
curve(pexp(x,parexpo_var1[1]),add=TRUE,lwd=2,col="red")
legend(x=30,y=0.2,legend="Comparaison avec la Fr de la loi expo",lty=1,lwd=2,col="red")

plot(ecdf(var1))
curve(plnorm(x,parlognorm_var1[1],parlognorm_var1[2]),add=TRUE,lwd=2,col="green")
legend(x=30,y=0.2,legend="Comparaison avec la Fr de la loi lnorm",lty=1,lwd=2,col="green")

plot(ecdf(var1))
curve(pnorm(x,parnorm_var1[1],parnorm_var1[2]),add=TRUE,lwd=2,col="orange")
legend(x=30,y=0.2,legend="Comparaison avec la Fr de la loi normale",lty=1,lwd=2,col="orange")

plot(ecdf(var1))
curve(pweibull(x,parweib_var1[1],parweib_var1[2]),add=TRUE,lwd=2,col="brown")
legend(x=30,y=0.2,legend="Comparaison avec la Fr de la loi de Weibull",lty=1,lwd=2,col="brown")

plot(ecdf(var1))
curve(pcauchy(x,parcauchy_var1[1],parcauchy_var1[2]),add=TRUE,lwd=2,col="pink")
legend(x=30,y=0.2,legend="Comparaison avec la Fr de la loi de Cauchy",lty=1,lwd=2,col="pink")

ks.test(x,pgamma,pargamma_var1[1],pargamma_var1[2])
ks.test(x,pexp,parexpo_var1[1])
ks.test(x,plnorm,parlognorm_var1[1],parlognorm_var1[2])
ks.test(x,pnorm,parnorm_var1[1],parnorm_var1[2])
ks.test(x,pweibull,parweib_var1[1],parweib_var1[2])
ks.test(x,pcauchy,parcauchy_var1[1],parcauchy_var1[2])


vrgamma_var1=fitdistr(x,"gamma")$loglik;vrgamma_var1
vnorm_var1=fitdistr(x,"normal")$loglik;vnorm_var1
vlnorm_var1=fitdistr(x,"lognormal")$loglik;vlnorm_var1

}


                                                            #########POUR PED############


{
  
  pargamma_var2=fitdistr(var2,"gamma")$estimate;pargamma_var2
  parexpo_var2=fitdistr(var2,"exponential")$estimate;parexpo_var2
  parlognorm_var2=fitdistr(var2,"lognormal")$estimate;parlognorm_var2
  parnorm_var2=fitdistr(var2,"normal")$estimate;parnorm_var2
  parweib_var2=fitdistr(var2,"weibull")$estimate;parweib_var2
  parcauchy_var2=fitdistr(var2,"cauchy")$estimate;parcauchy_var2 
  
  
  x <- var2
  par(mfrow=c(2,3))
  
  plot(ecdf(var2))
  curve(pgamma(x,pargamma_var2[1],pargamma_var2[2]),add=TRUE,lwd=2,col="blue")
  legend(x=0.5,y=0.2,legend="Comparaison avec la Fr de la loi gamma",lty=1,lwd=3,col="blue")
  
  plot(ecdf(var2))
  curve(pexp(x,parexpo_var2[1]),add=TRUE,lwd=2,col="red")
  legend(x=0.5,y=0.2,legend="Comparaison avec la Fr de la loi expo",lty=1,lwd=3,col="red")
  
  plot(ecdf(var2))
  curve(plnorm(x,parlognorm_var2[1],parlognorm_var2[2]),add=TRUE,lwd=2,col="green")
  legend(x=0.5,y=0.2,legend="Comparaison avec la Fr de la loi lnorm",lty=1,lwd=3,col="green")
  
  plot(ecdf(var2))
  curve(pnorm(x,parnorm_var2[1],parnorm_var2[2]),add=TRUE,lwd=2,col="orange")
  legend(x=0.5,y=0.2,legend="Comparaison avec la Fr de la loi normale",lty=1,lwd=3,col="orange")
  
  plot(ecdf(var2))
  curve(pweibull(x,parweib_var2[1],parweib_var2[2]),add=TRUE,lwd=2,col="brown")
  legend(x=0.5,y=0.2,legend="Comparaison avec la Fr de la loi de Weibull",lty=1,lwd=3,col="brown")
  
  plot(ecdf(var2))
  curve(pcauchy(x,parcauchy_var2[1],parcauchy_var2[2]),add=TRUE,lwd=2,col="pink")
  legend(x=0.5,y=0.2,legend="Comparaison avec la Fr de la loi de Cauchy",lty=1,lwd=3,col="pink")
  
  ks.test(x,pgamma,pargamma_var2[1],pargamma_var2[2])
  ks.test(x,pexp,parexpo_var2[1])
  ks.test(x,plnorm,parlognorm_var2[1],parlognorm_var2[2])
  ks.test(x,pnorm,parnorm_var2[1],parnorm_var2[2])
  ks.test(x,pweibull,parweib_var2[1],parweib_var2[2])
  ks.test(x,pcauchy,parcauchy_var2[1],parcauchy_var2[2])
  
  
  vrgamma_var2=fitdistr(x,"gamma")$loglik;vrgamma_var2
  vweib_var2=fitdistr(x,"weibull")$loglik;vweib_var2
  vlnorm_var2=fitdistr(x,"lognormal")$loglik;vlnorm_var2
  
}

############################DISTRIBUTION CONJOINTE######################################
dev.off();dev.off() #on ferme les fenêtres

##COURBES DES FONCTIONS COPULES


#Définition des copules
?GammaDist

x <- seq(0,100,1)

copula_fgm_generalise <- function(s,t,teta,p){
  a <- s*t*(1+teta*(1-s)*(1-t))^p
  return(a)
}

###########IL FAUT FERMER MAINTENANT LES DEUX FENÊTRES EXTERNES DÉJÀ OUVERTES, puis aller cliquer sur "Clear all plots" juste à droite-->

#Cas p=1, copule FGM tout court
#x11()
plot(copula_fgm_generalise(pgamma(x,shape=21.8241415,scale=0.6565676),pgamma(x,shape=2.584001,scale=4.890340),0.326,1),lwd=0.01, col="black",type ="l",xlab="Plage de valeurs en abscisse",ylab="Fonctions de répartition conjointes")
#plot(copula_fgm_generalise(pgamma(seq(0,1,0.01),shape=21.8241415,scale=0.6565676),pgamma(seq(0,1,0.01),shape=2.584001,scale=4.890340),0.326,1))

#essai avec les fonctions quantiles(cela ne marche naturellement pas puisque nous ne disposons pas de la fonction de répartition conjointe)
#plot(copula_fgm_generalise(qgamma(seq(0,1,0.0005),shape=21.8241415,scale=0.6565676),qgamma(seq(0,1,0.0005),shape=2.584001,scale=4.890340),0.326,1),lwd=0.01, col="black",type ="l",xlab="Plage de valeurs en abscisse",ylab="Fonctions de répartition conjointes")


#Cas p<>1, copule FGM généralisée

lines(copula_fgm_generalise(pgamma(x,shape=21.8241415,scale=0.6565676),pgamma(x,shape=2.584001,scale=4.890340),0.174,2.6854),col="blue",type ="l")

#----------
copula_HK_generalise <- function(s,t,teta,p,alpha){  
  a <- s*t*(1+teta*(1-s^alpha)*(1-t^alpha))^p  
  return(a)  
} 

lines(copula_HK_generalise(pgamma(x,shape=21.8241415,scale=0.6565676),pgamma(x,shape=2.584001,scale=4.890340), p = 0.326, alpha = 0.896, teta = 0.294),col="green",type ="l") 
#----------

copula_BK <- function(s,t,teta,gamma,lambda,p){
  a <- s*t*(1+teta*(1-((s^gamma)^lambda))*(1-((t^gamma)^lambda)))^p
  return(a)
}

lines(copula_BK(pgamma(x,shape=21.8241415,scale=0.6565676),pgamma(x,shape=2.584001,scale=4.890340),0.231,0.926,1.329,1),col="pink",type ="l")
#----------
copula_HK <- function(s,t,teta,p){  
  a <- s*t*(1+teta*(1-s)^p*(1-t)^p)  
  return(a)  
} 

lines(copula_HK(pgamma(x,shape=21.8241415,scale=0.6565676),pgamma(x,shape=2.584001,scale=4.890340), p = 0.326, teta = 0.294),col="yellow",type ="l") 

#--------
copula_LX <- function(s,t,a,b,teta,p){  
  v <- s*t*(1+teta*(s^a)*((1-s)^b)*(t^a)*((1-t)^b))^p  
  return(v)  
} 

lines(copula_LX(pgamma(x,shape=21.8241415,scale=0.6565676),pgamma(x,shape=2.584001,scale=4.890340), a= 1.018, b = 1.018, p = 1, teta = 0.184),col="red",type ="l") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

title(main="Courbes comparatives des 6 modèles de copules FGM", cex=0.9)
legend(x=50,y=0.1,legend="Copule FGM",lty=1,col="black");
legend(x=50,y=0.2,legend="Copule FGM généralisée",lty=1,col="blue")
legend(x=50,y=0.3,legend="Copule Huang and Kotz",lty=1,col="green")
legend(x=50,y=0.4,legend="Copule Bairamov and Kotz",lty=1,col="pink");
legend(x=50,y=0.5,legend="Copule Huang and Kotz généralisée",lty=1,col="yellow")
legend(x=50,y=0.6,legend="Copule Lai and Xie",lty=1,col="red")


###Rho de Spearman

#Page 7. Calul de l'un des deux termes de la borne supérieure

stan <- function(k){
      a <- 0
      for (i in 1:k){a <- a + (1/(i*(i+1)))^2}
      
    return(a)
}
-24-12*stan(11)

4*pi^2-36-12*stan(11)   #On retrouve la valeur de l'article, avec quelques chiffres précis en supplément  0.002311616



                                       ################# ESTIMATIONS DES PARAMÉTRES ##################

#IFM=Inference Functions for Margins


#install.packages("copula")
library(copula)

#

#Définition des données bivariées
data_1 <- cbind(pgamma(var1,shape=21.8241415,rate=0.6565676),pgamma(var2,shape=2.584001,rate=4.890340))  
#ou
data_2 <- cbind(pobs(var1),pobs(var2))  
#"pobs" est utilisé pour convertir les données de chaque variable en valeurs de probabilité empiriques avant d'estimer les paramètres de la copule FGM

#library(MASS)

# Estimation de la copule fgm: 1ère méthode 
{
copula_model <- fgmCopula(dim=2)
fit1 <- fitCopula(copula_model, data=data_1, method = "ml")
fit2 <- fitCopula(copula_model, data=data_2, method = "ml")

(copula_params <- coef(fit1))
(copula_params <- coef(fit2))
}

##On a thêta = 0.2904459

#Estimation des paramètres de la copule fgm simple d'une deuxième manière: on trouve un résultat similaire: thêta = 0.2908512
#L'avantage ici est qu'on peut définir la fonction nous-mêmes puis optimiser

{
  
# Densité de la copule
copula_density <- function(s, t, theta) {
  return(1 + theta * (1 - 2 * s) * (1 - 2 * t))
                                        }

# Log-vraisemblance de la copule
log_likelihood <- function(theta, data) {
  s <- data[,1]
  t <- data[,2]
    sum(log(copula_density(s, t, theta)))
                                        }
# Données simulées
data <- data_1
#data <- data_2
s <- data[,1]
t <- data[,2]

# Estimation de theta par FML avec contraintes
theta_start <- 0  # Valeur initiale pour l'optimisation
estimation <- optim(theta_start, log_likelihood, data=data, method = "Brent", lower=-1, upper=1, control=list(fnscale = -1))

# Résultat de l'estimation
theta_est <- estimation$par
cat("L'estimation de theta par FML avec contraintes:", theta_est, "\n") #la valeur de thêta est 0.2908512 

}

##Estimation des paramètres de la copule fgm généralisée

{
# Fonction de la densité de la copule modifiée
copula_density <- function(s, t, theta, p) {
  u <- 1 + theta * (1 - s) * (1 - t)
  c <- u^p + p*theta*t*(s-1)*u^(p-1) + p*s*(theta*(2*t-1)*u^(p-1) + (theta^2)*(p-1)*(s-1)*(t^2-t)*u^(p-2))
  return(c)
                                           }

# Log-vraisemblance de la copule modifiée
log_likelihood <- function(params, data) {
  theta <- params[1]
  p <- params[2]
  s <- data[,1]
  t <- data[,2]
  sum(-log(copula_density(s, t, theta, p)))
                                         }

# Données 
data <- data_1
s <- data[,1]
t <- data[,2]

# Estimation des paramètres par FML avec contraintes
params_start <- c(0.2, 0.5)  # Valeurs initiales pour l'optimisation
#estimation <- optim(params_start, log_likelihood, data=data, method="Brent", lower=c(-1, 0.1), upper=c(1, #5), #control=list(fnscale=-1))

estimation <- optim(params_start, log_likelihood, data=data, method="CG")

# Résultats de l'estimation
theta_est <- estimation$par[1]
p_est <- estimation$par[2]
cat("Estimation de theta par FML avec contraintes:", theta_est, "\n")
cat("Estimation de p par FML avec contraintes:", p_est, "\n")
}

##on obtient: p = 1.259995 et thêta_fgm_généralisée = 0.2310301 
###############################################################

#Calcul des coefficients de corrélation de Spearman

#Données originales
cor(var1,var2,method="pearson")
cor(var1,var2,method="spearman")  #valeur 0.09697264
#ou encore cov(var1,var2)/(sd(var1)*sd(var2))

?cor
#Copule FGM

theta <- 0.2908512
theta/3            #coef de corrélation de spearman = 0.0969504


#Copule FGM généralisée

k <- seq(1, 1.259995,1)   #(on n'oublie pas: p_estimé = 1.259995)
theta_fgm_généralisée = 0.2310301
12*sum(choose(1.259995,k)*((theta_fgm_généralisée^k)*(((1/(k+1))*(1/(k+2)))^2)))   #on a le coefficient de corrélation de Spearman = 0.09703226


#

choose(100,100)


#Exemples de l'article
#on a construit ceci juste pour avoir une idée de l'évolution de la courbe représentative de certaines fonctions,
#ce qui nous a aidés dans nos démonstrations

{

#Exemple 2

x <- seq(0,1,0.0001)
courbe1 <- x*(1-2*x^2)
plot(courbe1,ylim=c(-1,1),type="l")

courbe2 <- (x*(1-2*x^2))^2
lines(courbe2,col="green")


fc <- function(x){
  return((x-2*x^3)^2)
}

fc2 <- function(x){
  return(exp(-2*x)-exp(x)+1)
}
plot(fc2(x))

}


