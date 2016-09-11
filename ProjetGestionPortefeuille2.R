# Importation des library
library(tseries)
library(MASS)

# Inputs, période sur laquelle on va étudier les données
t0 = "2005-03-14"
t1 = "2009-03-04"

#liste 100 valeurs
cours=c("ABT", "ACN", "ACE", "ACT", "ADBE", "AES", "AET", "AFL", "A", "GAS", "APD", "ARG", "AKAM", "AA", "ALXN", "ATI", "AGN", "ADS", "ALL", "ALTR", "MO", "AMZN", "AEE", "AEP", "AXP", "AMT", "ABC", "AME", "AMGN", "APH", "APC", "ADI", "AON", "APA", "AIV", "AAPL", "AMAT", "ADM", "AIZ", "T", "ADSK", "ADP", "AN", "AZO", "AVB", "AVY", "AVP", "BHI", "BLL", "BCR", "BAX", "BBT", "BEAM", "BDX", "BBBY", "BMS", "BRK-B","BIIB", "BLK", "HRB", "BA", "BWA", "BXP", "BSX", "BMY", "BRCM", "BF-B", "CA", "CVC", "COG", "CAM", "CPB", "COF", "CAH", "KMX", "CCL", "CAT", "CBG", "CELG", "CNP", "CTL", "CERN", "CHRW", "CHK", "CVX", "CB", "CI", "CINF", "CTAS", "CSCO", "C", "CTXS", "CLF", "CLX", "CME",  "CMS", "CCE", "CTSH", "CL", "CMCSA")

# Initialisation des matrices
SERIESO<-matrix(nrow=1001,ncol=length(cours))
SERIESC<-matrix(nrow=1001,ncol=length(cours))
SERIESmean<-matrix(nrow=1001,ncol=length(cours))

# Importation et construction des données importees YAHOO FINANCE
for (i in 1:length(cours)) {
  SERIESO[,i]=get.hist.quote(instrument=cours[i],start=t0,end =t1,compression="d",quote="Open")
  SERIESC[,i]=get.hist.quote(instrument=cours[i],start=t0,end =t1,compression="d",quote="Close")
}
colnames(SERIESO)<-c("ABT", "ACN", "ACE", "ACT", "ADBE", "AES", "AET", "AFL", "A", "GAS", "APD", "ARG", "AKAM", "AA", "ALXN", "ATI", "AGN", "ADS", "ALL", "ALTR", "MO", "AMZN", "AEE", "AEP", "AXP", "AMT", "ABC", "AME", "AMGN", "APH", "APC", "ADI", "AON", "APA", "AIV", "AAPL", "AMAT", "ADM", "AIZ", "T", "ADSK", "ADP", "AN", "AZO", "AVB", "AVY", "AVP", "BHI", "BLL", "BCR", "BAX", "BBT", "BEAM", "BDX", "BBBY", "BMS", "BRK-B","BIIB", "BLK", "HRB", "BA", "BWA", "BXP", "BSX", "BMY", "BRCM", "BF-B", "CA", "CVC", "COG", "CAM", "CPB", "COF", "CAH", "KMX", "CCL", "CAT", "CBG", "CELG", "CNP", "CTL", "CERN", "CHRW", "CHK", "CVX", "CB", "CI", "CINF", "CTAS", "CSCO", "C", "CTXS", "CLF", "CLX", "CME",  "CMS", "CCE", "CTSH", "CL", "CMCSA")

colnames(SERIESC)<-c("ABT", "ACN", "ACE", "ACT", "ADBE", "AES", "AET", "AFL", "A", "GAS", "APD", "ARG", "AKAM", "AA", "ALXN", "ATI", "AGN", "ADS", "ALL", "ALTR", "MO", "AMZN", "AEE", "AEP", "AXP", "AMT", "ABC", "AME", "AMGN", "APH", "APC", "ADI", "AON", "APA", "AIV", "AAPL", "AMAT", "ADM", "AIZ", "T", "ADSK", "ADP", "AN", "AZO", "AVB", "AVY", "AVP", "BHI", "BLL", "BCR", "BAX", "BBT", "BEAM", "BDX", "BBBY", "BMS", "BRK-B","BIIB", "BLK", "HRB", "BA", "BWA", "BXP", "BSX", "BMY", "BRCM", "BF-B", "CA", "CVC", "COG", "CAM", "CPB", "COF", "CAH", "KMX", "CCL", "CAT", "CBG", "CELG", "CNP", "CTL", "CERN", "CHRW", "CHK", "CVX", "CB", "CI", "CINF", "CTAS", "CSCO", "C", "CTXS", "CLF", "CLX", "CME",  "CMS", "CCE", "CTSH", "CL", "CMCSA")

# Moyenne des 2 matrices: valeur haute et valeur basse
SERIESmean=(SERIESO+SERIESC)/2

# Construction des log rendements arithmetique Rt-Rt-1/Rt-1
rSERIES=t(diff(log(SERIESmean)))

# Calcul des matrices de covariance empirique, Separation en deux matrices
E1=rSERIES[1:100,1:500]%*%t(rSERIES[1:100,1:500])/(dim(rSERIES)[2]/2)
E2=rSERIES[1:100,501:1000]%*%t(rSERIES[1:100,501:1000])/(dim(rSERIES)[2]/2)

# Calcul des matrices inverses à l'aide du package MASS 
E1Inv=ginv(E1)
E2Inv=ginv(E2)
colnames(E1Inv)<-c("ABT", "ACN", "ACE", "ACT", "ADBE", "AES", "AET", "AFL", "A", "GAS", "APD", "ARG", "AKAM", "AA", "ALXN", "ATI", "AGN", "ADS", "ALL", "ALTR", "MO", "AMZN", "AEE", "AEP", "AXP", "AMT", "ABC", "AME", "AMGN", "APH", "APC", "ADI", "AON", "APA", "AIV", "AAPL", "AMAT", "ADM", "AIZ", "T", "ADSK", "ADP", "AN", "AZO", "AVB", "AVY", "AVP", "BHI", "BLL", "BCR", "BAX", "BBT", "BEAM", "BDX", "BBBY", "BMS", "BRK-B","BIIB", "BLK", "HRB", "BA", "BWA", "BXP", "BSX", "BMY", "BRCM", "BF-B", "CA", "CVC", "COG", "CAM", "CPB", "COF", "CAH", "KMX", "CCL", "CAT", "CBG", "CELG", "CNP", "CTL", "CERN", "CHRW", "CHK", "CVX", "CB", "CI", "CINF", "CTAS", "CSCO", "C", "CTXS", "CLF", "CLX", "CME",  "CMS", "CCE", "CTSH", "CL", "CMCSA")
colnames(E2Inv)<-c("ABT", "ACN", "ACE", "ACT", "ADBE", "AES", "AET", "AFL", "A", "GAS", "APD", "ARG", "AKAM", "AA", "ALXN", "ATI", "AGN", "ADS", "ALL", "ALTR", "MO", "AMZN", "AEE", "AEP", "AXP", "AMT", "ABC", "AME", "AMGN", "APH", "APC", "ADI", "AON", "APA", "AIV", "AAPL", "AMAT", "ADM", "AIZ", "T", "ADSK", "ADP", "AN", "AZO", "AVB", "AVY", "AVP", "BHI", "BLL", "BCR", "BAX", "BBT", "BEAM", "BDX", "BBBY", "BMS", "BRK-B","BIIB", "BLK", "HRB", "BA", "BWA", "BXP", "BSX", "BMY", "BRCM", "BF-B", "CA", "CVC", "COG", "CAM", "CPB", "COF", "CAH", "KMX", "CCL", "CAT", "CBG", "CELG", "CNP", "CTL", "CERN", "CHRW", "CHK", "CVX", "CB", "CI", "CINF", "CTAS", "CSCO", "C", "CTXS", "CLF", "CLX", "CME",  "CMS", "CCE", "CTSH", "CL", "CMCSA")

# Calculs des esperances de la matrice des rendements
m1<-matrix(nrow=100,ncol=1)
#m2<-matrix(nrow=100,ncol=1)
for (i in 1:100)
{
  m1[i]=mean(rSERIES[i,1:500])
  #m2[i]=mean(rSERIES[i,501:1000])
}
#on remplace m2 par g
rendderncol=rSERIES[1:100,1000:1000] #on prend la dernière colonne des rendements
sompondrendderncol=0
for(i in 1:100)
{
  sompondrendderncol=sompondrendderncol+((rendderncol[i])*(rendderncol[i]))
}
g<-c()
for(i in 1:100)
{
  #g[i]<-rendderncol[i]/sompondrendderncol
  g[i]<-rendderncol[i]
}
# Implementation de Markowitz
# INITIALISATION
u<-c(1:100)/100
w1<-c(1:100)
w2e<-c(1:100)
w2<-c(1:100)
Rinsample<-c(nrow=length(u))
Routsample<-c(nrow=length(u))
Rtrue<-c(nrow=length(u))

for(i in 1:100)
{
  w1<-u[i]*(E1Inv%*%m1)/(t(m1)%*%E1Inv%*%m1)[1,1] # W1 Estime
  #w2e<-u[i]*(E1Inv%*%m2)/(t(m2)%*%E1Inv%*%m2)[1,1] # W2 Estime
  #w2<-u[i]*(E2Inv%*%m2)/(t(m2)%*%E2Inv%*%m2)[1,1] # pour calcule R^2 true
  w2e<-u[i]*(E1Inv%*%g)/(t(g)%*%E1Inv%*%g)[1,1] # W2 Estime
  w2<-u[i]*(E2Inv%*%g)/(t(g)%*%E2Inv%*%g)[1,1] # pour calcule R^2 true
  
  # Variance
  #Rinsample[i]=t(w1)%*%E1%*%w1
  Routsample[i]=t(w2e)%*%E2%*%w2e
  Rtrue[i]=t(w2)%*%E2%*%w2
}

# Graphiques
#plot(Rinsample,u, col="green", type="l") pas necessaire car pas la même période que les deux autres 
plot(Routsample,u,col="red",type="l",main= "Variance en fonction de la moyenne, sans méthode de nétoyage",xlab= "variance")
lines(Rtrue,u,col="darkblue")
legend("topright", legend = c("Rtrue","Routsample"), col = c("darkblue", "red"), pch = "-", pt.cex = 1.5,  text.col = "forestgreen")


# Clipping: Methode de nettoyage (se rapproche plus de la courbe bleu)

rho=diag(E1)^(-1/2)*E1*diag(E1)^(-1/2)
# Decomposition valeurs propres
rhovp=eigen(rho)$values
# Decomposition vecteurs propres
rhovectorp=eigen(rho)$vectors

q = length(cours)/500
lambda_p = (1+sqrt(q))^2

# Ancienne longueur
old_length=length(rhovp)

# Filtre
rhovp=rhovp[rhovp >= lambda_p]

new_length=length(rhovp)

# Conserver la TR(Matrice) cad Tr(cov)=n et trouver constante
valeur<-(old_length-sum(rhovp))/(old_length-length(rhovp))
for (i in (new_length+1):old_length)
{
  rhovp[i]<-valeur
}
# On se remet en matrice de correlation O*lambda*inv(O)
rho=rhovectorp%*%diag(rhovp)%*%ginv(rhovectorp)

# Matrice de correlation vers matrice de covariance
sig=diag(E1)

# refaire markovitz
E1clip=sig^(1/2)*rho*(sig)^(1/2)

# Calcul des matrices inverses
E1Invclip=ginv(E1clip)


Routsampleclip<-c()
for(i in 1:100)
{

  #calcul Variance
  w2eclip<-u[i]*(E1Invclip%*%g)/(t(g)%*%E1Invclip%*%g)[1,1] # W2 Estime
  Routsampleclip[i]=t(w2eclip)%*%E2%*%w2eclip
  
}

plot(Routsample,u,col="red",type="l",main= "Variance en fonction de la moyenne, avec clipping",xlab= "variance")
lines(Rtrue,u,col="darkblue")
lines(Routsampleclip,u,col="green")
legend("topright", legend = c("Rtrue","Routsample","Routsampleclip"), col = c("darkblue", "red","green"), pch = "-", pt.cex = 1.5,  text.col = "forestgreen")


#Shrinkage
id=diag(1,nrow=100)
variance_alpha=c()
vecId<-t(t(rep(1,100)))
#mu etoile fixé a 0.5
for ( i in 1:100)
{
  alpha=i/100
  rho<-alpha*id+(1-alpha)*cor(E1)
  Ecbis<-diag(E1)^(1/2)*rho*diag(E1)^(1/2)
  EcbisInv<-ginv(Ecbis)
  wECe<-(EcbisInv%*%vecId)/(t(vecId)%*%EcbisInv%*%vecId)[1,1]
  variance_alpha[i] = t(wECe)%*%E2%*%wECe
  
}
plot(variance_alpha,col="black")
# Trouver la variance de alpha qui est minimum
variance_alpha_min = min(variance_alpha)
# On cherche alpha minimum
for (i in 1:100)
{
  if (variance_alpha[i] == variance_alpha_min)
  {
    alpha_min = i/100
  }
}
#on va remplacer l'alpha min et le réinjecter dans les equations

alphaE=alpha_min
RoutsampleShrink=c()
for ( i in 1:100)
{
  rhoE<-alphaE*id+(1-alphaE)*cor(E1)
  EcbisE<-diag(E1)^(1/2)*rhoE*diag(E1)^(1/2)
  EcbisEInv<-ginv(EcbisE)
  #w2EShrink<-u[i]*(EcbisEInv%*%m2)/(t(m2)%*%EcbisEInv%*%m2)[1,1]
  w2EShrink<-u[i]*(EcbisEInv%*%g)/(t(g)%*%EcbisEInv%*%g)[1,1]
  RoutsampleShrink[i]=t(w2EShrink)%*%E2%*%w2EShrink
}
plot(RoutsampleShrink,u,col="black",type="l",main= "Variance en fonction de la moyenne, avec shrinkage",xlab= "variance")
lines(Routsample,u,col="red",type="l")
lines(Rtrue,u,col="darkblue")
legend("topright", legend = c("Rtrue","Routsample","RoutsampleShrink"), col = c("darkblue", "red","black"), pch = "-", pt.cex = 1.5,  text.col = "forestgreen")

#méthode utilisant la loi de puissance
rhoMP=diag(E1)^(-1/2)*E1*diag(E1)^(-1/2)
#E1valp=eigen(E1)$values
#E1vectp=eigen(E1)$vectors
E1valp=eigen(rhoMP)$values
E1vectp=eigen(rhoMP)$vectors
lambda<-c()
#alfa = 0,71
for ( k in 1:100) #calcul lambda
{
  alphaMP=0.71
  E1valp[k]<-2*alphaMP-1+(1-alphaMP)*sqrt(100/k)
}  
# On se remet en matrice de correlation O*lambda*inv(O)
rho=E1vectp%*%diag(E1valp)%*%ginv(E1vectp)
# Matrice de correlation vers matrice de covariance
sig=diag(E1)
# refaire markovitz
E1MP=sig^(1/2)*rho*(sig)^(1/2)
# Calcul des matrices inverses
E1InvMP=ginv(E1MP)
RoutsampleMP<-c()
for(i in 1:100)
{
  #calcul Variance
  w2eMP<-u[i]*(E1InvMP%*%g)/(t(g)%*%E1InvMP%*%g)[1,1] # W2 Estime
  RoutsampleMP[i]=t(w2eMP)%*%E2%*%w2eMP
  
}
plot(Routsample,u,col="red",type="l",main= "Variance en fonction de la moyenne, avec clipping",xlab= "variance")
lines(Rtrue,u,col="darkblue")
lines(RoutsampleMP,u,col="green")
legend("topright", legend = c("Rtrue","Routsample","RoutsampleMP"), col = c("darkblue", "red","green"), pch = "-", pt.cex = 1.5,  text.col = "forestgreen")

#plot final
plot(RoutsampleShrink,u,col="black",type="l",main= "Variance en fonction de la moyenne, avec clipping et shrinkage",xlab= "variance")
lines(Routsample,u,col="red")
lines(Rtrue,u,col="darkblue")
lines(RoutsampleMP,u,col="brown")
lines(Routsampleclip,u,col="green")
legend("bottomright",cex = 0.8, legend = c("Rtrue","Routsample","RoutsampleShrink", "Routsampleclip","RoutSampleLoiPuiss"), col = c("darkblue", "red","black","green","brown"), pch = "-", pt.cex = 1.5,  text.col = "forestgreen")

