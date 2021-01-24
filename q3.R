
les_u=seq(0,5000,by=1000)  #vecteur de provisionnement
les_temps=c()  #vecteur des temps de ruine moyens
lambda=10      #nombre de sinistres par jour
r=0.1     #taux de profit net
m_Z=90
c=(1+r)*lambda*m_Z
T_max=365
nbre_simul=1000
variance=c() #vecteur des variances
deficit=c()
for (j in 1:length(les_u)){
  #la proba de ruine pour j
  
  #  ruine :      booléen pour contrôler la ruine
  u=les_u[j]
  temps_ruine=0 #temps de ruine moyen initialisé à 0
  nbre_ruine=0 #nbre de ruines initialisé à 0
  les_ruines=c() #sert pour calculer la variance
  variance[j]=0
  deficit[j]=0
  for  (i in 1:nbre_simul){
    T=0
    R=u
    ruine=FALSE 
    while(T<=T_max&ruine==FALSE){
      delta=rexp(1,lambda)
      T=T+delta
     
      #Z=m_Z
      #Z=rexp(1,1/m_Z)
      Z=rpareto(1, 2,m_Z)
      R=R+c*delta-Z
      if (R<0){
        ruine=TRUE
        nbre_ruine=nbre_ruine+1
        temps_ruine=temps_ruine+T
        les_ruines[nbre_ruine]=T
        deficit[j]=deficit[j]+R
      }

    }
    
  }
  deficit[j]=-deficit[j]/nbre_ruine
  temps_ruine=temps_ruine/nbre_ruine #temps de ruine moyen
  les_temps[j]=temps_ruine
  #calcul de la variance
  for (i in 1:nbre_ruine){
    variance[j]=variance[j]+(les_ruines[i]-temps_ruine)^2
  }
  variance[j]=variance[j]/nbre_ruine
  
}


plot(les_u,les_temps,type='l',col='green',xlab='provision',ylab='temps de ruine moyen',main = "temps de ruine moyen
     en fonction du provisionnment")
lines(les_u,les_temps,type='l',col='red')
legend("topright", legend=c("exponentiel", "constante","Pareto"), col=c("blue", "red","green"), lty=1:3, cex=0.8)

