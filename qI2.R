les_u=seq(0,3000,by=100)  #vecteur de provisionnement
les_ruines=c()  #vecteur de probabilit�s de ruine
lambda=10      #nombre de sinistres par jour
r=0.1     #taux de profit net
m_Z=90
c=(1+r)*lambda*m_Z
T_max=365
nbre_simul=1000
#d�ficit=c()#le d�ficit moyen de l'assurance apr�s la ruine
variance=c()
for (j in 1:length(les_u)){
  #la proba de ruine pour j
  
  #  ruine :      bool�en pour contr�ler la ruine
  u=les_u[j]
  nbre_ruine=0 #nbre de ruines initialis� � 0
  #d�ficit[j]=0
  variance[j]=0
  for  (i in 1:nbre_simul){
  
    T=0
    R=u
    ruine=FALSE 
    
    while(T<=T_max&ruine==FALSE){
      delta=rexp(1,lambda)
      T=T+delta
      
      Z=m_Z
      #Z=rexp(1,1/m_Z)
      #Z=rpareto(1,2,m_Z)
      
      R=R+c*delta-Z
      
      if (R<0){
        ruine=TRUE
        nbre_ruine=nbre_ruine+1
        #d�ficit[j]=d�ficit[j]+R
      }
  
    }
   
  }
  proba_ruine=nbre_ruine/nbre_simul
  les_ruines[j]=proba_ruine
  #d�ficit[j]=d�ficit[j]/nbre_ruine
  variance[j]=proba_ruine-proba_ruine^2
}

plot(les_u,les_ruines,type='l',col='blue',xlab='provision',ylab='probabilit� de ruine',main = "Probabilit� de ruine
     en fonction du provisionnment")
lines(les_u,les_ruines,type='l',col='red')
plot(les_u,d�ficit,type='l')
legend("topright", legend=c("exponentiel", "constante","Pareto"), col=c("blue", "red","green"), lty=1:3, cex=0.8)
###################################################

i=1
psi=c()

while (i<=length(les_u)){ 
  psi[i]=1/(1+r)*exp(-r*les_u[i]/m_Z/(1+r))
  i=i+1
}
lines(les_u,psi,type='l',col='red')

legend("topright", legend=c("exp�rimental", "th�orique"), col=c("blue", "red"), lty=1:2, cex=0.8)
