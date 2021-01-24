les_u=seq(0,3000,by=100)  #vecteur de provisionnement
les_ruines=c()  #vecteur de probabilités de ruine
lambda=10      #nombre de sinistres par jour
r=0.1     #taux de profit net
m_Z=90
c=(1+r)*lambda*m_Z
T_max=365
nbre_simul=1000
#déficit=c()#le déficit moyen de l'assurance après la ruine
variance=c()
for (j in 1:length(les_u)){
  #la proba de ruine pour j
  
  #  ruine :      booléen pour contrôler la ruine
  u=les_u[j]
  nbre_ruine=0 #nbre de ruines initialisé à 0
  #déficit[j]=0
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
        #déficit[j]=déficit[j]+R
      }
  
    }
   
  }
  proba_ruine=nbre_ruine/nbre_simul
  les_ruines[j]=proba_ruine
  #déficit[j]=déficit[j]/nbre_ruine
  variance[j]=proba_ruine-proba_ruine^2
}

plot(les_u,les_ruines,type='l',col='blue',xlab='provision',ylab='probabilité de ruine',main = "Probabilité de ruine
     en fonction du provisionnment")
lines(les_u,les_ruines,type='l',col='red')
plot(les_u,déficit,type='l')
legend("topright", legend=c("exponentiel", "constante","Pareto"), col=c("blue", "red","green"), lty=1:3, cex=0.8)
###################################################

i=1
psi=c()

while (i<=length(les_u)){ 
  psi[i]=1/(1+r)*exp(-r*les_u[i]/m_Z/(1+r))
  i=i+1
}
lines(les_u,psi,type='l',col='red')

legend("topright", legend=c("expérimental", "théorique"), col=c("blue", "red"), lty=1:2, cex=0.8)
