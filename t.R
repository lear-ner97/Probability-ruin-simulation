




lambda=10      #nombre de sinistres par jour
r=0.1     #taux de profit net
m_Z=90
c=(1+r)*lambda*m_Z
u=0
T_max=365
nbre_simul=1000
nbre_ruine=0 #nbre de ruines initialisé à 0
for (i in 1:nbre_simul){

  T=0
  R=u
  ruine=FALSE   #booléen pour contrôler la ruine
  
  while(T<=T_max & ruine==FALSE){
    delta=rexp(1,lambda)
    T=T+delta
    #Z=m_Z
    #Z=rexp(1,1/m_Z)
    Z=rpareto(1, 2, m_Z)
    R=R+c*delta-Z
    if (R<0){
      ruine=TRUE
      nbre_ruine=nbre_ruine+1
    }
  
  }

}
proba_ruine=nbre_ruine/nbre_simul #proba de ruine
variance=proba_ruine-proba_ruine^2







#1/L'inconvénient: erreur d'estimation + complexité de l'algorithme en temps de calcul : très lent quand 
#on veut représenter psi=f(u)



#II.1/
#a/ sont très proches
#b/ de même
#c/de même


















