tab=read.table('Ronfle.txt',header=T)


#la variable cible soit être quanti
tab$ronfle=as.factor(tab$ronfle)

#on regarde si les classes sont bien équilibrées
table(tab$ronfle) #normal que ce soit déséquilibré

plot(tab)

modele1 = glm(ronfle~age,tab,family="binomial")
summary(modele1)


#logit(p(x))=0.5*age-3.11 
#     -> p(x)=1/1+exp(3.11-0.5*age)



a0 = modele1$coefficient[1]
a1 = modele1$coefficient[2]
x=seq(from=20,to=80,by=1)

px=1/(1+exp(-a0-a1*x))
plot(x,px,type='l',lwd=2,xlab='age',ylab="p(x)",ylim=c(0,1))
abline(h=0.5,col='red',lwd=2,lty=2)

#lnodds = 3.11+0.05x
#odds(x)=exp(3.11+0.05x)
#odds(80)=exp(3.11+0.05*80) = 1.9 
#on doit avoir odds>1 -> une personne de 80 ans a 1.9 fois + de chance de ronfler que de ne pas ronfler 

lnodds = 3.11+0.05*x
oddsx=exp(3.11+0.05*x)
odds80=exp(3.11+0.05*80)
print(odds80)

#Question e)

#OR = odd1/odd0  pour un accroissement d'une année 
#OR = odd(x+1)/odd(x) = exp(a1)  (cf cours)
# = exp(0.05) = 1.05

OR = exp(a1)
print(OR)
#pour un accroissement de 1an, le risque de ronfler est multiplié par 1.05
#pour un accroissment de 10 ans :
#OR = odd(x+10)/odd(x) = exp(a1*10) = 1.6
OR10 = exp(a1 * 10)
print(OR10)


modele2 = glm(ronfle~sexe,tab,family="binomial")
summary(modele2)


#procédure step
step(modele1,modele2)

modele=glm(ronfle~age+poids+taille+alcool+sexe+tabac,data=tab,family='binomial')
summary(modele)
#la variable la moins significative est la taille car z value la plus faible-> modele=glm(ronfle~age+poids+alcool+sexe+tabac,data=tab,family='binomial')
#puis on enlève le poids -> modele=glm(ronfle~age+alcool+sexe+tabac,data=tab,family='binomial')

#modele : Ronfle = -4.25+0.06*age+2.37*1_alcool=I+1.21*1_alcool=M-1.17*1_tabac=0


#on a fait la procédure pas à pas mais on peut le faire directement grace àla fonction step
step(modele)
#puis le sexe -> modele=glm(ronfle~age+alcool+tabac,data=tab,family='binomial')
#ça supprime la variable avec la plus petite AIC -> d'abord taille puis recalcul de l'AIC et supprime le poids....
#au final, elle retourne e modèle qu'elle obtient


#on imagine des interactions possibles entre les variables avec le *

modelebis=glm(ronfle~age+poids+taille+alcool+sexe+tabac + age*tabac,data=tab,family='binomial')
summary(modelebis)
step(modelebis)
#modèle : -7.78 + 0.12*age + 2.46*1_alcool=I + 1.45 * 1_alcool=M+3.81*1_tabac=0-0.09age*1_tabac=0