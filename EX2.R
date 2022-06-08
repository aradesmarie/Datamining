tab = read.table('Weight.csv',header=T,sep=';')
tab$LOW=as.factor(tab$LOW)
tab$SMOKE=as.factor(tab$SMOKE)
tab$HT=as.factor(tab$HT)
tab$UI=as.factor(tab$UI)
tab$PTL=as.factor(tab$PTL)

n=nrow(tab)
ntrain=floor(2*n/3)
index = sample(1:n,ntrain,replace=F) #sans remise -> replace = F
train = tab[index,]
test = tab[-index,]
summary(tab)
summary(train)
summary(test)

modele=glm(LOW~AGE++LWT+SMOKE+HT+UI+FTV+PTL,data=train,family='binomial')
summary(modele)


modele1=glm(LOW~AGE++LWT+SMOKE+UI+FTV+PTL,data=train,family='binomial')
summary(modele1)

modele = step(modele)
#on enlève d'abord PTL puis HT
#erreur de prévision
prev=predict(modele,test,type='response') # retourne P(Y=1|X)
prev=as.numeric(prev>0.5)
MatConf=table(test$LOW,prev)
print(MatConf)
print(paste("Taux d'erreur de LOW=0",MatConf[1,2]/sum(MatConf[1,])))
print(paste("Taux d'erreur de LOW=0",MatConf[2,1]/sum(MatConf[2,])))
