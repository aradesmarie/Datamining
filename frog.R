tab=read.table("Frogs.csv",sep = ';',header=T)
print(length(tab))

#si la variable cible est le genre
plot(tab$MFCCs_.1,tab$MFCCs_.2,col=tab$Genus)

#créion de la base d'apprentisage (2/3)
n = nrow(tab)
ntrain=floor(2*n/3)
index = sample(1:n,ntrain,replace=F)  #il faut faire un tirage aléatoire pour avoir la base d'apprentissage avec 4390 valeurs aléatoires si jamais elles sont rangées
train = tab[index,]


#création de la base test(1/3)
test = tab[-index]

#vérification
tab$Genius = as.factor(tab$Genius)
prop.table(table(tab$Genius))

prop.table(table(train$Genius))


#construction du modèle
modele=naiveBayes(Genus ~. ,data=tab)
print(modele)

#construction du modèle 
prev = predict(modele, tab)
print(prev)

#matrice de confiance
MatConf = table(tab$Genus,prev)
print(MatConf)
#taux de bien classés
TauxBienClasses = sum(diag(MatConf))/nrow(tab) 
print(TauxBienClasses)

#classe par classe
apply(MatConf,1,sum)
diag(MatConf)/apply(MatConf,1,sum)


#si la variable cible est l'espèce

#créion de la base d'apprentisage (2/3)


#création de la base test(1/3)


#construction du modèle
modele=naiveBayes(Species ~. ,data=tab)
print(modele)

#construction du modèle 
prev = predict(modele, tab)
print(prev)

#matrice de confiance
MatConf = table(tab$Species,prev)
print(MatConf)
#taux de bien classés
TauxBienClasses = sum(diag(MatConf))/nrow(tab) 
print(TauxBienClasses)

#classe par classe
apply(MatConf,1,sum)
diag(MatConf)/apply(MatConf,1,sum)
