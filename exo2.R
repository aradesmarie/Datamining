tab = read.table("Test_Classif_dpt.txt",header=T)
plot(tab$x1,tab$x2, col=tab$Y)

# Transformation de la variable cible en variable qualitative
tab$Y=as.factor(tab$Y)

par(mfcol=c(1,2))
# Ajustement de l’arbre de décision
library("rpart")
library("rpart.plot")
arbre=rpart(Y~x1+x2, data=tab,control=rpart.control(minsplit=5,cp=0))
plotcp(arbre)  #le 3 = le vert, le 1 = le noir et le 2 = le rouge

# Pas besoin d’élaguer l’arbre
# Affichage de l’arbre
prp(arbre,extra=1)  #extra = 1 car 


    ## avec le deuxième jeu de données  ##

tab1 = read.table("Test_Classif_Correl.txt",header=T)
plot(tab1$X1,tab1$X2, col=tab1$classes)

# Transformation de la variable cible en variable qualitative
tab1$classes=as.factor(tab1$classes)

par(mfcol=c(1,2))
# Ajustement de l’arbre de décision
library("rpart")
library("rpart.plot")
arbre1=rpart(classes~ . , data=tab1)
plotcp(arbre1)  #le 3 = le vert, le 1 = le noir et le 2 = le rouge

# Pas besoin d’élaguer l’arbre
# Affichage de l’arbre
prp(arbre1,extra=1)  #extra = 1 car 
arbre2=rpart(classes~ . , data=tab1,control=rpart.control(minsplit=2,cp=0))
prp(arbre2,extra=1)
#rpart contrstruit l'arbre et l'élague222222240