n = 2000
X1 = runif(n,min=-0.5, max=0.5)
X2 = runif(n,min=0,max=1)

Y = 0.1*X2>X1^2
print(Y)
plot(X1,X2,col=Y+1)

library(nnet)
library(NeuralNetTools)

Mydata=data.frame(X1,X2,as.factor(Y))
NN=nnet(Y~X1+X2,Mydata,size=3,maxit=1000)  #size = 2 pour deux neurones
plotnet(NN)


#Question3
summary(NN)  ##wts sont les poids de chaque branche
poids = NN$wts
plot(X1,X2,col=Y+1)
abline(-poids[1]/poids[3],-poids[2]/poids[3],col="green",lwd=3)
abline(-poids[4]/poids[6],-poids[5]/poids[6],col="green",lwd=3)
abline(-poids[7]/poids[9],-poids[8]/poids[9],col="green",lwd=3)

##avant de faire les abline, retourner sur le nuage de points

#Question4
sigma1 = poids[1] + poids[2]*X1 + poids[3]*X2
print(sigma1)
sigma2 = poids[4] + poids[5]*X1 + poids[6]*X2
print(sigma2)
sigma3 = poids[7] + poids[8] * X1 + poids[9] * X2
Z1 = 1/(1+exp(-sigma1))
Z2 = 1/(1+exp(-sigma2))
Z3 = 1/(1+exp(-sigma3))

#pour 2 neurones
plot(Z1,Z2,col=Y+1)
abline(-poids[7]/poids[9],-poids[8]/poids[9],col="yellow",lwd=3)

#pour 3 neurones
#plot en 3D

install.packages(rgl)
library(rgl)
plot3d(Z1,Z2,Z3,col=Y+1)


##Question6
n = 2000
X1 = runif(n,min=-0.5, max=0.5)
X2 = runif(n,min=0,max=1)
X3 = runif(n,min=-0.25,max=0.75)

