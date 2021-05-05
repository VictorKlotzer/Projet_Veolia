###################################
##  Allez avec l'ACP maintenant  ##
###################################

#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")
#setwd("C:/Users/Manon DESLOGES/Desktop/bureauEtude/")

#== Chargement des packages utilisés
library(caret)
library(lubridate)
library(glmnet)
library(FactoMineR)

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees_v2.csv")
donnees$day_moment = as.factor(donnees$day_moment)
donnees$skidType = as.factor(donnees$skidType)
donnees$DT = ymd_hms(donnees$DT)


data_1 = donnees[donnees$skidType == "Residential_Radiator_BadInsulation",]
data_1 = data_1[order(data_1$time),]


data_ACP = data_1[c("skidID","Puissance","P_surfacique","P_surfacique_030","P_surfacique_060",
                    "P_surfacique_090","P_surfacique_120","time","day_moment",
                    "OutdoorTemp","IndoorTemp_060")]
data_ACP$day_moment = as.numeric(data_ACP$day_moment)

ACP = PCA(data_ACP, scale.unit = TRUE, ncp = 5, graph = F, quali.sup = 1)

barplot(ACP$eig[,"eigenvalue"])

X = ACP$ind$coord
df = data.frame(Tint = data_1$IndoorTemp,
                X1 = X[,1],
                X2 = X[,2],
                X3 = X[,3],
                X4 = X[,4],
                X5 = X[,5])

mod = lm(Tint ~ X1 + X2 + X3 + X4 + X5, data=df)

par(mfrow = c(2,3))
for(skid in c("SSH0000121","SSH0000449","SSH0000518")){
  plot(data_1$DT[data_1$skidID==skid],data_1$IndoorTemp[data_1$skidID==skid], type='l' )
  lines(data_1$DT[data_1$skidID==skid],mod$fitted[data_1$skidID==skid], col="green")
  
  jour_deb = 29
  une_semaine = (jour_deb-22)*12*24 + (1:(6*24*7))
  plot(data_1$DT[data_1$skidID==skid][une_semaine],data_1$IndoorTemp[data_1$skidID==skid][une_semaine], type='l' )
  lines(data_1$DT[data_1$skidID==skid],mod$fitted[data_1$skidID==skid], col="green")
}





