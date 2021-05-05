###########################################################################################
##  Calculs des prédictions  des températures par validation croisée sur plusieurs skid  ##
###########################################################################################

#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")
#setwd("C:/Users/anouc/Desktop/4A/S8/Bureau d'études/Codes")

#== Importation de fonctions pour calculer les prédictions
source("../Codes R/03b - Fonctions.R")
library(caret)

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees.csv")
donnees$day_moment = as.factor(donnees$day_moment)
donnees$skidType = as.factor(donnees$skidType)

#levels(donnees$skidID) = c("SSH0000001","SSH0000061","SSH0000068","SSH0000121","SSH0000198","SSH0000218",
#                           "SSH0000241","SSH0000258","SSH0000275","SSH0000288","SSH0000339","SSH0000341",
#                           "SSH0000353","SSH0000381","SSH0000402","SSH0000449","SSH0000457","SSH0000518","SSH0000574")

noms_skid = c("SSH0000001","SSH0000068","SSH0000121","SSH0000198","SSH0000218","SSH0000241",
              "SSH0000258","SSH0000275","SSH0000339","SSH0000341","SSH0000353","SSH0000381",
              "SSH0000402","SSH0000449","SSH0000457","SSH0000518","SSH0000574")
# On retire les skids 61 et 288 car toutes les puissances sont nulles

# Pour ne pas faire les calculs sur tous les skid
noms_skid = noms_skid[c(3,14,16)]


######################################################################################
##  MODÈLE linéaire :                                                               ##
##     IndoorTemp ~ OutdoorTemp + day_moment + Puissance                            ##
##                + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120   ##
######################################################################################


pred_lm = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique

  # Formule du modèle linéaire
  formule = formula(IndoorTemp ~ OutdoorTemp + day_moment + Puissance
                    + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120)
  
  pred_lm[[skid]] = CrossValid_Temperature_lm(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$IndoorTemp, type='l', xlab='Temps', ylab='Température intérieure',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm[[skid]]$time, pred_lm[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 10000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$IndoorTemp[une_semaine], type='l',
       xlab='Temps', ylab='Températue intérieure', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm[[skid]]$time, pred_lm[[skid]]$pred, col='red', lwd=2)
}









