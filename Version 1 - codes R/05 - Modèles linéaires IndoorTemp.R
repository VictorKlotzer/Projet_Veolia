#########################################################################
##  Sélection du modèle par validation croisée sur tous les  skid      ##
#########################################################################

#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")
#setwd("C:/Users/Manon DESLOGES/Desktop/bureauEtude/")

#== Chargement des packages utilisés
library(caret)

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees.csv")
donnees$day_moment = as.factor(donnees$day_moment)
donnees$skidType = as.factor(donnees$skidType)

#== Importation de fonctions pour calculer les prédictions
source("../Codes R/03b - Fonctions.R") # Victor
#source("./Code/03b - Fonctions.R") # Manon

#== Noms des skids
noms_skid = c("SSH0000001","SSH0000068","SSH0000121","SSH0000198","SSH0000218","SSH0000241",
              "SSH0000258","SSH0000275","SSH0000339","SSH0000341","SSH0000353","SSH0000381",
              "SSH0000402","SSH0000449","SSH0000457","SSH0000518","SSH0000574")

# Pour ne pas faire les calculs sur tous les skid
noms_skid_affichage = noms_skid[c(3,14,16)]

 ######################################################################################
##  MODÈLE linéaire 1 :
##    IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
##                 + P_surfacique_030 + P_surfacique_060 + P_surfacique_090 + P_surfacique_120
#######################################################################################

pred_lm_1 = list()

for(skid in noms_skid){
  print(paste('Skid :', skid, ', il reste :', as.character(length(noms_skid)-which(noms_skid==skid)),'skids a traiter après'))
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle linéaire
  formule = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
                    + P_surfacique_030 + P_surfacique_060 + P_surfacique_090 + P_surfacique_120)
  
  pred_lm_1[[skid]] = CrossValid_Temperature_lm_Psurf(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid_affichage){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$IndoorTemp, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm_1[[skid]]$time, pred_lm_1[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 10000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$IndoorTemp[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm_1[[skid]]$time, pred_lm_1[[skid]]$pred, col='red', lwd=2)
}


 ######################################################################################
##  MODÈLE linéaire 2 :
##    IndoorTemp ~ OutdoorTemp + P_surfacique
##                  + P_surfacique_030 + P_surfacique_060 + P_surfacique_090 + P_surfacique_120
#######################################################################################

pred_lm_2 = list()

for(skid in noms_skid){
  print(paste('Skid :', skid, ', il reste :', as.character(length(noms_skid)-which(noms_skid==skid)),'skids a traiter après'))
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle linéaire
  formule = formula(IndoorTemp ~ OutdoorTemp + P_surfacique
                    + P_surfacique_030 + P_surfacique_060 + P_surfacique_090 + P_surfacique_120)
  
  pred_lm_2[[skid]] = CrossValid_Temperature_lm_Psurf(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid_affichage){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$IndoorTemp, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm_2[[skid]]$time, pred_lm_2[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 1000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$IndoorTemp[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm_2[[skid]]$time, pred_lm_2[[skid]]$pred, col='red', lwd=2)
}

######################################################################################
##  MODÈLE linéaire 3 :
##    IndoorTemp ~ OutdoorTemp + P_surfacique + P_surfacique_120
#######################################################################################

pred_lm_3 = list()

for(skid in noms_skid){
  print(paste('Skid :', skid, ', il reste :', as.character(length(noms_skid)-which(noms_skid==skid)),'skids a traiter après'))
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle linéaire
  formule = formula(IndoorTemp ~ OutdoorTemp + P_surfacique + P_surfacique_120)
  
  pred_lm_3[[skid]] = CrossValid_Temperature_lm(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid_affichage){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$IndoorTemp, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm_3[[skid]]$time, pred_lm_3[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 1000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$IndoorTemp[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm_3[[skid]]$time, pred_lm_3[[skid]]$pred, col='red', lwd=2)
}



#== Comparaison des MSE

MSE = rep(0,3)

for (skid in noms_skid){
  MSE[1] = MSE[1] + pred_lm_1[[skid]]$MSE
  MSE[2] = MSE[2] + pred_lm_2[[skid]]$MSE
  MSE[3] = MSE[3] + pred_lm_3[[skid]]$MSE
}
