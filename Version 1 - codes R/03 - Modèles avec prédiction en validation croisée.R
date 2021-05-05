#########################################################################
##  Calculs des prédictions par validation croisée sur plusieurs skid  ##
#########################################################################

#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees.csv")
donnees$day_moment = as.factor(donnees$day_moment)
donnees$skidType = as.factor(donnees$skidType)
donnees$skidID = as.factor(donnees$skidID)

#== Importation de fonctions pour calculer les prédictions
source("../Codes R/03b - Fonctions.R")


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
## - M2 de Manon -
##  MODÈLE linéaire 1 :
##    Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp
##                + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120
#######################################################################################

pred_lm_1 = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle linéaire
  formule = formula(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp
                    + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120)
  
  pred_lm_1[[skid]] = CrossValid_lm(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm_1[[skid]]$time, pred_lm_1[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 1000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm_1[[skid]]$time, pred_lm_1[[skid]]$pred, col='red', lwd=2)
}


 ######################################################################################
## - M3 de Manon -
##  MODÈLE linéaire 2 :
##    Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120
#######################################################################################

pred_lm_2 = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle linéaire
  formule = formula(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120)
  
  pred_lm_2[[skid]] = CrossValid_lm(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm_2[[skid]]$time, pred_lm_2[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 10000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm_2[[skid]]$time, pred_lm_2[[skid]]$pred, col='red', lwd=2)
}

 ######################################################################################
## - ... de Manon -
##  MODÈLE Ridge 1 :
##    Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp
##                + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120
#######################################################################################

pred_ridge_1 = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle ridge
  formule = formula(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp
                    + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120)
  
  pred_ridge_1[[skid]] = CrossValid_ridge(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_ridge_1[[skid]]$time, pred_ridge_1[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 20000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_ridge_1[[skid]]$time, pred_ridge_1[[skid]]$pred, col='red', lwd=2)
}

 ######################################################################################
## - ... de Manon -
##  MODÈLE Ridge 2 :
##    Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120
#######################################################################################

pred_ridge_2 = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle ridge
  formule = formula(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120)
  
  pred_ridge_2[[skid]] = CrossValid_ridge(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_ridge_2[[skid]]$time, pred_ridge_2[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 20000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_ridge_2[[skid]]$time, pred_ridge_2[[skid]]$pred, col='red', lwd=2)
}

 ######################################################################################
## - M2 de Manon -
##  MODÈLE 7 AMÉLIORÉ de Anouck :
##    Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060 
##                + Puissance_090 + Puissance_120
##                + Prev_OutdoorTemp_240 + Prev_OutdoorTemp_480 + Prev_OutdoorTemp_720
##                + Next_OutdoorTemp_240 + Next_OutdoorTemp_480 + Next_OutdoorTemp_720
#######################################################################################


# Ajout des nouvelles variables
n = length(donnees$OutdoorTemp)

donnees$Prev_OutdoorTemp_240 = c(rep(NA,48), donnees$OutdoorTemp[-((n-48+1):n)])
donnees$Prev_OutdoorTemp_480 = c(rep(NA,96), donnees$OutdoorTemp[-((n-96+1):n)])
donnees$Prev_OutdoorTemp_720 = c(rep(NA,144), donnees$OutdoorTemp[-((n-144+1):n)])

donnees$Next_OutdoorTemp_240 = c(donnees$OutdoorTemp[-(1:48)], rep(NA,48))
donnees$Next_OutdoorTemp_480 = c(donnees$OutdoorTemp[-(1:96)], rep(NA,96))
donnees$Next_OutdoorTemp_720 = c(donnees$OutdoorTemp[-(1:144)], rep(NA,144))

donnees = na.omit(donnees)

pred_lm_7 = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  # Formule du modèle linéaire
  formule = formula(Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060 
                    + Puissance_090 + Puissance_120
                    + Prev_OutdoorTemp_240 + Prev_OutdoorTemp_480 + Prev_OutdoorTemp_720
                    + Next_OutdoorTemp_240 + Next_OutdoorTemp_480 + Next_OutdoorTemp_720)
  
  pred_lm_7[[skid]] = CrossValid_lm(data, formule)
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
       main = paste('Skid',substr(skid,8,10)))
  lines(pred_lm_7[[skid]]$time, pred_lm_7[[skid]]$pred, col='red', lwd=2)
  
  une_semaine = 10000 + (1:(6*24*7))
  plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
       xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
  lines(pred_lm_7[[skid]]$time, pred_lm_7[[skid]]$pred, col='red', lwd=2)
}

#####

  ##########################################################
##                                                          ##
#   Prédiction de la puissance surfacique par type de skid   #
##                                                          ##
  ##########################################################

skid_types = levels(donnees$skidType)

# Pour ne pas utiliser toutes les données
skid_types = skid_types[2]

 ######################################################################################
##  MODÈLE linéaire surfacique :
##    PuissanceSurfacique ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120
#######################################################################################

pred_Psurf_lm = list()

for(type in skid_types){
  data = donnees[donnees$skidType == type,]
  
  # Formule du modèle ridge
  formule = formula(P_surfacique ~ time + I(time^2) + IndoorTemp + OutdoorTemp + P_surfacique_120)
  
  pred_Psurf_lm[[type]] = CrossValid_lm_Psurf(data, formule)
}


#== Affichage
par(mfrow = c(3,4))
for(type in skid_types){
  for(skid in names(pred_Psurf_ridge[[type]])){
    data = donnees[donnees$skidID == skid,]
    data = data[order(data$time),] # données rangées dans l'ordre chronologique
    
    plot(data$time, data$P_surfacique, type='l', xlab='Temps', ylab='Puissance surfacique',
         main = paste('Skid',substr(skid,8,10)))
    lines(pred_Psurf_lm[[type]][[skid]]$time, pred_Psurf_lm[[type]][[skid]]$pred, col='red', lwd=2)
    
    une_semaine = 20000 + (1:(6*24*7))
    plot(data$time[une_semaine], data$P_surfacique[une_semaine], type='l',
         xlab='Temps', ylab='Puissance surfacique', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(pred_Psurf_lm[[type]][[skid]]$time, pred_Psurf_lm[[type]][[skid]]$pred, col='red', lwd=2)
    
    aire = data$area[1]
    
    plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
         main = paste('Skid',substr(skid,8,10)))
    lines(pred_Psurf_lm[[type]][[skid]]$time, aire*10^(-6)*pred_Psurf_lm[[type]][[skid]]$pred, col='red', lwd=2)
    
    une_semaine = 20000 + (1:(6*24*7))
    plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
         xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(pred_Psurf_lm[[type]][[skid]]$time, aire*10^(-6)*pred_Psurf_lm[[type]][[skid]]$pred, col='red', lwd=2)
  }
}


 ######################################################################################
##  MODÈLE Ridge surfacique :
##    PuissanceSurfacique ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120
#######################################################################################

pred_Psurf_ridge = list()

for(type in skid_types){
  data = donnees[donnees$skidType == type,]
  
  # Formule du modèle ridge
  formule = formula(P_surfacique ~ time + I(time^2) + IndoorTemp + OutdoorTemp + P_surfacique_120)
  
  pred_Psurf_ridge[[type]] = CrossValid_ridge_Psurf(data, formule)
}


#== Affichage
par(mfrow = c(3,4))
for(type in skid_types){
  for(skid in names(pred_Psurf_ridge[[type]])){
    data = donnees[donnees$skidID == skid,]
    data = data[order(data$time),] # données rangées dans l'ordre chronologique
    
    plot(data$time, data$P_surfacique, type='l', xlab='Temps', ylab='Puissance surfacique',
         main = paste('Skid',substr(skid,8,10)))
    lines(pred_Psurf_ridge[[type]][[skid]]$time, pred_Psurf_ridge[[type]][[skid]]$pred, col='red', lwd=2)
    
    une_semaine = 20000 + (1:(6*24*7))
    plot(data$time[une_semaine], data$P_surfacique[une_semaine], type='l',
         xlab='Temps', ylab='Puissance surfacique', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(pred_Psurf_ridge[[type]][[skid]]$time, pred_Psurf_ridge[[type]][[skid]]$pred, col='red', lwd=2)
    
    aire = data$area[1]
    
    plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance',
         main = paste('Skid',substr(skid,8,10)))
    lines(pred_Psurf_ridge[[type]][[skid]]$time, aire*10^(-6)*pred_Psurf_ridge[[type]][[skid]]$pred, col='red', lwd=2)
    
    une_semaine = 20000 + (1:(6*24*7))
    plot(data$time[une_semaine], data$Puissance[une_semaine], type='l',
         xlab='Temps', ylab='Puissance', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(pred_Psurf_ridge[[type]][[skid]]$time, aire*10^(-6)*pred_Psurf_ridge[[type]][[skid]]$pred, col='red', lwd=2)
  }
}






















#####

  #########################
##                         ##
#    Résidus des modèles    #
##                         ##
  #########################

mod_lm_1 = list()
mod_lm_2 = list()
mod_lm_7 = list()
mod_lm_Psurf = list()

for(skid in noms_skid){
  data = donnees[donnees$skidID == skid,]
  data = data[-1] # Pour retirer la colonne skidID
  data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  #== Ajout des puissances moyennes des 30, 60, 90 et 120 minutes passées
  P_moyen_030 = convolve(data$Puissance, rep(1/6,6), type = "filter")
  P_moyen_060 = convolve(data$Puissance, rep(1/12,12), type = "filter")
  P_moyen_090 = convolve(data$Puissance, rep(1/18,18), type = "filter")
  P_moyen_120 = convolve(data$Puissance, rep(1/24,24), type = "filter")
  data$Puissance_030 = c(rep(NA,6), P_moyen_030[-length(P_moyen_030)])
  data$Puissance_060 = c(rep(NA,12), P_moyen_060[-length(P_moyen_060)])
  data$Puissance_090 = c(rep(NA,18), P_moyen_090[-length(P_moyen_090)])
  data$Puissance_120 = c(rep(NA,24), P_moyen_120[-length(P_moyen_120)])
  data = na.omit(data)
  
  # lm_1
  mod_lm_1[[skid]] = lm(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp
                        + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120, data=data)
  
  # lm_2
  mod_lm_2[[skid]] = lm(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp + Puissance_120, data=data)
  
  # lm_7
  mod_lm_7[[skid]] = lm(Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060 
                        + Puissance_090 + Puissance_120
                        + Prev_OutdoorTemp_240 + Prev_OutdoorTemp_480 + Prev_OutdoorTemp_720
                        + Next_OutdoorTemp_240 + Next_OutdoorTemp_480 + Next_OutdoorTemp_720, data=data)
}

for(type in skid_types){
  data = donnees[donnees$skidType == type,]
  
  data$P_surfacique_030 = rep(0,nrow(data))
  data$P_surfacique_060 = rep(0,nrow(data))
  data$P_surfacique_090 = rep(0,nrow(data))
  data$P_surfacique_120 = rep(0,nrow(data))
  
  for(skid in noms_skid){
    indices_skid = which(data$skidID == skid) # Indices "absolus" des données de ce skid dans 'data'
    
    Psurf_moyen_030 = convolve(data$P_surfacique[indices_skid], rep(1/6,6), type = "filter")
    Psurf_moyen_060 = convolve(data$P_surfacique[indices_skid], rep(1/12,12), type = "filter")
    Psurf_moyen_090 = convolve(data$P_surfacique[indices_skid], rep(1/18,18), type = "filter")
    Psurf_moyen_120 = convolve(data$P_surfacique[indices_skid], rep(1/24,24), type = "filter")
    
    data$P_surfacique_030[indices_skid] = c(rep(NA,6), Psurf_moyen_030[-length(Psurf_moyen_030)])
    data$P_surfacique_060[indices_skid] = c(rep(NA,12), Psurf_moyen_060[-length(Psurf_moyen_060)])
    data$P_surfacique_090[indices_skid] = c(rep(NA,18), Psurf_moyen_090[-length(Psurf_moyen_090)])
    data$P_surfacique_120[indices_skid] = c(rep(NA,24), Psurf_moyen_120[-length(Psurf_moyen_120)])
  }
  data = na.omit(data)
  # lm_Psurf
  mod_lm_Psurf[[type]] = lm(P_surfacique ~ time + I(time^2) + IndoorTemp + OutdoorTemp
                           + P_surfacique_120, data=data)
}

# Affichage
par(mfrow = c(3,4))
acf(mod_lm_1$SSH0000121$residuals, main="lm 1 : SSH0000121")
acf(mod_lm_2$SSH0000121$residuals, main="lm 2 : SSH0000121")
acf(mod_lm_7$SSH0000121$residuals, main="lm 7 : SSH0000121")
acf(mod_lm_Psurf$Residential_Radiator_BadInsulation$residuals, main="lm Psurf")
acf(mod_lm_1$SSH0000449$residuals, main="lm 1 : SSH0000449")
acf(mod_lm_2$SSH0000449$residuals, main="lm 2 : SSH0000449")
acf(mod_lm_7$SSH0000449$residuals, main="lm 7 : SSH0000449")
plot.new()
acf(mod_lm_1$SSH0000518$residuals, main="lm 1 : SSH0000518")
acf(mod_lm_2$SSH0000518$residuals, main="lm 2 : SSH0000518")
acf(mod_lm_7$SSH0000518$residuals, main="lm 7 : SSH0000518")






