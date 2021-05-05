############################################
##  Sélection du modèle par type de skid  ##
############################################

#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")
#setwd("C:/Users/Manon DESLOGES/Desktop/bureauEtude/")

#== Chargement des packages utilisés
library(caret)
library(lubridate)
library(glmnet)

#== Importation de fonctions pour calculer les prédictions
source("../Codes R/06 - Fonctions.R")

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees_v2.csv")
donnees$day_moment = as.factor(donnees$day_moment)
donnees$skidType = as.factor(donnees$skidType)
donnees$DT = ymd_hms(donnees$DT)


#== Noms des skids
noms_skid = c("SSH0000001","SSH0000068","SSH0000121","SSH0000198","SSH0000218","SSH0000241",
              "SSH0000258","SSH0000275","SSH0000339","SSH0000341","SSH0000353","SSH0000381",
              "SSH0000402","SSH0000449","SSH0000457","SSH0000518","SSH0000574")

donnees = donnees[!(donnees$skidID == "SSH0000061"),]
donnees = donnees[!(donnees$skidID == "SSH0000288"),]

# Pour ne pas faire les calculs sur tous les skid
noms_skid_affichage = noms_skid[c(3,14,16)]
#noms_skid_affichage = noms_skid[c(2,6,10,11,12,15,17)]
#noms_skid_affichage = noms_skid[c(1,4,5,7,8,9,13)]


################################################################
## Explication de la température intérieure d'un skid

modl_lm_1 = list()
step_lm_1 = list()
modl_lm_2 = list()
step_lm_2 = list()
modl_ridge = list()
step_ridge = list()

for(skid in noms_skid_affichage){
  data = donnees[donnees$skidID == skid,]
  
  # Formules des modèles
  formule_1 = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
                      + P_surfacique_030 + P_surfacique_060
                      + P_surfacique_090 + P_surfacique_120)
  formule_2 = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique 
                      + P_surfacique_030 + P_surfacique_060
                      + P_surfacique_090 + P_surfacique_120 +
                      + IndoorTemp_010 + IndoorTemp_020 + IndoorTemp_030
                      + IndoorTemp_040 + IndoorTemp_050 + IndoorTemp_060)
  # formule_3 = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
  #                     + P_surfacique_030 + P_surfacique_060
  #                     + P_surfacique_090 + P_surfacique_120)
  
  modl_lm_1[[skid]] = lm(formule_1, data=data)
  step_lm_1[[skid]] = step(modl_lm_1[[skid]]) # Choisi par l'AIC
  modl_lm_2[[skid]] = lm(formule_2, data=data)
  step_lm_2[[skid]] = step(modl_lm_2[[skid]]) # Choisi par l'AIC
  
  #== Choix du paramètre de ridge
  mod_selec_lasso = cv.glmnet(model.matrix(formule_2, data = data), data$IndoorTemp,
                              alpha = 0, family = "gaussian", lambda=exp(seq(-12,-2,length.out=200)))
  
  modl_ridge[[skid]] = glmnet(model.matrix(formule_2, data = data), data$IndoorTemp,
                             alpha = 0, family = "gaussian", lambda = mod_selec_lasso$lambda.min)
  #step_ridge[[skid]] = step(modl_lm_skid[[skid]]) # Choisi par l'AIC
}


#== Affichage
par(mfrow = c(2,4))
for(skid in noms_skid_affichage){
  data = donnees[donnees$skidID == skid,]
  #data = data[order(data$time),] # données rangées dans l'ordre chronologique
  
  plot(data$DT, data$IndoorTemp, type='l', lwd=2, xlab='Temps', ylab='Température intérieure',
       main = paste('Par skid : Skid',substr(skid,8,10)))
  # lines(data$time, modl_lm_1[[skid]]$fitted, col='red', lwd=2)
  lines(data$DT, step_lm_1[[skid]]$fitted, col='blue', lwd=1)
  # lines(data$time, modl_lm_2[[skid]]$fitted, col='yellow', lwd=2)
  lines(data$DT, step_lm_2[[skid]]$fitted, col='green', lwd=1)
  lines(data$DT, predict(modl_ridge[[skid]], newx=model.matrix(formule_2, data = data)), col='red', lwd=1)
  
  jour_deb = 29
  une_semaine = (jour_deb-22)*12*24 + (1:(6*24*7))
  plot(data$DT[une_semaine], data$IndoorTemp[une_semaine], type='l', lwd=2,
       xlab='Temps', ylab='Température intérieure', main = paste('Une semaine du skid',substr(skid,8,10)))
  # lines(data$time, modl_lm_1[[skid]]$fitted, col='red', lwd=2)
  lines(data$DT, step_lm_1[[skid]]$fitted, col='blue', lwd=1)
  # lines(data$time, modl_lm_2[[skid]]$fitted, col='yellow', lwd=2)
  lines(data$DT, step_lm_2[[skid]]$fitted, col='green', lwd=1)
  lines(data$DT, predict(modl_ridge[[skid]], newx=model.matrix(formule_2, data = data)), col='red', lwd=1)
}

  
  


##################################
## Par type de skid

#== Types des skids
skid_types = levels(donnees$skidType)
skid_types_affichage = skid_types[2]


modl_lm_1_type = list()
# step_lm_1_type = list()
modl_lm_2_type = list()
# step_lm_2_type = list()
modl_lm_3_type = list()
# step_lm_3_type = list()
pred_lm_1 = list()
pred_lm_2 = list()
pred_lm_3 = list()

for(type in skid_types_affichage){
  data = donnees[donnees$skidType == type,]
  
  # Formules des modèles
  formule_1 = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
                      + P_surfacique_030 + P_surfacique_060
                      + P_surfacique_090 + P_surfacique_120)
  formule_2 = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique 
                      + P_surfacique_030 + P_surfacique_060
                      + P_surfacique_090 + P_surfacique_120 +
                        + IndoorTemp_010 + IndoorTemp_020 + IndoorTemp_030
                      + IndoorTemp_040 + IndoorTemp_050 + IndoorTemp_060)
  formule_3 = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
                      + P_surfacique_030 + P_surfacique_060
                      + P_surfacique_090 + P_surfacique_120 + IndoorTemp_060)

  modl_lm_1_type[[type]] = lm(formule_1, data=data)
  # step_lm_1_type[[type]] = step(modl_lm_1_type[[type]]) # Choisi par l'AIC
  modl_lm_2_type[[type]] = lm(formule_2, data=data)
  # step_lm_2_type[[type]] = step(modl_lm_2_type[[type]]) # Choisi par l'AIC
  modl_lm_3_type[[type]] = lm(formule_3, data=data)
  
  # pred_lm_1[[type]] = successiveCV_lm(data, formule_1)
  # pred_lm_2[[type]] = successiveCV_lm(data, formule_2)
  # pred_lm_3[[type]] = successiveCV_lm(data, formule_3)
}


#== Affichage et comparaison des modèles

MAPE_1 = list()
MAPE_2 = list()
MAPE_3 = list()

# Modèle 1
for(type in skid_types_affichage){
  par(mfrow = c(2,4))
  for(skid in levels(factor(donnees$skidID[donnees$skidType == type]))){
    data = donnees[donnees$skidID == skid,]
    #data = data[order(data$time),] # données rangées dans l'ordre chronologique
    
    plot(data$DT, data$IndoorTemp, type='l', xlab='Temps', ylab='Température intérieure',
         main = paste('Mod 1 par type : Skid',substr(skid,8,10)))
    lines(data$DT, modl_lm_1_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
          col='red', lwd=2)
    # lines(data$DT, step_lm_1_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
    #       col='blue', lwd=1)
    lines(pred_lm_1[[type]][[skid]]$DT, pred_lm_1[[type]][[skid]]$pred,
          col='blue', lwd=1)
    
    jour_deb = 29
    une_semaine = (jour_deb-22)*12*24 + (1:(6*24*7))
    plot(data$DT[une_semaine], data$IndoorTemp[une_semaine], type='l',
         xlab='Temps', ylab='Température intérieure', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(data$DT, modl_lm_1_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
          col='red', lwd=2)
    # lines(data$DT, step_lm_1_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
    #       col='blue', lwd=1)
    lines(pred_lm_1[[type]][[skid]]$DT, pred_lm_1[[type]][[skid]]$pred,
          col='blue', lwd=1)
    
    MAPE_1[[type]] = c(MAPE_1[[type]],pred_lm_1[[type]][[skid]]$MAPE)
  }
}
  
# Modèle 2
for(type in skid_types_affichage){
  par(mfrow = c(2,4))
  for(skid in levels(factor(donnees$skidID[donnees$skidType == type]))){
    data = donnees[donnees$skidID == skid,]
    #data = data[order(data$time),] # données rangées dans l'ordre chronologique
    
    plot(data$DT, data$IndoorTemp, type='l', xlab='Temps', ylab='Température intérieure',
         main = paste('Mod 2 par type : Skid',substr(skid,8,10)))
    lines(data$DT, modl_lm_2_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
          col='red', lwd=2)
    lines(pred_lm_2[[type]][[skid]]$DT, pred_lm_2[[type]][[skid]]$pred,
          col='blue', lwd=1)
    
    jour_deb = 29
    une_semaine = (jour_deb-22)*12*24 + (1:(6*24*7))
    plot(data$DT[une_semaine], data$IndoorTemp[une_semaine], type='l',
         xlab='Temps', ylab='Température intérieure', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(data$DT, modl_lm_2_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
          col='red', lwd=2)
    lines(pred_lm_2[[type]][[skid]]$DT, pred_lm_2[[type]][[skid]]$pred,
          col='blue', lwd=1)
    
    MAPE_2[[type]] = c(MAPE_2[[type]],pred_lm_2[[type]][[skid]]$MAPE)
  }
}

# Modèle 3
for(type in skid_types_affichage){
  par(mfrow = c(2,4))
  for(skid in levels(factor(donnees$skidID[donnees$skidType == type]))){
    data = donnees[donnees$skidID == skid,]
    #data = data[order(data$time),] # données rangées dans l'ordre chronologique
    
    plot(data$DT, data$IndoorTemp, type='l', xlab='Temps', ylab='Température intérieure',
         main = paste('Mod 3 par type : Skid',substr(skid,8,10)))
    lines(data$DT, modl_lm_3_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
          col='red', lwd=2)
    # lines(pred_lm_3[[type]][[skid]]$DT, pred_lm_3[[type]][[skid]]$pred,
    #       col='blue', lwd=1)
    # 
    jour_deb = 29
    une_semaine = (jour_deb-22)*12*24 + (1:(6*24*7))
    plot(data$DT[une_semaine], data$IndoorTemp[une_semaine], type='l',
         xlab='Temps', ylab='Température intérieure', main = paste('Une semaine du skid',substr(skid,8,10)))
    lines(data$DT, modl_lm_3_type[[type]]$fitted[ donnees[donnees$skidType == type,]$skidID == skid ],
          col='red', lwd=2)
    # lines(pred_lm_3[[type]][[skid]]$DT, pred_lm_3[[type]][[skid]]$pred,
    #       col='blue', lwd=1)
    
    MAPE_3[[type]] = c(MAPE_3[[type]],pred_lm_3[[type]][[skid]]$MAPE)
  }
}

##################################
## Par type de skid

#== Types des skids
skid_types = levels(donnees$skidType)
skid_types_affichage = skid_types[2]


modl_ridge = list()
mod_selec_lasso = list()

for(type in skid_types_affichage){
  data = donnees[donnees$skidType == type,]
  
  # Formules des modèles
  formule = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
                      + P_surfacique_030 + P_surfacique_060
                      + P_surfacique_090 + P_surfacique_120 + IndoorTemp_060)
  
  #== Choix du paramètre de ridge
  mod_selec_lasso[[type]] = cv.glmnet(model.matrix(formule, data = data), data$IndoorTemp,
                              alpha = 0, family = "gaussian", lambda=exp(seq(-20,-2,length.out=200)))

  modl_ridge[[type]] = glmnet(model.matrix(formule, data = data), data$IndoorTemp,
                              alpha = 0, family = "gaussian", lambda = mod_selec_lasso[[type]]$lambda.min)
  #step_ridge[[skid]] = step(modl_lm_skid[[skid]]) # Choisi par l'AIC
}


# Zone de test

data_1 = donnees[donnees$skidType == "Residential_Radiator_BadInsulation",]
data_1 = data_1[order(data_1$time),]
skid = "SSH0000449" #"SSH0000121"  "SSH0000518" 
n = table(data_1$skidID)[skid]
ind_test = which(data_1$skidID == skid)[floor(n*.8):n]
data_appr = data[-ind_test,]
data_test = data[ind_test,]

formule = formula(IndoorTemp ~ OutdoorTemp + day_moment + P_surfacique
                  + P_surfacique_030 + P_surfacique_060
                  + P_surfacique_090 + P_surfacique_120 + IndoorTemp_060)

par(mfrow = c(2,3))
lambdas = c(.01,.1,.5,1,2)#seq(1,10,by=2)
for( lam in lambdas ){
  mod_ridge = glmnet(model.matrix(formule, data = data_appr), data_appr$IndoorTemp,
                      alpha = .5, family = "gaussian", lambda = lam)
  pred = predict(mod_ridge, newx=model.matrix(formule, data = data_test))
  MAPE = mean(abs(data_test$IndoorTemp - pred) / data_test$IndoorTemp)
  print(paste('lambda =',lam,'MAPE =',MAPE))
  
  plot(data_1$DT[data_1$skidID == skid], data_1$IndoorTemp[data_1$skidID == skid], type='l')
  lines(data_appr$DT[data_appr$skidID == skid], predict(mod_ridge, newx=model.matrix(formule, data = data_appr))[data_appr$skidID == skid], col="green")
  lines(data_test$DT, pred, col="red")
}




