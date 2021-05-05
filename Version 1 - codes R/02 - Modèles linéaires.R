#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees.csv")
donnees$day_moment = as.factor(donnees$day_moment)


#################################
## Test sur un seul skid

MSE = NULL # Contiendra les MSE pour les modèles testés sur ce skid

data = donnees[donnees$skidID=="SSH0000001",]
data = data[-1] # Pour retirer la colonne skidID
data = data[order(data$time),] # données rangées dans l'ordre chronologique

#== Redimensionnement ?
#data$Puissance = data$Puissance*1000
#data$time = data$time/10
#data$time_squared = data$time_squared/10000

#== Matrice de corrélation (en valeur absolue)
round(abs(cor(data[-3])),3)

#== Données d'apprentissage et de test
indices_appr = 1:floor(nrow(data)*.75) # indices dans l'ordre sinon sample(1:nrow(data),floor(nrow(data)*.75)) 
data_appr = data[indices_appr,]
data_appr = data_appr[order(data_appr$time),]
data_test = data[-indices_appr,]



###################################################################################################
##
## Modèle 1 : Puissance ~ time + I(time^2) + day_moment + IndoorTemp + OutdoorTemp
##
###################################################################################################

mod1 = lm(Puissance ~ time + I(time^2) + as.factor(day_moment) + IndoorTemp + OutdoorTemp, data = data_appr)
summary(mod1)

# Résidus
library(forecast)
#checkresiduals(mod1$residuals)
acf(mod1$residuals)

# Erreur de prédiction
pred1 = predict(mod1, newdata=data_test[,-1])
MSE1 = mean((data_test$Puissance - pred1)^2)
MSE[1] = MSE1

# Affichage
plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance', main ='Modèle 1')
lines(data_appr$time, mod1$fitted.values, col='green')
lines(data_test$time, pred1, col='red')

# Affichage juste sur une semaine
une_semaine = 10000 + (1:(6*24*7))
plot(data_appr$time[une_semaine], data_appr$Puissance[une_semaine], type='l', xlab='Temps',
     ylab='Puissance', main ='Modèle 1')
lines(data_appr$time[une_semaine], mod1$fitted.values[une_semaine], col='green')



###################################################################################################
##
## Modèle 2 : Puissance ~ IndoorTemp + OutdoorTemp
##
###################################################################################################

mod2 = lm(Puissance ~ IndoorTemp + OutdoorTemp, data = data_appr)
summary(mod2)

# Résidus
#checkresiduals(mod2$residuals)
acf(mod2$residuals)

# Erreur de prédiction
pred2 = predict(mod2, newdata=data_test[,-1])
MSE[2] = mean((data_test$Puissance - pred2)^2)



###################################################################################################
##
## Modèle 3 : Puissance ~ time + IndoorTemp + OutdoorTemp
##
###################################################################################################

mod3 = lm(Puissance ~ time + IndoorTemp + OutdoorTemp, data = data_appr)
summary(mod3)

# Résidus
#checkresiduals(mod3$residuals)
acf(mod3$residuals)

# Erreur de prédiction
pred3 = predict(mod3, newdata=data_test[,-1])
MSE[3] = mean((data_test$Puissance - pred3)^2)



#== Prise en compte des puissances passées

###################################################################################################
##
## Modèle 4 : Puissance ~ time + I(time^2) + as.factor(day_moment) + IndoorTemp + OutdoorTemp 
##                        + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120
##
###################################################################################################

data$Puissance_030 = c(rep(NA,6), convolve(data$Puissance, rep(1/6,6), type = "filter")[-1])
data$Puissance_060 = c(rep(NA,12), convolve(data$Puissance, rep(1/12,12), type = "filter")[-1])
data$Puissance_090 = c(rep(NA,18), convolve(data$Puissance, rep(1/18,18), type = "filter")[-1])
data$Puissance_120 = c(rep(NA,24), convolve(data$Puissance, rep(1/24,24), type = "filter")[-1])
data = na.omit(data)

data_appr = data[indices_appr,]
data_appr = data_appr[order(data_appr$time),]
data_test = data[-indices_appr,]

mod4 = lm(Puissance ~ time + I(time^2) + as.factor(day_moment) + IndoorTemp + OutdoorTemp 
          + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120, data = data_appr)
summary(mod4)

# Résidus
#checkresiduals(mod4$residuals)
acf(mod4$residuals)

# Erreur de prédiction
pred4 = predict(mod4, newdata=data_test[,-1])
MSE[4] = mean((data_test$Puissance - pred4)^2)



###################################################################################################
##
## Modèle 5 : Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp 
##                        + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120
##
## (modèle sélectionné par AIC à partir du modèle 4)
##
###################################################################################################

mod5 = lm(Puissance ~ time + I(time^2) + IndoorTemp + OutdoorTemp 
          + Puissance_030 + Puissance_060 + Puissance_090 + Puissance_120, data = data_appr)
summary(mod5)

# Résidus
#checkresiduals(mod5$residuals)
acf(mod5$residuals)

# Erreur de prédiction
pred5 = predict(mod5, newdata=data_test[,-1])
MSE[5] = mean((data_test$Puissance - pred5)^2)

# Affichage
plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance', main ='Modèle 5')
lines(data_appr$time, mod5$fitted.values, col='green')
lines(data_test$time, pred5, col='red')

# Juste sur une semaine
une_semaine = 10000 + (1:(6*24*7))
plot(data_appr$time[une_semaine], data_appr$Puissance[une_semaine], type='l', xlab='Temps',
     ylab='Puissance', main ='Modèle 5')
lines(data_appr$time[une_semaine], mod5$fitted.values[une_semaine], col='green')



###################################################################################################
##
## Modèle 6 : Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060
##                         + Puissance_090 + Puissance_120
##
###################################################################################################

mod6 = lm(Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060 
          + Puissance_090 + Puissance_120, data = data_appr)
summary(mod6)

# Résidus
#checkresiduals(mod6$residuals)
acf(mod6$residuals)

# Erreur de prédiction
pred6 = predict(mod6, newdata=data_test[,-1])
MSE[6] = mean((data_test$Puissance - pred6)^2)

# Affichage
plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance', main ='Modèle 6')
lines(data_appr$time, mod6$fitted.values, col='green')
lines(data_test$time, pred6, col='red')

# Juste sur une semaine
une_semaine = 28500 + (1:(6*24*2))
plot(data_appr$time[une_semaine], data_appr$Puissance[une_semaine], type='l', xlab='Temps',
     ylab='Puissance', main ='Modèle 6')
lines(data_appr$time[une_semaine], mod6$fitted.values[une_semaine], col='green')
lines(data_test$time, pred6, col='red')




#== Le calcul des prévisions était faussé car les puissances moyennes passées entrée en paramètre 
#=  étaient déjà connues. Pour calculer l'erreur de prédiction, on va donc ici directement utiliser
#=  les puissances que l'on vient de prédire. Comme l'on ne veut prédire que sur 12h en avance, on
#=  propose de calculer l'erreur de prédiction par validation croisée en utilisant des "blocs de 12h".

###################################################################################################
##
## Modèle 6 : Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060
##                         + Puissance_090 + Puissance_120
##
###################################################################################################

indices_blocs = list()
nb_blocs = floor((nrow(data)-24)/144) # on veut avoir au moins 24 données dans le bloc 0 pour calculer les puissances

indices_blocs[[1]] = 1:(nrow(data)-nb_blocs*144)

# Construction de bloc de données sur 12h (12h * 12 données/par heures = 144)
for (i in 1:(nb_blocs)){
  indices_blocs[[i+1]] = (144*(i-1)+1):(144*i) + (nrow(data)-nb_blocs*144)
}

pred6 = NULL
res = NULL
for(i in 2:(nb_blocs+1)){
  
  data_appr = data[-indices_blocs[[i]],]
  data_appr = data_appr[order(data_appr$time),] # probablement inutile, juste pour être sûr
  data_test = data[indices_blocs[[i]],]
  
  Puissances_reelles = data_test$Puissance
  
  les_dernieres_24 = indices_blocs[[i-1]][1:24 + (length(indices_blocs[[i-1]]) - 24)]
  data_test = rbind(data[les_dernieres_24,], data_test)
  
  mod6_bloc = lm(Puissance ~ IndoorTemp + OutdoorTemp + Puissance_030 + Puissance_060 
                 + Puissance_090 + Puissance_120, data = data_appr)
  
  # Calculs des prévisions
  pred_bloc = NULL
  
  for(j in (1:144)+24){
    somme_puissances = data_test$Puissance[(j-24):(j-1)]
    data_test$Puissance_030[j] = sum(somme_puissances[19:24])/6
    data_test$Puissance_060[j] = sum(somme_puissances[13:24])/12
    data_test$Puissance_090[j] = sum(somme_puissances[7:24])/18
    data_test$Puissance_120[j] = sum(somme_puissances)/24
    
    puissance_predite = predict(mod6_bloc, newdata=data_test[j,-1])
    data_test$Puissance[j] = puissance_predite # Mise à jour au fur et à mesure que l'on prédit
    pred_bloc = c(pred_bloc, puissance_predite)
  }
  
  pred6 = c(pred6, pred_bloc)
  res = c(res, (Puissances_reelles - pred_bloc)^2)
}
MSE[6] = mean(res)



# Affichage
plot(data$time, data$Puissance, type='l', xlab='Temps', ylab='Puissance', main ='Modèle 6')
lines(data$time[-indices_blocs[[1]]], pred6, col='red', lwd=2)

  
# Juste sur une semaine
une_semaine = 15000 + (1:(6*24*7))
temps = data$time[-indices_blocs[[1]]]
P = data$Puissance[-indices_blocs[[1]]]
plot(temps[une_semaine], P[une_semaine], type='l', xlab='Temps',
     ylab='Puissance', main ='Modèle 6')
lines(temps[une_semaine], pred6[une_semaine], col='red')

