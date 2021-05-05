CrossValid_lm = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE LINÉAIRE
  
  
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
  
  
  indices_blocs = list() # Contiendra les indices des blocs de validation croisée
  nb_blocs = floor((nrow(data)-24)/144) # on veut avoir au moins 24 données dans le bloc 0 pour calculer les puissances
  
  indices_blocs[[1]] = 1:(nrow(data)-nb_blocs*144) # Indices des données du bloc 0
  
  # Construction des blocs de données sur 12h (12h * 12 données/heures = 144)
  for (i in 1:(nb_blocs)){
    indices_blocs[[i+1]] = (144*(i-1)+1):(144*i) + (nrow(data)-nb_blocs*144)
  }
  
  pred = NULL # Contiendra le vecteur des prédictions
  res = NULL # Contiendra les résidus des prédictions
  
  for(i in 2:(nb_blocs+1)){
    # Pour faire comme une barre de chargement
    if ((i-1)%%25 == 0) { print(paste('  bloc',i-1,'sur',nb_blocs)) }
    
    # Séparation des données d'apprentissage et des données de test de ce bloc
    data_appr = data[-indices_blocs[[i]],]
    data_test = data[indices_blocs[[i]],]
    
    Puissances_reelles = data_test$Puissance # Copie des vraies valeurs des Puissances
    
    # On ajoute à 'data_test' les dernières 24 données (chronologiques) pour permettre ensuite de faire
    # les calculs de puissances moyennes passées
    les_dernieres_24 = indices_blocs[[i-1]][1:24 + (length(indices_blocs[[i-1]]) - 24)]
    data_test = rbind(data[les_dernieres_24,], data_test)
    
    # Modèle linéaire
    mod = lm(formule_modele, data = data_appr)
    
    # Calculs des prédictions de ce bloc pour chacune des 144 données à prédire
    for(j in (1:144)+24){
      somme_puissances = data_test$Puissance[(j-24):(j-1)]
      data_test$Puissance_030[j] = sum(somme_puissances[19:24])/6  # Puissance moyenne sur le 30 dernières minutes
      data_test$Puissance_060[j] = sum(somme_puissances[13:24])/12
      data_test$Puissance_090[j] = sum(somme_puissances[7:24])/18
      data_test$Puissance_120[j] = sum(somme_puissances)/24
      
      data_test$Puissance[j] = predict(mod, newdata=data_test[j,-1]) # Prédiction pour la donnée j
      # Mise à jour au fur et à mesure que l'on prédit dans 'data_test'
      # pour que les puissances moyennes se basent sur les prédictions
    }
    
    pred_bloc = data_test$Puissance[(1:144)+24]
    pred = c(pred, pred_bloc)
    res = c(res, (Puissances_reelles - pred_bloc))
  }
  
  l = list()
  l$pred = pred
  l$time = data$time[-indices_blocs[[1]]]
  l$last_model = mod
  l$res = res
  l$MSE = mean(res^2)
  l$nMSE = l$MSE/var(data$Puissance)
  
  return(l)
}  

CrossValid_ridge = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE RIDGE
  library(glmnet)
  
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
  
  #== Choix du paramètre de ridge
  mod_selec_lasso = cv.glmnet(model.matrix(formule_modele, data = data), data$Puissance,
                              alpha = 0, family = "gaussian", lambda=exp(seq(-12,-2,length.out=200)))
  
  
  indices_blocs = list() # Contiendra les indices des blocs de validation croisée
  nb_blocs = floor((nrow(data)-24)/144) # on veut avoir au moins 24 données dans le bloc 0 pour calculer les puissances
  
  indices_blocs[[1]] = 1:(nrow(data)-nb_blocs*144) # Indices des données du bloc 0
  
  # Construction des blocs de données sur 12h (12h * 12 données/heures = 144)
  for (i in 1:(nb_blocs)){
    indices_blocs[[i+1]] = (144*(i-1)+1):(144*i) + (nrow(data)-nb_blocs*144)
  }
  
  pred = NULL # Contiendra le vecteur des prédictions
  res = NULL # Contiendra les résidus des prédictions
  
  for(i in 2:(nb_blocs+1)){
    # Pour faire comme une barre de chargement
    if ((i-1)%%25 == 0) { print(paste('  bloc',i-1,'sur',nb_blocs)) }
    
    # Séparation des données d'apprentissage et des données de test de ce bloc
    data_appr = data[-indices_blocs[[i]],]
    data_test = data[indices_blocs[[i]],]
    
    Puissances_reelles = data_test$Puissance # Copie des vraies valeurs des Puissances
    
    # On ajoute à 'data_test' les dernières 24 données (chronologiques) pour permettre ensuite de faire
    # les calculs de puissances moyennes passées
    les_dernieres_24 = indices_blocs[[i-1]][1:24 + (length(indices_blocs[[i-1]]) - 24)]
    data_test = rbind(data[les_dernieres_24,], data_test)
    
    # Modèle ridge
    mod = glmnet(model.matrix(formule_modele, data = data_appr), data_appr$Puissance,
                 alpha = 0, family = "gaussian", lambda = mod_selec_lasso$lambda.min)
    
    
    # Calculs des prédictions de ce bloc pour chacune des 144 données à prédire
    for(j in (1:144)+24){
      somme_puissances = data_test$Puissance[(j-24):(j-1)]
      data_test$Puissance_030[j] = sum(somme_puissances[19:24])/6  # Puissance moyenne sur le 30 dernières minutes
      data_test$Puissance_060[j] = sum(somme_puissances[13:24])/12
      data_test$Puissance_090[j] = sum(somme_puissances[7:24])/18
      data_test$Puissance_120[j] = sum(somme_puissances)/24
      
      # Prédiction pour la donnée j
      data_test$Puissance[j] = predict(mod, newx=model.matrix(formule_modele, data = data_test[j,]))
      # Mise à jour au fur et à mesure que l'on prédit dans 'data_test'
      # pour que les puissances moyennes se basent sur les prédictions
    }
    
    pred_bloc = data_test$Puissance[(1:144)+24]
    pred = c(pred, pred_bloc)
    res = c(res, (Puissances_reelles - pred_bloc))
  }
  
  l = list()
  l$pred = pred
  l$time = data$time[-indices_blocs[[1]]]
  l$last_model = mod
  l$res = res
  l$MSE = mean(res^2)
  l$nMSE = l$MSE/var(data$Puissance)
  
  return(l)
}  

CrossValid_lm_Psurf = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE LINÉAIRE en prédisant la PUISSANCE SURFACIQUE
  library(glmnet)
  
  data = data[order(data$time),] # Pour s'assurer que les données sont bien dans l'ordre chronologique
  data$skidID = droplevels(data$skidID) # Pour retirer les modalités non présentes de skidID
  liste_skids = levels(data$skidID) # Liste des skids présents dans data
  
  # Petit affichage lors de l'exécution de la fonction
  print(paste('Skids à traiter :',paste(liste_skids, collapse=", ")))
  
  #== Ajout des variables puissances surfaciques moyennes des 30, 60, 90 et 120 minutes passées
  data$P_surfacique_030 = rep(0,nrow(data))
  data$P_surfacique_060 = rep(0,nrow(data))
  data$P_surfacique_090 = rep(0,nrow(data))
  data$P_surfacique_120 = rep(0,nrow(data))
  
  for(skid in liste_skids){
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
  
 
  #== Création des indices de blocs pour chaque skid
  indices_blocs = list()
  
  for(skid in liste_skids){
    indices_skid = which(data$skidID == skid) # indices "absolus" des données de ce skid dans 'data'
    
    indices_blocs[[skid]] = list() # contiendra les indices des blocs de validation croisée de ce skid
    
    n = nrow(data[indices_skid,]) # nombre de lignes dans 'data' pour ce skid
    nb_blocs = floor((n-24)/144) # on veut avoir au moins 24 données dans le bloc 0 pour calculer les puissances
    
    indices_blocs[[skid]][[1]] = indices_skid[1:(n-nb_blocs*144)] # indices des données du bloc 0
    
    # Construction des blocs de données sur 12h (12h * 12 données/heures = 144)
    for (i in 1:(nb_blocs)){
      indices_blocs[[skid]][[i+1]] = indices_skid[(144*(i-1)+1):(144*i) + (n-nb_blocs*144)]
    }
  }
  
  
  pred = list() # Contiendra le vecteur des prédictions
  res = list() # Contiendra les résidus des prédictions
  
  #== Prédiction par skid en validation croisée
  for(skid in liste_skids){
    print('------------------------------')
    for(i in 2:length(indices_blocs[[skid]])){
      # Affichage lors de l'exécution pour faire comme une barre de chargement
      if ((i-1)%%25 == 0) { print(paste(skid,':  bloc',i-1,'sur',length(indices_blocs[[skid]]))) }
      
      # Séparation des données d'apprentissage et des données de test de ce bloc pour ce skid
      data_appr = data[-indices_blocs[[skid]][[i]],]
      data_test = data[indices_blocs[[skid]][[i]],]
      
      Psurf_reelles = data_test$P_surfacique # Copie des vraies valeurs des Puissances surfaciques
      
      # On ajoute à 'data_test' les dernières 24 données (chronologiques) pour permettre ensuite de faire
      # les calculs de puissances surfaciques moyennes passées
      les_dernieres_24 = indices_blocs[[skid]][[i-1]][1:24 + (length(indices_blocs[[skid]][[i-1]]) - 24)]
      data_test = rbind(data[les_dernieres_24,], data_test)
      
      # Modèle linéaire
      mod = lm(formule_modele, data = data_appr)
      
      # Calculs des prédictions de ce bloc pour chacune des 144 données à prédire
      for(j in (1:144)+24){
        somme_Psurf = data_test$P_surfacique[(j-24):(j-1)]
        data_test$P_surfacique_030[j] = sum(somme_Psurf[19:24])/6  # Puissance surfacique moyenne sur les 30 dernières minutes
        data_test$P_surfacique_060[j] = sum(somme_Psurf[13:24])/12 # etc.
        data_test$P_surfacique_090[j] = sum(somme_Psurf[7:24])/18
        data_test$P_surfacique_120[j] = sum(somme_Psurf)/24
        
        # Prédiction pour la donnée j
        data_test$P_surfacique[j] = predict(mod, newdata=data_test[j,-1])
        # Mise à jour au fur et à mesure que l'on prédit dans 'data_test'
        # pour que les puissances surfaciques moyennes se basent sur les prédictions
      }
      
      pred_bloc = data_test$P_surfacique[(1:144)+24]
      pred[[skid]] = c(pred[[skid]], pred_bloc)
      res[[skid]] = c(res[[skid]], (Psurf_reelles - pred_bloc))
    }
  }
  
  l = list()
  for(skid in liste_skids){
    l[[skid]] = list()
    l[[skid]]$pred = pred[[skid]]
    l[[skid]]$time = data$time[data$skidID == skid][-indices_blocs[[skid]][[1]]]
    l[[skid]]$res = res[[skid]]
    l[[skid]]$MSE = mean(res[[skid]]^2)
    l[[skid]]$nMSE = l[[skid]]$MSE/var(data$P_surfacique)
  }
  
  return(l)
}  

CrossValid_ridge_Psurf = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE RIDGE en prédisant la PUISSANCE SURFACIQUE
  library(glmnet)
  
  data = data[order(data$time),] # Pour s'assurer que les données sont bien dans l'ordre chronologique
  data$skidID = droplevels(data$skidID) # Pour retirer les modalités non présentes de skidID
  liste_skids = levels(data$skidID) # Liste des skids présents dans data
  
  # Petit affichage lors de l'exécution de la fonction
  print(paste('Skids à traiter :',paste(liste_skids, collapse=", ")))
  
  #== Ajout des variables puissances surfaciques moyennes des 30, 60, 90 et 120 minutes passées
  data$P_surfacique_030 = rep(0,nrow(data))
  data$P_surfacique_060 = rep(0,nrow(data))
  data$P_surfacique_090 = rep(0,nrow(data))
  data$P_surfacique_120 = rep(0,nrow(data))
  
  for(skid in liste_skids){
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
  
  
  #== Choix du paramètre de ridge
  mod_selec_lasso = cv.glmnet(model.matrix(formule_modele, data = data), data$P_surfacique,
                              alpha = 0, family = "gaussian", lambda=exp(seq(-12,-2,length.out=200)))
  
  #== Création des indices de blocs pour chaque skid
  indices_blocs = list()

  for(skid in liste_skids){
    indices_skid = which(data$skidID == skid) # indices "absolus" des données de ce skid dans 'data'
    
    indices_blocs[[skid]] = list() # contiendra les indices des blocs de validation croisée de ce skid
    
    n = nrow(data[indices_skid,]) # nombre de lignes dans 'data' pour ce skid
    nb_blocs = floor((n-24)/144) # on veut avoir au moins 24 données dans le bloc 0 pour calculer les puissances
    
    indices_blocs[[skid]][[1]] = indices_skid[1:(n-nb_blocs*144)] # indices des données du bloc 0
    
    # Construction des blocs de données sur 12h (12h * 12 données/heures = 144)
    for (i in 1:(nb_blocs)){
      indices_blocs[[skid]][[i+1]] = indices_skid[(144*(i-1)+1):(144*i) + (n-nb_blocs*144)]
    }
  }

  
  pred = list() # Contiendra le vecteur des prédictions
  res = list() # Contiendra les résidus des prédictions
  
  #== Prédiction par skid en validation croisée
  for(skid in liste_skids){
    print('------------------------------')
    for(i in 2:length(indices_blocs[[skid]])){
      # Affichage lors de l'exécution pour faire comme une barre de chargement
      if ((i-1)%%25 == 0) { print(paste(skid,':  bloc',i-1,'sur',length(indices_blocs[[skid]]))) }
      
      # Séparation des données d'apprentissage et des données de test de ce bloc pour ce skid
      data_appr = data[-indices_blocs[[skid]][[i]],]
      data_test = data[indices_blocs[[skid]][[i]],]
      
      Psurf_reelles = data_test$P_surfacique # Copie des vraies valeurs des Puissances surfaciques
      
      # On ajoute à 'data_test' les dernières 24 données (chronologiques) pour permettre ensuite de faire
      # les calculs de puissances surfaciques moyennes passées
      les_dernieres_24 = indices_blocs[[skid]][[i-1]][1:24 + (length(indices_blocs[[skid]][[i-1]]) - 24)]
      data_test = rbind(data[les_dernieres_24,], data_test)
      
      # Modèle ridge
      mod = glmnet(model.matrix(formule_modele, data = data_appr), data_appr$P_surfacique,
                   alpha = 0, family = "gaussian", lambda = mod_selec_lasso$lambda.min)
      
      
      # Calculs des prédictions de ce bloc pour chacune des 144 données à prédire
      for(j in (1:144)+24){
        somme_Psurf = data_test$P_surfacique[(j-24):(j-1)]
        data_test$P_surfacique_030[j] = sum(somme_Psurf[19:24])/6  # Puissance surfacique moyenne sur les 30 dernières minutes
        data_test$P_surfacique_060[j] = sum(somme_Psurf[13:24])/12 # etc.
        data_test$P_surfacique_090[j] = sum(somme_Psurf[7:24])/18
        data_test$P_surfacique_120[j] = sum(somme_Psurf)/24
        
        # Prédiction pour la donnée j
        data_test$P_surfacique[j] = predict(mod, newx=model.matrix(formule_modele, data = data_test[j,]))
        # Mise à jour au fur et à mesure que l'on prédit dans 'data_test'
        # pour que les puissances surfaciques moyennes se basent sur les prédictions
      }
      
      pred_bloc = data_test$P_surfacique[(1:144)+24]
      pred[[skid]] = c(pred[[skid]], pred_bloc)
      res[[skid]] = c(res[[skid]], (Psurf_reelles - pred_bloc))
    }
  }
  
  l = list()
  for(skid in liste_skids){
    l[[skid]] = list()
    l[[skid]]$pred = pred[[skid]]
    l[[skid]]$time = data$time[data$skidID == skid][-indices_blocs[[skid]][[1]]]
    l[[skid]]$res = res[[skid]]
    l[[skid]]$MSE = mean(res[[skid]]^2)
    l[[skid]]$nMSE = l[[skid]]$MSE/var(data$P_surfacique)
  }
  
  return(l)
}  

CrossValid_Temperature_lm = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE LINÉAIRE
  
  
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
  
  # Variable de contrôle pour la validation
  control  <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = TRUE)
  modele = train(formule_modele, data = data, method = "lm", trControl=control)
  
  l = list()
  l$pred = modele$finalModel$fitted.values
  l$time = data$time
  l$last_model = modele$finalModel
  l$res = modele$finalModel$residuals
  l$MSE = mean(l$res^2)
  l$nMSE = l$MSE/var(data$IndoorTemp)
  
  return(l)
}  

CrossValid_Temperature_lm_Psurf = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE LINÉAIRE
  
  
  #== Ajout des puissances moyennes des 30, 60, 90 et 120 minutes passées
  P_moyen_030 = convolve(data$P_surfacique, rep(1/6,6), type = "filter")
  P_moyen_060 = convolve(data$P_surfacique, rep(1/12,12), type = "filter")
  P_moyen_090 = convolve(data$P_surfacique, rep(1/18,18), type = "filter")
  P_moyen_120 = convolve(data$P_surfacique, rep(1/24,24), type = "filter")
  data$P_surfacique_030 = c(rep(NA,6), P_moyen_030[-length(P_moyen_030)])
  data$P_surfacique_060 = c(rep(NA,12), P_moyen_060[-length(P_moyen_060)])
  data$P_surfacique_090 = c(rep(NA,18), P_moyen_090[-length(P_moyen_090)])
  data$P_surfacique_120 = c(rep(NA,24), P_moyen_120[-length(P_moyen_120)])
  data = na.omit(data)
  
  # Variable de contrôle pour la validation
  control  <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = TRUE)
  modele = train(formule_modele, data = data, method = "lm", trControl=control)
  
  l = list()
  l$pred = modele$finalModel$fitted.values
  l$time = data$time
  l$last_model = modele$finalModel
  l$res = modele$finalModel$residuals
  l$MSE = mean(l$res^2)
  l$nMSE = l$MSE/var(data$IndoorTemp)
  
  return(l)
}  