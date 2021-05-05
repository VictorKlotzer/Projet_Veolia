successiveCV_lm = function(data, formule_modele){
  ## Erreur de prédiction calculée par validation croisée sur des blocs de 12h
  ## pour un MODÈLE LINÉAIRE
  
  data = data[order(data$time),] # Pour s'assurer que les données sont bien dans l'ordre chronologique
  data$skidID = droplevels(data$skidID) # Pour retirer les modalités non présentes de skidID
  liste_skids = levels(data$skidID) # Liste des skids présents dans data
  
  # Petit affichage lors de l'exécution de la fonction
  print(paste('Skids à traiter :', paste(liste_skids, collapse=", ")))
  
  
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
      
      IndoorTemp_reelles = data_test$IndoorTemp # Copie des vraies valeurs des températures intérieures
      
      # On ajoute à 'data_test' les dernières 12 données (chronologiques) pour permettre ensuite de
      # garder les températures extérieures passées
      les_dernieres_12 = indices_blocs[[skid]][[i-1]][1:12 + (length(indices_blocs[[skid]][[i-1]]) - 12)]
      data_test = rbind(data[les_dernieres_12,], data_test)
      
      # Modèle linéaire
      mod = lm(formule_modele, data = data_appr)
      
      # Calculs des prédictions de ce bloc pour chacune des 144 données à prédire
      for(j in (1:144)+12){ # de 1 à 144 (+12 pour prendre en compte les valeurs ajoutées)
        data_test$IndoorTemp_010[j] = data_test$IndoorTemp[j-2]
        data_test$IndoorTemp_020[j] = data_test$IndoorTemp[j-4]
        data_test$IndoorTemp_030[j] = data_test$IndoorTemp[j-6]
        data_test$IndoorTemp_040[j] = data_test$IndoorTemp[j-8]
        data_test$IndoorTemp_050[j] = data_test$IndoorTemp[j-10]
        data_test$IndoorTemp_060[j] = data_test$IndoorTemp[j-12]
        
        data_test$IndoorTemp[j] = predict(mod, newdata=data_test[j,-1]) # Prédiction pour la donnée j
        # Mise à jour au fur et à mesure que l'on prédit dans 'data_test'
        # pour que les températures passées se basent sur les prédictions
      }
      
      pred_bloc = data_test$IndoorTemp[(1:144)+12]
      pred[[skid]] = c(pred[[skid]], pred_bloc)
      res[[skid]] = c(res[[skid]], (IndoorTemp_reelles - pred_bloc))
    }
  }
  
  l = list()
  for(skid in liste_skids){
    l[[skid]] = list()
    l[[skid]]$pred = pred[[skid]]
    l[[skid]]$DT = data$DT[data$skidID == skid][-indices_blocs[[skid]][[1]]]
    l[[skid]]$res = res[[skid]]
    l[[skid]]$MSE = mean(res[[skid]]^2)
    l[[skid]]$nMSE = l[[skid]]$MSE/var(data$IndoorTemp)
    l[[skid]]$MAPE = mean(abs(res[[skid]] / data$IndoorTemp[data$skidID == skid][-indices_blocs[[skid]][[1]]]))
  }
  
  return(l)
}  








