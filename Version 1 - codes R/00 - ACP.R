#== Dossier de travail
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")
#setwd("C:/Users/anouc/Desktop/4A/S8/Bureau d'études/Codes")
library("FactoMineR")

#== Lecture des données nettoyées du fichier 'donnees.csv'
donnees = read.csv("donnees.csv")
#donnees$day_moment = as.factor(donnees$day_moment)

noms_skid = levels(donnees$skidID)

# Choisir le skid dont on affiche les ACP
for(skid in noms_skid[1]){
  data = donnees[donnees$skidID==skid,c("Puissance","P_surfacique","time","day_moment","OutdoorTemp","IndoorTemp","area")]
  
  ACP.X = PCA(data, scale.unit = TRUE, ncp = 5, graph = TRUE, quali.sup = 1)
  
  # Globalement les graphes sont plus ou moins identiques (avec pas mal de différences)
  # On a toujours toutes les variables que nous avons gardées comme désignées importantes 
  
  ACP.X
  valProp = ACP.X$eig[,1]
  plot(1:nrow(ACP.X$eig), valProp, lwd=2, type="b", main="Eboulis des valeurs propres", xlab="Rang", ylab="Valeur propre")
  barplot(valProp)
  # On retient 3 axes
}

# data = donnees[donnees$skidID=="SSH0000241",]
# data = donnees[donnees$skidID=="SSH0000258",]
# data = donnees[donnees$skidID=="SSH0000275",]
# data = donnees[donnees$skidID=="SSH0000288",]
# data = donnees[donnees$skidID=="SSH0000339",]
# data = donnees[donnees$skidID=="SSH0000341",]
# data = donnees[donnees$skidID=="SSH0000353",]
# data = donnees[donnees$skidID=="SSH0000381",]
# data = donnees[donnees$skidID=="SSH0000402",]
# data = donnees[donnees$skidID=="SSH0000449",]
# data = donnees[donnees$skidID=="SSH0000457",]
# data = donnees[donnees$skidID=="SSH0000518",]
# data = donnees[donnees$skidID=="SSH0000574",] 
# 
# ACP.X = PCA(data, scale.unit = TRUE, ncp = 5, graph = TRUE, quali.sup = 1)
# 
# # Globalement les graphes sont plus ou moins identiques (avec pas mal de différences)
# # On a toujours toutes les variables que nous avons gardées comme désignées importantes 
# 
# ACP.X
# valProp = ACP.X$eig[,1]
# plot(1:nrow(ACP.X$eig), valProp, lwd=2, type="b", main="Eboulis des valeurs propres", xlab="Rang", ylab="Valeur propre")
# barplot(valProp)
# # On retient 3 axes