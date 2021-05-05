#== Emplacement des données
setwd("C:/Users/Victor/Documents/Scolaire/INSA Rennes/4GM/Bureau d'étude/Données")

#== Importation des données
BasicInfo = read.csv("BasicInfo.csv")
DataDescr = read.csv("DataDescription.csv")
IndoorTemp = read.csv("IndoorTemperature.csv")     # toutes les 10 min
OutdoorTemp = read.csv("OutdoorTemperature.csv")   # toutes les 5 min
Scada = read.csv("Scada.csv")                      # toutes les 5 min
#TFake = read.csv("TFake.csv")                      # toutes les 30 min

#== Conversions des dates
library(lubridate)
IndoorTemp$DT = ymd_hms(IndoorTemp$DT)
OutdoorTemp$DT = ymd_hms(OutdoorTemp$DT)
Scada$DT = ymd_hms(Scada$DT)
#TFake$DT = ymd_hms(TFake$DT)

#== Arrondis des dates à des temps choisis (https://lubridate.tidyverse.org/reference/round_date.html)
IndoorTemp$DT  = ceiling_date(IndoorTemp$DT, unit = "10 mins")
OutdoorTemp$DT = ceiling_date(OutdoorTemp$DT, unit = "5 mins")
Scada$DT = ceiling_date(Scada$DT, unit = "5 mins")
#TFake$DT = ceiling_date(TFake$DT, unit = "10 mins")

#== Suppression des doubles lignes
doublons = duplicated(IndoorTemp[, c(1, 2)]) # 1 et 2 correspondent au temps et au skid
IndoorTemp =  IndoorTemp[!doublons,]
# On répète les valeurs de IndoorTemp 5 minutes plus tôt pour avoir une donnée toutes les 5 min
IndoorTemp_bis = IndoorTemp 
IndoorTemp_bis$DT = IndoorTemp_bis$DT - hm("0:05")
IndoorTemp = rbind(IndoorTemp, IndoorTemp_bis)

doublons = duplicated(OutdoorTemp$DT)
OutdoorTemp = OutdoorTemp[!doublons,]

Scada = Scada[, -1] # Pour retirer la première colonne contenant des indexes
doublons = duplicated(Scada[, c(1, 2)]) # 1 et 2 correspondent au temps et au skid
Scada = Scada[!doublons,]


#== Construction du dataFrame avec toutes les infos
donnees = merge(Scada, OutdoorTemp, by = "DT")
donnees = merge(donnees, IndoorTemp, by = c("skidID", "DT"))
donnees = merge(donnees, BasicInfo, by = "skidID")
donnees = donnees[!duplicated(donnees),]


#== Création de nouvelles variables (et suppression des variables inutiles)
donnees$time = donnees$DT - ymd_hms("2019-10-01 00:00:00")
donnees$day_moment = as.factor( floor(hour(donnees$DT)/2) ) # pour 12 moments de la journée
donnees$skidType = as.factor(paste0(donnees$buildingTypeEN,'_',donnees$heatTypeEN,'_',
                                    ifelse(donnees$energyType=='Y','GoodInsulation','BadInsulation')))
donnees$P_surfacique = donnees$Puissance/donnees$area * 10^6

donnees = donnees[order(donnees$time),]

donnees$P_surfacique_030 = rep(0,nrow(donnees))
donnees$P_surfacique_060 = rep(0,nrow(donnees))
donnees$P_surfacique_090 = rep(0,nrow(donnees))
donnees$P_surfacique_120 = rep(0,nrow(donnees))

for(skid in levels(donnees$skidID)){
  indices_skid = which(donnees$skidID == skid)
  
  P_moyen_030 = convolve(donnees$P_surfacique[indices_skid], rep(1/6,6), type = "filter")
  P_moyen_060 = convolve(donnees$P_surfacique[indices_skid], rep(1/12,12), type = "filter")
  P_moyen_090 = convolve(donnees$P_surfacique[indices_skid], rep(1/18,18), type = "filter")
  P_moyen_120 = convolve(donnees$P_surfacique[indices_skid], rep(1/24,24), type = "filter")
  
  donnees$P_surfacique_030[indices_skid] = c(rep(NA,6), P_moyen_030[-length(P_moyen_030)])
  donnees$P_surfacique_060[indices_skid] = c(rep(NA,12), P_moyen_060[-length(P_moyen_060)])
  donnees$P_surfacique_090[indices_skid] = c(rep(NA,18), P_moyen_090[-length(P_moyen_090)])
  donnees$P_surfacique_120[indices_skid] = c(rep(NA,24), P_moyen_120[-length(P_moyen_120)])
}
donnees = na.omit(donnees)


donnees = donnees[c("skidID","Puissance","P_surfacique",
                    "P_surfacique_030","P_surfacique_060","P_surfacique_090","P_surfacique_120", 
                    "time","day_moment",
                    "OutdoorTemp","IndoorTemp","IndoorTemp_010","IndoorTemp_020",
                    "IndoorTemp_030","IndoorTemp_040","IndoorTemp_050","IndoorTemp_060",
                    "area","skidType")]


#== Export du dataFrame 'donnees'
write.csv(donnees, file="donnees.csv", row.names = FALSE)


#== Gestion de valeurs extrêmes

# Plot des valeurs pour voir si on a des valeurs bizarres
#plot(OutdoorTemp$DT, OutdoorTemp$OutdoorTemp, type = "l")
#plot(IndoorTemp$DT, IndoorTemp$IndoorTemp, type = "l")
# On voit que des températures intérieures sont en dessous de 15 et d'autres au dessus de 25...

# Suppression des valeurs incohérentes
