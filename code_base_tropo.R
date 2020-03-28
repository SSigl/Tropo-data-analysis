#=========================================================================#
#                                                                         #
#                         Travail thèse -- Laure Csm                      #
#                                                                         #
#=========================================================================#

rm(list = ls())

# packages utiles pour l'importation et la transformation de la bdd
library(readr)
library(car)

#importations d'autres packages utiles
library(Factoshiny)
library(rlist)
library(stringr) 
library(gdata) 
library(factoextra) 
library(cluster) 
library(NbClust)

# packages utiles pour ACP
library(FactoMineR)

# packages utiles pour les random forest
library(VSURF)
library(randomForest)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# IMPORTATION ET TRANSFORMATION BASE DE DONNÉES #

# NB : avant ça, sur excel, j'ai converti la base de données initiale en fichier csv
# en enlevant au préalable tous les caractères spéciaux (accents, guillemets, etc)
# on travaille sur le fichier csv ici
setwd("/Users/SuzanneSigalla/Documents/Travail these LaureCsm/")
data <- read.csv("data.csv",sep=";",encoding="latin1",header=F)

# on retire les 10 dernières colonnes qui sont vides (bug d'importation certainement)
data <- data[,1:165]

# on travaille sur le nom des colonnes pour enlever les sauts à la ligne qui rendent 
# les noms plus compliqués à lire
col_names <- data[1,]
replace_linebreaks <- function(x)
{return (gsub("[\r\n]", " ",x))}
col_names <- apply(col_names,1:2,FUN=replace_linebreaks)
names(data) <- col_names
data <- data[-1,]

# un autre problème dans le nom des colonnes (caractères invalides)
#valid_column_names <- make.names(names=names(data), unique=TRUE, allow_ = TRUE)
#names(data) <- valid_column_names

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ANALYSE DE LA BASE DE DONNÉES

# on s'intéresse à la variable sur laquelle on veut accomplir la régression
# ie les variables Revised Trauma Score (RTS)
# ou TRISS
# on commence par RTS
#str(data$`Revised Trauma Score (RTS)`)
data$`Revised Trauma Score (RTS)` <- as.numeric(gsub(",", ".", gsub("\\.", "", data$`Revised Trauma Score (RTS)`)))
# puis même manip pour TRISS
data$`Trauma Injury Severity Score (TRISS in %)` <-as.numeric(gsub(",", ".", gsub("\\.", "", data$`Trauma Injury Severity Score (TRISS in %)`)))

# résultats : (on voit bien la différence entre les deux scores)
#table(data$`Revised Trauma Score (RTS)`)
#table(data$`Trauma Injury Severity Score (TRISS in %)`)



#      PREMIÈRE ANALYSE : ACP
#_________________________________


# Listes des variables quantitatives sans trop de valeurs manquantes :
# age, poids, taille, BMI, ASA-PS, Glasgow initial, Glasgow moteur initial
# différentes PAS, PAD, fréq cardiaques, différentes hemocue, differentes SpO2,
# délai arrivée sur les lieux-hopital, Glasgow hospitalier, temperature, pH
# PaO2, PCO2, exces de base, FiO2, Hb conventionnelle labo, plaquettes, TP,
# Fibrogene, Creatinine, Bicar mesure, différentes troponines, alcoolemie,
# total score IGS, total score SOFA, duree de sejour en rea, nombre total CGR, 
# nombre total PFC, nombre total CP, nombre jours à l'hopital,
# score ISS, (score MGAP? ou est-ce que c'est aussi un score de trauma?),
# différentes AIS

col_quanti_RTS <- 
  c('Revised Trauma Score (RTS)','Age du patient (ans)','Poids (en kg)', 'Taille (en m)','BMI','ASA-PS','Glasgow initial',
    'Glasgow moteur initial','Pression Arterielle Diastolique (PAD) a l arrivee du SMUR',
    'Pression Arterielle Systolique (PAS) a l arrivee du SMUR','Pression Arterielle Systolique (PAS) minimum',
    'Pression Arterielle Diastolique (PAD) minimum','Pression Arterielle Systolique - PAS','Pression Arterielle Diastolique - PAD',
    'Frequence cardiaque (FC) a l arrivee du SMUR','Frequence cardiaque (FC) maximum','Hemocue initial',
    'Hemocue, a l arrivee hopital par equipe hopital','SpO2 min','Saturation peripherique oxygene - SpO2',
    'Delai   arrivee sur les lieux - arrivee hopital  ','Temperature','pH','PaO2','pCO2','Exces de base',
    'FiO2','Hb conventionnelle laboratoire','Plaquettes','TP','Fibrinogene','Creatinine','Bicar mesure','Troponine standard',
    'Troponine ultrasensible','Alcoolemie (g L)','Total Score IGS','Total Score SOFA',
    'Duree de sejour en rea- si date de sortie connue, duree de sejour = (date sortie - date d entree)- si date de sortie inconnue, duree de sejour = (date actuelle - date d entree)',
    'Nombre total de CGR','Nombre total de PFC','Nombre total CP','Nombre de jours a  l hopital','Score ISS','AIS  Face',
    'AIS Head_neck','AIS Chest','AIS Abdomen','AIS Extremities','AIS External','AIS tete et cou')

col_quanti_TRISS <- 
  c('Trauma Injury Severity Score (TRISS in %)','Age du patient (ans)','Poids (en kg)', 'Taille (en m)','BMI','ASA-PS','Glasgow initial',
    'Glasgow moteur initial','Pression Arterielle Diastolique (PAD) a l arrivee du SMUR',
    'Pression Arterielle Systolique (PAS) a l arrivee du SMUR','Pression Arterielle Systolique (PAS) minimum',
    'Pression Arterielle Diastolique (PAD) minimum','Pression Arterielle Systolique - PAS','Pression Arterielle Diastolique - PAD',
    'Frequence cardiaque (FC) a l arrivee du SMUR','Frequence cardiaque (FC) maximum','Hemocue initial',
    'Hemocue, a l arrivee hopital par equipe hopital','SpO2 min','Saturation peripherique oxygene - SpO2',
    'Delai   arrivee sur les lieux - arrivee hopital  ','Temperature','pH','PaO2','pCO2','Exces de base',
    'FiO2','Hb conventionnelle laboratoire','Plaquettes','TP','Fibrinogene','Creatinine','Bicar mesure','Troponine standard',
    'Troponine ultrasensible','Alcoolemie (g L)','Total Score IGS','Total Score SOFA',
    'Duree de sejour en rea- si date de sortie connue, duree de sejour = (date sortie - date d entree)- si date de sortie inconnue, duree de sejour = (date actuelle - date d entree)',
    'Nombre total de CGR','Nombre total de PFC','Nombre total CP','Nombre de jours a  l hopital','Score ISS','AIS  Face',
    'AIS Head_neck','AIS Chest','AIS Abdomen','AIS Extremities','AIS External','AIS tete et cou')


# les colonnes avec des virgules au lieu des points (pour effectuer le remplacement après)
col_comma <- c('Taille (en m)','BMI','Hemocue initial','Hemocue, a l arrivee hopital par equipe hopital','Temperature','pH',
               'Exces de base','Hb conventionnelle laboratoire','Fibrinogene','Alcoolemie (g L)')

other_col_RTS <- setdiff(col_quanti_RTS,col_comma)
other_col_TRISS <- setdiff(col_quanti_TRISS,col_comma)

# on ne garde que les variables numériques pour l'acp
data_quanti_RTS <- subset(data,select=col_quanti_RTS)
data_quanti_TRISS <- subset(data,select=col_quanti_TRISS)

# transformer ces variables en variables au format numérique (sinon pas d'ACP)
for(i in 1:length(col_comma)){
  j = which(colnames(data_quanti_RTS)==col_comma[i])
  data_quanti_RTS[,j] <- as.numeric(gsub(",", ".", gsub("\\.", "", data_quanti_RTS[,j])))
}
for(i in 1:length(col_comma)){
  j = which(colnames(data_quanti_TRISS)==col_comma[i])
  data_quanti_RTS[,j] <- as.numeric(gsub(",", ".", gsub("\\.", "", data_quanti_TRISS[,j])))
}
for(i in 1:length(other_col_RTS)){
  j = which(colnames(data_quanti_RTS)==other_col_RTS[i])
  data_quanti_RTS[,j] <- as.numeric(data_quanti_RTS[,j])
}
for(i in 1:length(other_col_TRISS)){
  j = which(colnames(data_quanti_TRISS)==other_col_TRISS[i])
  data_quanti_TRISS[,j] <- as.numeric(data_quanti_TRISS[,j])
}

# enlever les valeurs manquantes (pb : ne garde que 10% de la base mais c'est déjà un début)
data_quanti_RTS <- na.omit(data_quanti_RTS)
data_quanti_TRISS <- na.omit(data_quanti_TRISS)

PCA(data_quanti_RTS)
PCA(data_quanti_TRISS)




#data.surf <- VSURF(data[,-110],data[,110])
#train <- data %>% sample_frac(0.8)
#test <- anti_join(data, train)
#(model <- randomForest(Deces ~ ., data = train, ntree = 500, na.action = na.omit))

#sum(is.na(data[,3]))

#is_val_miss <- function(x){return (sum(is.na(x)))}

#missing_val <- apply(data,2,is_val_miss)









