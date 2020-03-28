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
library(car)
library(dplyr)

# packages utiles pour ACP/ACM
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


# quelques petits ajustements (problème de codage -- nettoyage de la bdd)
data$Deces <- recode(data$Deces,"c('Non ')='Non'")
data$`ASA-PS` <- recode(data$`ASA-PS`,"c('7, inconnu')=NA")
data$`Anomalie pupillaire (Pre-hospitalier)`<- recode(data$`Anomalie pupillaire (Pre-hospitalier)`,
                                              "c('Anisocorie unilaterale','Anomalie pupillaire',
                                              'Mydriase\n bilaterale')='Oui'")
data$`Anomalie pupillaire (Pre-hospitalier)`[data$`Anomalie pupillaire (Pre-hospitalier)`!='Non'] <- 'Oui'
# rq : les deux dernières lignes sont nécessaires (ce n'est pas une erreur)



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
               'Exces de base','Hb conventionnelle laboratoire','Fibrinogene','Alcoolemie (g L)','FiO2')

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
  data_quanti_TRISS[,j] <- as.numeric(gsub(",", ".", gsub("\\.", "", data_quanti_TRISS[,j])))
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

ACP_RTS <- PCA(data_quanti_RTS)
ACP_TRISS <- PCA(data_quanti_TRISS)

barplot(ACP_RTS$eig[,1])
barplot(ACP_TRISS$eig[,1])

plot.PCA(ACP_RTS,choix="var")
plot.PCA(ACP_TRISS,choix="var")


#________________________________________________________
#                     RANDOM FOREST


# on sélectionne les variables "initiales" (juste après le trauma)

col_init <- c('Score ISS','Trauma Injury Severity Score (TRISS in %)','Revised Trauma Score (RTS)','SCORE MGAP',
              'Age du patient (ans)','Sexe','Poids (en kg)', 'Taille (en m)','BMI','ASA-PS','Amputation','Annonce comme instable',
              'Traitement anticoagulant','Traitement antiagregants','Anomalie pupillaire (Pre-hospitalier)',
              'Arret cardio-respiratoire (massage)','Intubation orotracheale (IOT) SMUR','Temperature','pH',
              'PaO2','pCO2','Exces de base','FiO2','Hb conventionnelle laboratoire','Plaquettes','TP',
              'Fibrinogene','Creatinine','Bicar mesure','Troponine standard','Troponine ultrasensible',
              'Alcoolemie (g L)','Deces','Glasgow initial','Glasgow moteur initial','Mecanisme en cause','Origine du patient','Blast',
              'Ischemie du membre','Pression Arterielle Diastolique (PAD) a l arrivee du SMUR',
              'Pression Arterielle Systolique (PAS) a l arrivee du SMUR','Frequence cardiaque (FC) a l arrivee du SMUR',
              'Hemocue initial','Hemocue, a l arrivee hopital par equipe hopital','Delai   arrivee sur les lieux - arrivee hopital  ',
              'Total Score IGS','Total Score SOFA','Nombre de jours a  l hopital')

data_RF <- subset(data,select=col_init)

col_comma <- c('Taille (en m)','BMI','Hemocue initial','Hemocue, a l arrivee hopital par equipe hopital','Temperature','pH',
               'Exces de base','Hb conventionnelle laboratoire','Fibrinogene','Alcoolemie (g L)','FiO2')

other_num_col <- c('Score ISS','Trauma Injury Severity Score (TRISS in %)','Revised Trauma Score (RTS)','SCORE MGAP', 
                   'Glasgow initial','Glasgow moteur initial','Total Score IGS','Total Score SOFA',
                   'Age du patient (ans)','Poids (en kg)','PaO2','pCO2','Plaquettes','TP','Creatinine','Bicar mesure',
                   'Troponine standard','Troponine ultrasensible','Pression Arterielle Diastolique (PAD) a l arrivee du SMUR',
                   'Pression Arterielle Systolique (PAS) a l arrivee du SMUR','Frequence cardiaque (FC) a l arrivee du SMUR',
                   'Delai   arrivee sur les lieux - arrivee hopital  ','Nombre de jours a  l hopital')

other_fact_col <- setdiff(col_init,c(col_comma,other_num_col))

# convertir les variables au format approprié (numerique ou facteur)
for(i in 1:length(col_comma)){
  j = which(colnames(data_RF)==col_comma[i])
  data_RF[,j] <- as.numeric(gsub(",", ".", gsub("\\.", "", data_RF[,j])))
}
for(i in 1:length(other_num_col)){
  j = which(colnames(data_RF)==other_num_col[i])
  data_RF[,j] <- as.numeric(data_RF[,j])
}
for(i in 1:length(other_fact_col)){
  j = which(colnames(data_RF)==other_fact_col[i])
  data_RF[,j] <- as.factor(data_RF[,j])
  
}

# remarque : le score de Glasgow de sortie est rarement calculé 
# au lieu de ça : on peut plutôt prendre en compte la durée du séjour à l'hôpital 

data_RF <- na.omit(data_RF)

# première méthode de sélection de variables
data.surf <- VSURF(data_RF[,-48],data_RF[,48])
var_imp <- as.array(data.surf$varselect.pred)
get_name <- function(i){return(colnames(data_RF)[i])}
var_imp <- apply(var_imp,1,get_name)
# la troponine ultrasensible fait partie des variables les plus importantes.

# deuxième méthode de sélection de variables (on utilise un autre package)

set.seed(100)
revised_col <- make.names(colnames(data_RF))
names(data_RF) <- revised_col

rf0<-randomForest(`Nombre.de.jours.a..l.hopital` ~ ., data=data_RF, mtry =6, ntree=200, nodesize=1, importance=TRUE,na.action = na.omit)
imp <- importance(rf0, type = 1, scale=T)
varImpPlot(rf0, type=1)

o<-order(imp,decreasing=TRUE)
plot(imp[o],type='b',pch=20, axes=F, xlab="", ylab='importance')
axis(1,c(1:length(imp)), rownames(imp)[o], las=2, cex=0.5)
axis(2)
rownames(imp)[o]
# idem la troponine ultrasensible est aussi dans les 10 variables les plus importantes.


