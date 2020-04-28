#=========================================================================#
#                                                                         #
#                         IMPORTATION ET NETTOYAGE                        #
#                                                                         #
#=========================================================================#

#%%%%%%%%%
# PACKAGES

# packages utiles pour l'importation et la transformation de la bdd
library(readr)
library(car)

#importations d'autres packages utiles
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

# La base de données a été au prélable transformée (pour enlever tous les caractères spéciaux) 
# puis convertie en fichier csv
setwd("/Users/SuzanneSigalla/Documents/Travail these LaureCsm/")
data <- read.csv("data.csv",sep=";",encoding="latin1",header=F,stringsAsFactors = FALSE)

# on travaille sur le nom des colonnes pour enlever les sauts à la ligne qui rendent 
# les noms plus compliqués à lire
col_names <- data[1,]
replace_linebreaks <- function(x)
{return (gsub("[\r\n]", " ",x))}
col_names <- apply(col_names,1:2,FUN=replace_linebreaks)
names(data) <- col_names
data <- data[-1,]


# quelques petits ajustements (problème de codage -- nettoyage de la bdd)
data$Deces[data$Deces=='Non '] <- 'Non'
data$`ASA-PS` [data$`ASA-PS` %in% c('7, inconnu')] <- NA
data$`Anomalie pupillaire (Pre-hospitalier)`[data$`Anomalie pupillaire (Pre-hospitalier)` %in%
                                               c('Anisocorie unilaterale','Anomalie pupillaire',
                                                 'Mydriase\n bilaterale')] <- 'Oui'







