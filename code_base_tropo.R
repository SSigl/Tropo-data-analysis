#=========================================================================#
#                                                                         #
#                         Travail thèse -- Laure Csm                      #
#                                                                         #
#=========================================================================#

rm(list = ls())

# packages utiles pour l'importation et la transformation de la bdd
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(tibble)
library(car)

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
valid_column_names <- make.names(names=names(data), unique=TRUE, allow_ = TRUE)
names(data) <- valid_column_names

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ANALYSE DE LA BASE DE DONNÉES

# on s'intéresse à la variable sur laquelle on veut accomplir la régression
# ie la variable "Deces"
str(data$Deces)

# on recode cette variable pour qu'elle n'ait que deux niveaux ("Oui" et "Non") 
# au lieu de ("Oui", "Non" et "Non ")
# (problème classique en analyse de données : uniformiser les données)
levels(data$Deces)
data$Deces <- recode(data$Deces, "c('Non ')='Non'")

# on retire les lignes où l'on n'a pas d'information sur le décès 
#(puisque c'est ce qui nous intéresse ici)



# pour obtenir l'indice de la colonne Deces (utile dans la suite)
grep("Deces",colnames(data)) 



#data.surf <- VSURF(data[,-110],data[,110])
train <- data %>% sample_frac(0.8)
test <- anti_join(data, train)
(model <- randomForest(Deces ~ ., data = train, ntree = 500, na.action = na.omit))

sum(is.na(data[,3]))

is_val_miss <- function(x){return (sum(is.na(x)))}

missing_val <- apply(data,2,is_val_miss)










