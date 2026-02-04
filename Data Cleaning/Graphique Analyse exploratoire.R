library(dplyr)
library(ggplot2)
library(rstudioapi)

##---- Étape 1 - 📁 Prise en main (2h) ---
#Importation des données
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
currentpath=getwd()
getwd()
data <- read.csv(paste0(currentpath,"/nettoyage_data.csv"))

                          ##------#Univariée :-------##  

#calculer les indicateurs de chaque variable : 
# Exemple : calculer min, max et moyenne pour une colonne "ma_colonne" du dataset "data"

data %>% group_by(pnns_groups_1) %>% 
  summarise(
    Min = min(energy_100g, na.rm = TRUE),
    Max = max(energy_100g, na.rm = TRUE),
    Moyenne = mean(energy_100g, na.rm = TRUE)
  )



# 1️⃣ Histogramme (distribution d'une variable)

ggplot(data, aes(x = pnns_groups_1)) +
  geom_bar(fill = "blue", color = "black") + 
  labs(title = "Répartition du nombre de produits par catégorie ",
       x = "Groupes de produit", 
       y = "Fréquence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotation des labels si nécessaire


# 3️⃣ Histogramme (nombre d'observations parcatégoriee)

ggplot(data, aes(x = nutriscore)) +
  geom_bar(fill = "blue", color = "black") + 
  labs(title = "Répartition du nombre de produits par catégorie ",
       x = "Groupes de produit", 
       y = "Fréquence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotation des labels si nécessaire



# 2️⃣ Boxplot (distribution des valeurs par espèce


ggplot(data, aes(y = energy_100g)) +
  geom_boxplot(alpha = 0.7, color = 'blue') +
  labs(title = "Boxplot de l'energie",
       x = "Energy_100g",
       y = "Valeur energétique") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotation des labels si nécessaire



ggplot(data, aes(x = pnns_groups_1, y = energy_100g, fill = pnns_groups_1)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de l'energie",
       x = "Catégorie de produit",
       y = "Valeur energétique") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotation des labels si nécessaire










#Multivariée :





#ACP pour groupe par les variables en 100g
