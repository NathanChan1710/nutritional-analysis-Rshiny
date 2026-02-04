library(dplyr)

##---- Étape 1 - 📁 Prise en main (2h) ---
#Importation des données
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
currentpath=getwd()
getwd()
data <- read.csv(paste0(currentpath,"/data.csv"))

#Exploration et visualisation du jeu de donnée 
str(data)
glimpse(data) 
head(data)
summary(data)

table(data$pnns_groups_1)
sapply(data,class) #class des variables

# Pourcentage de valeurs manquantes par colonne
val_manquante <- colSums(is.na(data)) / nrow(data) * 100

# Tableau récapitulatif
summary_table <- data.frame(Colonne = names(val_manquante),'Pourcentage_Valeurs_Manquantes' = val_manquante) %>% select(-Colonne)
print(summary_table)

##--- Étape 2 - 🧹 Nettoyage (3h) ---
test <- data %>% 
  filter(!pnns_groups_1 %in% c('unknown',''))


final <- test %>% 
  select(product_name,pnns_groups_1,energy_100g,fat_100g,carbohydrates_100g,sugars_100g,proteins_100g,salt_100g,nutrition.score.fr_100g)
#Convertir energy /4.2
final_val_manquante <- colSums(is.na(final)) / nrow(final) * 100
final_summary_table <- data.frame(Colonne = names(final_val_manquante),'Pourcentage_Valeurs_Manquantes' = final_val_manquante) %>% select(-Colonne)

final <- final %>% 
  filter((fat_100g >= 0 & fat_100g <= 100) &
           (carbohydrates_100g >= 0 & carbohydrates_100g <= 100) &
           (sugars_100g >= 0 & sugars_100g <= 100) &
           (proteins_100g >= 0 & proteins_100g <= 100) &
           (salt_100g >= 0 & salt_100g <= 100))

final <- final %>%
  mutate(kcal = energy_100g/4.2)

#On remplace les NA par la moyenne de chaque colonne
final_rempli <- final %>% 
  mutate(across(everything(), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

final_rempli <- final_rempli %>% 
  mutate(nutriscore = case_when(
    nutrition.score.fr_100g <= -1  ~ "A",
    nutrition.score.fr_100g <= 2  ~ "B",
    nutrition.score.fr_100g <= 10  ~ "C",
    nutrition.score.fr_100g <= 18  ~ "D",
    TRUE                  ~ "E"
  ))



write.csv(final_rempli,"nettoyage_data.csv",row.names = FALSE)
  