source( file.path(Datawork, "0_debugging.R"))
#1 Importons la base de données

inputfile <- file.path(datapath, "data_base.csv")
data_production <- read.csv(inputfile)

#2 vérifions s'il y a des doublons dans la base 

any(duplicated(data_production))  # Il n' ya pas de doublons dans la base parce que le resultat ici est "FALSE"
doublons <- sum(duplicated(data_production))

#3 cherchons les céréales cultivées

cultures_cultivées<- unique(data_production$culture_label)
# parmi les cultures, nous avons comme céréales: mil,mais,sorgo,riz

#4 Verifions s'il y a des superficies nulles

superficie_nulle <- any(data_production$superficie==0)
cat("Il y a des superficie nulle ! :", superficie_nulle , "\n")


# Vérifions s'il y a des valeurs aberrantes
#Cela revient à chercher s'il y a des rendements aberrants

# Calculer le rendement en divisant la production par la superficie

data_production$rendement <- data_production$production_kg / data_production$superficie

# Agréger les données par "departements" et "culture_label" en calculant la moyenne du rendement

average_yield <- aggregate(rendement ~ departements + culture_label, data_production, mean)

# Calculer les quartiles Q1 et Q3

Q1 <- quantile(average_yield$rendement, 0.25, na.rm = TRUE)
Q3 <- quantile(average_yield$rendement, 0.75, na.rm = TRUE)

# Calculer l'IQR (Interquartile Range)

IQR <- Q3 - Q1

# Calculer les bornes pour détecter les valeurs aberrantes

lower_bound <- 0 
upper_bound <- Q3 + 1.5 * IQR

# Filtrer les valeurs aberrantes par département et culture

the_outliers <- average_yield %>%
  group_by(rendement, culture_label) %>%
  filter(rendement < lower_bound | rendement > upper_bound)

# Afficher les valeurs aberrantes par département et culture

cat("Valeurs aberrantes par département et culture :\n")

#5 Imputer les valeurs aberrantes par la médiane du rendement de la culture dans le département

data_imputed <- data_production %>%
  group_by(departements, culture_label) %>%
  mutate(rendement = case_when(
    rendement < lower_bound | rendement > upper_bound ~ median(rendement, na.rm = TRUE),
    TRUE ~ rendement
  ))

#recalculons la production dans la base data_production pour voir si elles conviennent avec celles de la base initiale

data_production$production_kg <- data_imputed$superficie*data_imputed$rendement

# 6. Agréger les superficies et productions par ménage et par culture

var_agregee <- data_production %>%
  group_by(id_men, culture_label) %>%
  summarise(superficie = sum(superficie), production_kg = sum(production_kg))

# Enregistrer la base ainsi apurée avec un nom distinct
write.csv(var_agregee, file=file.path(datapath, "base_agregee.csv"), row.names = FALSE)
