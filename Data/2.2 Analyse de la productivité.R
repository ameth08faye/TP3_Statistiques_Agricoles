source(file.path(Datawork, "2.1_Préparation et Nettoyage des données.R"))

# 8. Calculer pour chaque ménage l'indice de diversité des cultures
#a
data_reshaped <- var_agregee %>%
  group_by(id_men, culture_label) %>%
  summarize(across(c(superficie, production_kg), sum)) %>% #on agrege d'abord
  pivot_wider(names_from = culture_label, values_from = c(superficie, production_kg)) %>%
  ungroup()

data_reshaped[] <- lapply(data_reshaped, as.numeric)

## b. La part des différentes spéculations

# i. Calculer la somme totale des superficies

data_reshaped$total_superficie <- rowSums(data_reshaped[, 2:33],na.rm = TRUE)
#ii. Calculer la somme des parts au carré de chaque culture

data_reshaped$parts_au_carre_sum <- rowSums(data_reshaped[, 2:33]^2/data_reshaped$total_superficie^2, na.rm = TRUE)

#iii. Calculer l'indice pour chaque ligne
data_reshaped$indice <- 1 - data_reshaped$parts_au_carre_sum

# 9. Calculer pour chaque ménage les rendements des différentes cultures
labels_specifiques <- c("Mil", "Riz", "Sorgo", "Mais", "Arachide", "Niébé")

rendements <- data_production %>%
  filter(culture_label %in% labels_specifiques) %>%
  group_by(id_men, culture_label) %>%
  summarise(rendement = sum(production_kg) / sum(superficie),
            superficie = sum(superficie))


# 10. Calculer la part de l'arachide dans les superficies emblavées et effectuer le test t

#a. Calculer la part d'arachide pour les superficies totales

part_arachide1 <- sum(rendements$superficie[rendements$culture_label == 'Arachide']) / sum(rendements$superficie)

#b Filtrer les observations avec des superficies de plus de 5 hectares

rendements_plus_5ha1 <- rendements[rendements$superficie > 5, ]

# Calculer la part d'arachide pour les superficies de plus de 5 hectares

part_arachide_plus_5ha1 <- sum(rendements_plus_5ha1$superficie[rendements_plus_5ha1$culture_label == 'Arachide']) / sum(rendements_plus_5ha1$superficie)

# Effectuer le test t

ttest_result <- t.test(x = rendements$superficie[rendements$culture_label == 'Arachide'], 
                       y = rendements_plus_5ha1$superficie[rendements_plus_5ha1$culture_label == 'Arachide'])

# 11. Calculer pour chaque département la part des différentes spéculations dans les superficies emblavées
part_speculations_departement <- data_production %>%
  group_by(departements, culture_label) %>%
  summarise(superficie = sum(superficie)) %>%
  group_by(departements) %>%
  mutate(part_speculation = superficie / sum(superficie)) %>%
  filter(culture_label %in% c("Mil", "Mais", "Riz", "Sorgho", "Arachide", "Niébé"))

# Diagramme circulaire
diagramme_circulaire <- ggplot(part_speculations_departement, aes(x = "", y = part_speculation, fill = culture_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~departements) +
  theme_void() +
  labs(fill = "Culture")

# 12. Calculer pour chaque département les rendements des différentes cultures

# Liste des cultures à considérer

cultures_a_considerer <- c("Mil", "Riz", "Sorgo", "Niébé", "Arachide", "Mais")

# Calculer les rendements par département pour les cultures spécifiées

rendements_departement <- data_production %>%
  filter(culture_label %in% cultures_a_considerer) %>%
  group_by(departements, culture_label) %>%
  summarise(rendement = sum(production_kg) / sum(superficie),
            superficie = sum(superficie))

# Exporter le tableau dans un fichier Excel

library(openxlsx)
write.xlsx(rendements_departement, file = file.path(datapath, "data_base_1.csv"), rowNames = FALSE)
