source(file.path(scripts, "2.3 Analyse des intrants.R"))

#13 Visualisons la diversité des cultures dans les différentes régions

# Calculons l'indice de diversité des cultures par région

# Créons une nouvelle base en conservant uniquement les premières occurrences de chaque id_men

nouvelle_base <- data_production %>% distinct(id_men, .keep_all = TRUE)

#joignons la nouvelle base de données à la base reshaped

data_reshaped$regions <- nouvelle_base$regions


# Visualisons  l'indice de diversité par région

#Calculons la moyenne de l'indice de diversité par région
average_indices <- aggregate(indice ~ regions, data_reshaped, mean)

#Traçons les indices moyens
ggplot(average_indices, aes(x = regions, y = indice, fill = regions)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Région", y = "Moyenne de l'indice de diversité", fill = "Région") +
  ggtitle("Moyenne de l'indice de diversité des cultures par région")

# Créons la mini-base de données en calculant les rendements de l'arachide par région

mini_base_riz <- data_production %>%
  filter(culture_label == "Riz") %>%
  group_by(regions) %>%
  summarise(rendement_riz = sum(production_kg) / sum(superficie))

#faisons le test

ttest_result_cult_riz <- t.test(data_reshaped$indice, mini_base_riz$rendement_riz)


#16) Visualisons la provenance des semences pour les différentes spéculations
#D'abord, faisons une fusion de bases concernées 

fusion23_2_riz <-merge(data_imputed, data_semence, by = c("id_men", "culture_label"))

# Filtrons les données pour la culture "Arachide"
fusion23_2_riz <- fusion23_2_riz %>%
  filter(culture_label == "Riz")

# Créons un graphique à barres montrant les rendements par source de provenance
ggplot(fusion23_2_riz, aes(x = provenance, y = rendement)) +
  geom_bar(stat = "summary", fun = "mean", fill = "green") +
  labs(x = "Provenance des semences", y = "Rendement moyen du riz",
       title = "Rendements de nièbè par source de provenance des semences") +
  theme_minimal()

##Cherchons si l'utilisation de semences certifiées permet l'atteinte de meilleurs rendements
# Calculons la moyenne des rendements par source de provenance
mean_rendement <- aggregate(rendement ~ types_semences + culture_label, data = fusion23_2_riz, FUN = mean)

#Commentons les résultats par zone 
# Créer un graphique à barres montrant les rendements par source de provenance (par zone)
ggplot(fusion23_2_riz, aes(x = provenance, y = rendement, fill = zone)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(x = "Provenance des semences", y = "Rendement moyen de riz",
       title = "Rendements de riz par source de provenance des semences (par zone)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +  # Exemple de palette de couleurs
  theme(legend.position = "top")

##18) Reprenons la régression avec d'autres variables

#Faisons la régression entre le rendement et les conditions de culture



