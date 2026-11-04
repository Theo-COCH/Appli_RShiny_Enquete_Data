# 1. PACKAGES =======================================================

library(readr)    # Importation des données
library(dplyr)    # Traitement des données
library(tidyr)    # Réorganisation des données
library(forcats)  # Gestion des facteurs
library(ggplot2)  # Visualisation graphique
library(ggiraph)  # Dataviz interactive
library(ggtext)   # Complément ggplot2
library(shiny)    # Application interactive
library(bslib)    # Personnalisation de l'interface

# 2. CHARGEMENT DES DONNÉES ==========================

## Importation du fichier CSV
# Importation des données brutes avec read_csv2 pour gérer correctement le séparateur


df_enquete_raw <- read_csv2("Data/enquete_data_raw.csv")

# 3. TRAITEMENTS DES DONNEES -----------------------------

## Liste des variables B2 pour afficher dans UI

# La fonction grep() permet de récuperer le colonnes dont le nom commence par "B2_frequence_utilisation_"
vars_b2 <- grep(
  pattern = "^B2_frequence_utilisation_",
  x = names(df_enquete_raw),
  value = TRUE
)

## Exclusion des répondants non admissibles
## On exclue tout statut étudiant, tout ceux en recherche d'emploi et les postes "Autre"
## Afin de ne garder que les professionnels actifs

df_enquete <- df_enquete_raw |>
  filter(
    !A2_statut %in% c(
      "Etudiant (hors alternance / stage)",
      "En recherche d'emploi"
    ),!is.na(A3_poste),A3_poste != "Autre"
  )


## Recodage des postes et du genre
## Les chargés d'études et statisticiens sont regroupés.
## Les genres manquants apparaissent sous « Non renseigné ».

df_enquete <- df_enquete |>
  mutate(
    A3_poste = case_when(
      A3_poste %in% c(
        "Data Analyst / analyste",
        "Chargé d’études / chargé d’études statistiques"
      ) ~ "Data analyst",
      
      A3_poste %in% c(
        "Data Scientist",
        "Statisticien"
      ) ~ "Data scientist",
      
      A3_poste == "Data Engineer" ~ "Data engineer"
    ),
    
    Y1A_genre = if_else(
      is.na(Y1A_genre),
      "Non renseigné",
      Y1A_genre
    )
  )


## Traitement des non-réponses à A4, A5 et A6
## Les non-réponses de A4, A5 et A6 apparaissent sous « Non-réponse ».

df_enquete <- df_enquete |>
  mutate(
    across(
      all_of(c(
        "A4_secteur",
        "A5_teleravail",
        "A6_experience"
      )),
      ~ if_else(
        is.na(.x),
        "Non-réponse",
        as.character(.x)
      )
    )
  )


## Traitement des non-réponses à B2

df_enquete <- df_enquete |>
  mutate(
    across(
      all_of(vars_b2),
      ~ if_else(
        is.na(.x),
        "Jamais",
        as.character(.x)
      )
    )
  )


# 4. PREPARATION DES DONNEES B2 --------------------------

## Regrouper les outils dans une seule variable afin d'éviter 13 colonnes de répétitions
#df_b2_long contient une ligne par réponse admissible et par outil, avec les valeurs manquantes de fréquence remplacées par « Jamais ».
df_b2_long <- df_enquete |>
  # Transformation des colonnes B2 ("plusieurs colonnes d'outils") en format ligne avec pivot_longer()
  # (une ligne par outils et par répondant) pour permettre une agrégation et tracer le graphe empilé
  pivot_longer(
    cols = all_of(vars_b2),
    names_to = "outil",
    names_prefix = "B2_frequence_utilisation_",
    values_to = "frequence"
  )

