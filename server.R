
library(shiny)


function(input, output, session) {
  
  # FILTRES -------------------------------------------
  
  ## Filtrer selon le poste et le genre
  
  df_filtre <- reactive({
    
    req(input$choix_poste, input$choix_genre)
    
    df <- df_enquete
    
    if (input$choix_poste != "Tous les postes") {
      df <- df |>
        filter(A3_poste == input$choix_poste)
    }
    
    if (input$choix_genre != "Tous les genres") {
      df <- df |>
        filter(Y1A_genre == input$choix_genre)
    }
    
    df
  })
  
  
  # Calculs résultats
  
  resultats <- reactive({
    
    req(input$choix_question)
    
    df <- df_filtre()
    
    # A4, A5 ou A6
    
    if (input$choix_question %in% c("A4", "A5", "A6")) {
      
      variable <- switch(
        input$choix_question,
        "A4" = "A4_secteur",
        "A5" = "A5_teleravail",
        "A6" = "A6_experience"
      )
      
      df |>
        transmute(
          reponse = .data[[variable]]
        ) |>
        count(reponse, name = "effectif") |>
        mutate(
          pourcentage = effectif / nrow(df) * 100
        ) |>
        arrange(desc(effectif))
      
    } else {
      
      # B2 : fréquences d'utilisation des outils
      
      df_b2_long |>
        filter(
          if (input$choix_poste == "Tous les postes") {
            TRUE
          } else {
            A3_poste == input$choix_poste
          },
          if (input$choix_genre == "Tous les genres") {
            TRUE
          } else {
            Y1A_genre == input$choix_genre
          }
        ) |>
        count(outil, frequence, name = "effectif") |>
        group_by(outil) |>
        mutate(
          pourcentage = effectif / sum(effectif) * 100
        ) |>
        ungroup() |>
        mutate(
          frequence = factor(
            frequence,
            levels = c(
              "Jamais",
              "Occasionnellement",
              "Régulièrement"
            )
          )
        ) |>
        arrange(outil, frequence)
    }
  })
  
  
  # Graphiques à afficher -----------------------------------------
  
  output$plot_resultats <- renderPlot({
    
    req(nrow(df_filtre()) > 0)
    
    donnees <- resultats()
    
    if (input$choix_question %in% c("A4", "A5", "A6")) {
      
      ggplot(
        donnees,
        aes(
          x = effectif,
          y = reorder(reponse, effectif)
        )
      ) +
        geom_col(fill = "dodgerblue3") +
        geom_text(
          aes(
            label = paste0(
              effectif, " (",
              round(pourcentage, 1), "%)"
            )
          ),
          hjust = -0.1,
          size = 3.5
        ) +
        labs(
          title = paste(
            "Résultats de la question",
            input$choix_question
          ),
          subtitle = "Effectifs et pourcentages",
          x = "Nombre de répondants",
          y = ""
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.25))
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(
            size = 15,
            face = "bold"
          ),
          plot.title.position = "plot"
        )
      
    } else {
      
      ggplot(
        donnees,
        aes(
          x = pourcentage,
          y = reorder(outil, pourcentage),
          fill = frequence
        )
      ) +
        geom_col(
          position = "stack"
        ) +
        geom_text(
          aes(
            label = ifelse(
              pourcentage >= 7,
              paste0(round(pourcentage), "%"),
              ""
            )
          ),
          position = position_stack(vjust = 0.5),
          size = 3
        ) +
        labs(
          title = "Fréquence d'utilisation des outils",
          subtitle = "Question B2",
          x = "Pourcentage des répondants",
          y = "",
          fill = "Fréquence"
        ) +
        scale_x_continuous(
          breaks = seq(0, 100, 20),
          labels = function(x) paste0(x, "%"),
          limits = c(0, 100),
          expand = c(0, 0)
        ) +
        scale_fill_manual(
          values = c(
            "Jamais" = "grey75",
            "Occasionnellement" = "dodgerblue2",
            "Régulièrement" = "royalblue4"
          ),
          drop = FALSE
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(
            size = 15,
            face = "bold"
          ),
          legend.position = "bottom"
        )
    }
    
  })
  
  
  # Tableau reporting -----------------------------
  
  output$table_resultats <- renderTable({
    
    donnees <- resultats()
    
    if (input$choix_question %in% c("A4", "A5", "A6")) {
      
      donnees |>
        transmute(
          Réponse = reponse,
          Effectif = effectif,
          `Pourcentage (%)` = round(pourcentage, 1)
        )
      
    } else {
      
      donnees |>
        transmute(
          Outil = outil,
          Fréquence = frequence,
          Effectif = effectif,
          `Pourcentage (%)` = round(pourcentage, 1)
        )
    }
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  digits = 1
  )
  
}


