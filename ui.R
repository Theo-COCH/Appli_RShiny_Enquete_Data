
library(shiny)
library(bslib)

# UI --------------------------------------------------

fluidPage(
  
  # THEME GENERAL --------------------------
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#24B8B0",
    secondary = "#FF9B76",
    bg = "#101B32",
    fg = "#F3F7FF",
    base_font = font_collection("system-ui")
  ),
  
  # STYLE PERSONNALISE ----------------------------
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #101B32;
      }

      .navbar {
        border: none;
        margin-bottom: 0;
      }

      .navbar-brand {
        font-weight: 800;
        letter-spacing: 1.5px;
      }

      .page-header-data {
        background: linear-gradient(120deg, #1C2B47, #263B5D);
        padding: 28px;
        border-radius: 14px;
        margin: 22px 0;
        border-left: 5px solid #24D6C8;
      }

      .page-header-data h2 {
        color: #24D6C8;
        font-weight: 800;
        margin-top: 0;
      }

      .page-header-data p {
        color: #D2DDF0;
        font-size: 15px;
      }

      .well {
        background-color: #1C2B47;
        border: 1px solid #344766;
        border-radius: 12px;
        box-shadow: none;
      }

      .control-label {
        color: #24D6C8;
        font-weight: 600;
      }

      .form-control {
        border-radius: 7px;
      }

      .panel-resultat {
        background-color: #1C2B47;
        border: 1px solid #344766;
        border-radius: 12px;
        padding: 18px;
        margin-bottom: 20px;
      }

      .panel-resultat h3 {
        color: #24D6C8;
        margin-top: 0;
      }

      .footer-data {
        text-align: left;
        color: #24B8B0;
        font-size: 12px;
        padding: 15px 0;
      }
      
      .footer-author {
        text-align: right;
        color: #F3F7FF;
        font-size: 12px;
        padding: 15px 0;
      }
    "))
  ),
  
  # NAVIGATION -------------------------------
  
  navbarPage(
    
    title = tagList(
      icon("chart-line"),
      "ENQUÊTE DATA"
    ),
    
    id = "navigation",
    
    collapsible = TRUE,
    
    # PAGE PRINCIPALE -----------------------------
    
    tabPanel(
      
      title = "Explorer les résultats",
      
      # INTRODUCTION ----------------------------------
      
      fluidRow(
        column(
          width = 12,
          
          div(
            class = "page-header-data",
            
            h2("Observation des métiers de la data"),
            
            p(
              "Explorez les réponses de l'enquête ",
              "à travers les secteurs d'activité, ",
              "le télétravail, l'expérience professionnelle ",
              "et l'utilisation des outils data."
            ),
            
            p(
              strong("Effectifs et pourcentages"),
              " | Filtres par métier et par genre"
            )
          )
        )
      ),
      
      # FILTRES ET RESULTATS --------------------------
      
      sidebarLayout(
        
        sidebarPanel(
          
          width = 3,
          
          h3("Paramètres"),
          
          p("Personnalisez les résultats à afficher."),
          
          tags$hr(),
          
          selectInput(
            inputId = "choix_question",
            label = "Question de l'enquête",
            choices = c(
              "A4 — Secteur d'activité" = "A4",
              "A5 — Télétravail" = "A5",
              "A6 — Expérience" = "A6",
              "B2 — Utilisation des outils" = "B2"
            ),
            selected = "A4"
          ),
          
          selectInput(
            inputId = "choix_poste",
            label = "Métier",
            choices = c(
              "Tous les postes" = "Tous les postes",
              "Data analyst" = "Data analyst",
              "Data scientist" = "Data scientist",
              "Data engineer" = "Data engineer"
            ),
            selected = "Tous les postes"
          ),
          
          selectInput(
            inputId = "choix_genre",
            label = "Genre",
            choices = c(
              "Tous les genres" = "Tous les genres",
              sort(unique(
                df_enquete$Y1A_genre[
                  !is.na(df_enquete$Y1A_genre)
                ]
              ))
            ),
            selected = "Tous les genres"
          ),
          
          tags$hr(),
          
          p(
            style = "color: #A9BCD8; font-size: 12px;",
            icon("circle-info"),
            "Les résultats se mettent à jour ",
            "automatiquement selon les filtres."
          )
        ),
        
        # ZONE PRINCIPALE -----------------------------
        
        mainPanel(
          
          width = 9,
          
          div(
            class = "panel-resultat",
            
            h3(
              icon("chart-column"),
              " Visualisation"
            ),
            
            plotOutput(
              outputId = "plot_resultats",
              height = "520px",
              width = "100%"
            )
          ),
          
          div(
            class = "panel-resultat",
            
            h3(
              icon("table"),
              " Résultats détaillés"
            ),
            
            p(
              style = "color: #A9BCD8;",
              "Effectifs et pourcentages par modalité."
            ),
            
            tableOutput("table_resultats")
          )
        )
      ),
      
      # Pied de page
      
      fluidRow(
        column(
          width = 8,
          div(
            class = "footer-data",
            "ENQUÊTE DATA | Analyse des réponses de l'enquête"
          )
        ),
        column(
          width = 4,
          div(
            class = "footer-author",
            "Réalisé par : AYEDJI Kokou - COCHET Théo"
          )
        )
      )
    )
  )
)
