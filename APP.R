library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)
library(FactoMineR)
library(factoextra)
library(scales)

# Charger les données
setwd(dir = dirname(rstudioapi::getActiveDocumentContext()$path))
chemin <- getwd()
data <- read.csv(paste0(chemin, '/nettoyage_data.csv'))

# Palette de couleurs nutritionnelle étendue
nutrition_palette <- c("#4CAF50", "#8BC34A", "#FF9800", "#FF5722", "#795548", "#607D8B", 
                       "#2E7D32", "#66BB6A", "#FFC107", "#E65100", "#5D4037", "#455A64",
                       "#81C784", "#AED581", "#FFD54F", "#FF8A65", "#8D6E63", "#78909C")
nutriscore_colors <- c("A" = "#00C851", "B" = "#7CB342", "C" = "#FFB300", "D" = "#FF8A00", "E" = "#FF3547")

# Interface utilisateur (UI)
ui <- fluidPage(
  # Thème personnalisé avec couleurs nutrition
  theme = bs_theme(
    bootswatch = "flatly", 
    version = 5,
    primary = "#4CAF50",      # Vert principal
    secondary = "#8BC34A",    # Vert clair
    success = "#2E7D32",      # Vert foncé
    warning = "#FF9800",      # Orange
    danger = "#FF5722",       # Rouge-orange
    bg = "#F8F9FA",          # Fond clair
    fg = "#2E7D32"           # Texte vert foncé
  ),
  
  # CSS minimal pour le thème nutrition
  tags$head(
    tags$style(HTML("
      body { background: linear-gradient(135deg, #F1F8E9, #E8F5E8); }
      .well { background: #FFFFFF; border: 2px solid #8BC34A; border-radius: 10px; }
      .nav-tabs .nav-link.active { background-color: #4CAF50 !important; color: white !important; }
      h1, h3, h4 { color: #2E7D32; font-weight: bold; }
    "))
  ),
  
  # Titre avec style nutrition
  div(style = "text-align: center; background: linear-gradient(90deg, #4CAF50, #8BC34A); 
              color: white; padding: 20px; margin-bottom: 20px; border-radius: 10px;",
      h1("Analyse Nutritionnelle des données Open Food Facts", style = "margin: 0; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);")),
  
  tabsetPanel(
    tabPanel("🔍 Vue d'ensemble",
             fluidRow(
               column(6,
                      div(style = "background: #E8F5E8; padding: 15px; border-radius: 10px; border-left: 5px solid #4CAF50;",
                          icon("filter", style = "color: #4CAF50; margin-right: 10px;"),
                          selectInput("selected_group", "Choisir une catégorie PNNS :",
                                      choices = c("Tout voir", unique(data$pnns_groups_1)),
                                      selected = "Tout voir")
                      )),
               column(6,
                      div(style = "background: #FFF3E0; padding: 15px; border-radius: 10px; border-left: 5px solid #FF9800;",
                          icon("chart-bar", style = "color: #FF9800; margin-right: 10px;"),
                          selectInput("selected_var", "Choisir une variable :",
                                      choices = names(data)[sapply(data, is.numeric)])
                      ))
             ),
             fluidRow(
               column(12,
                      div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                          h4("📊 Résumé statistique", style = "color: #2E7D32; border-bottom: 2px solid #8BC34A; padding-bottom: 10px;"),
                          tableOutput("summary_table"),
                          div(style = "background: #F1F8E9; padding: 10px; border-radius: 5px; margin: 10px 0;",
                              textOutput("summary_text")),
                          br(),
                          plotOutput("bar_plot"),
                          plotOutput("nutriscore_plot")
                      ))
             )
    ),
    tabPanel("🧮 Analyse multivariée",
             div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                 h4("ACP - Cercle des corrélations et clustering K-means", style = "color: #2E7D32;"),
                 fluidRow(
                   column(6,
                          div(style = "background: #E3F2FD; padding: 15px; border-radius: 8px;",
                              numericInput("k_cluster", "Choisir le nombre de clusters K-means :", value = 3, min = 2, max = 10)
                          )),
                   column(6,
                          div(style = "padding: 15px;",
                              actionButton("run_acp", "🚀 Lancer l'analyse", 
                                           class = "btn-success", style = "width: 100%; font-weight: bold;")
                          ))
                 ),
                 # Mise en page côte à côte pour les graphiques
                 fluidRow(
                   column(6,
                          plotOutput("correlation_circle", height = "550px")
                   ),
                   column(6,
                          plotOutput("kmeans_acp_plot", height = "550px")
                   )
                 ),
                 div(style = "background: #F1F8E9; padding: 15px; border-radius: 8px; border-left: 4px solid #4CAF50; margin-top: 20px;",
                     textOutput("acp_cluster_comment"))
             )
    ),
    tabPanel("🔬 Interprétation des clusters",
             div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
                 h4("💡 Compréhension des groupes identifiés", style = "color: #2E7D32;"),
                 div(style = "background: #E8F5E8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                     p("Ce Dashbord permet de comprendre comment sont formés les Clusters il détail les caractéristiques de chaque cluster identifié par l'analyse K-means lancé sur la page Analyse Multivariée")),
                 
                 # Mise en page côte à côte pour les tableaux
                 fluidRow(
                   column(6,
                          h4("Caractérisation des clusters", style = "color: #FF9800; border-bottom: 2px solid #FFB74D; padding-bottom: 5px;"),
                          tableOutput("cluster_characteristics")
                   ),
                   column(6,
                          h4("Profils nutritionnels par cluster", style = "color: #FF9800; border-bottom: 2px solid #FFB74D; padding-bottom: 5px;"),
                          div(style = "overflow-x: auto;",
                              tableOutput("cluster_profiles"))
                   )
                 ),
                 br(),
                 h4("Distribution des groupes PNNS par cluster", style = "color: #FF9800; border-bottom: 2px solid #FFB74D; padding-bottom: 5px;"),
                 plotOutput("cluster_pnns_plot")
             )
    ),
    tabPanel("📋 Table complète",
             fluidRow(
               column(6,
                      div(style = "background: #E8F5E8; padding: 15px; border-radius: 10px;",
                          selectInput("filter_group", " Filtrer par groupe PNNS :",
                                      choices = c("Tout voir", unique(data$pnns_groups_1)),
                                      selected = "Tout voir")
                      )),
               column(6,
                      div(style = "background: #FFF3E0; padding: 15px; border-radius: 10px;",
                          selectInput("filter_nutri", " Filtrer par Nutriscore :",
                                      choices = c("Tout voir", "A", "B", "C", "D", "E"),
                                      selected = "Tout voir")
                      ))
             ),
             div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-top: 20px;",
                 h3("📊 Table interactive des données", style = "color: #2E7D32;"),
                 DTOutput("full_table")
             )
    )
  )
)

# Serveur
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$selected_group == "Tout voir") {
      data
    } else {
      data %>% filter(pnns_groups_1 == input$selected_group)
    }
  })
  
  output$summary_table <- renderTable({
    filtered_data() %>%
      summarise(
        Min = min(!!sym(input$selected_var), na.rm = TRUE),
        Max = max(!!sym(input$selected_var), na.rm = TRUE),
        Moyenne = mean(!!sym(input$selected_var), na.rm = TRUE)
      )
  }, striped = TRUE, bordered = TRUE)
  
  output$summary_text <- renderText({
    moyenne <- mean(filtered_data()[[input$selected_var]], na.rm = TRUE)
    paste0("📊 La moyenne de ", input$selected_var, " pour ",
           input$selected_group, " est de ", round(moyenne, 2))
  })
  
  output$bar_plot <- renderPlot({
    n_groups <- length(unique(filtered_data()$pnns_groups_1))
    colors_to_use <- rep(nutrition_palette, ceiling(n_groups / length(nutrition_palette)))[1:n_groups]
    
    ggplot(filtered_data(), aes_string(x = "pnns_groups_1", y = input$selected_var, fill = "pnns_groups_1")) +
      stat_summary(fun = mean, geom = "bar", color = "white", size = 0.5) +
      scale_fill_manual(values = colors_to_use) +
      labs(title = paste("Valeur moyenne de", input$selected_var, "par groupe PNNS"),
           x = "Groupes PNNS", y = input$selected_var) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#2E7D32", face = "bold"),
        legend.position = "none",
        plot.title = element_text(color = "#2E7D32", size = 14, face = "bold"),
        axis.title = element_text(color = "#2E7D32", face = "bold"),
        panel.grid.major = element_line(color = "#E8F5E8"),
        panel.background = element_rect(fill = "#FAFAFA", color = NA)
      )
  })
  
  output$nutriscore_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = "nutriscore", y = input$selected_var, fill = "nutriscore")) +
      stat_summary(fun = mean, geom = "bar", color = "white", size = 0.5) +
      scale_fill_manual(values = nutriscore_colors) +
      labs(title = paste("🏷️ Valeur moyenne de", input$selected_var, "par Nutriscore"),
           x = "Nutriscore", y = input$selected_var) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(color = "#2E7D32", size = 14, face = "bold"),
        axis.title = element_text(color = "#2E7D32", face = "bold"),
        axis.text = element_text(color = "#2E7D32", face = "bold"),
        panel.grid.major = element_line(color = "#E8F5E8"),
        panel.background = element_rect(fill = "#FAFAFA", color = NA)
      )
  })
  
  table_filtered <- reactive({
    df <- data
    if (input$filter_group != "Tout voir") {
      df <- df %>% filter(pnns_groups_1 == input$filter_group)
    }
    if (input$filter_nutri != "Tout voir") {
      df <- df %>% filter(nutriscore == input$filter_nutri)
    }
    df
  })
  
  output$full_table <- renderDT({
    datatable(table_filtered(), 
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover',
              rownames = FALSE) %>%
      formatStyle(columns = names(table_filtered()), 
                  backgroundColor = '#FAFAFA',
                  color = '#2E7D32')
  })
  
  observeEvent(input$run_acp, {
    output$correlation_circle <- renderPlot({
      numeric_data <- data %>% select(where(is.numeric)) %>% na.omit()
      acp_res <- PCA(numeric_data, graph = FALSE)
      fviz_pca_var(acp_res, 
                   col.var = "contrib", 
                   gradient.cols = c("#4CAF50", "#FF9800", "#FF5722"), 
                   repel = TRUE,
                   title = "Cercle des corrélations") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2E7D32", size = 14, face = "bold"),
          axis.title = element_text(color = "#2E7D32", face = "bold"),
          panel.background = element_rect(fill = "#FAFAFA", color = NA)
        )
    })
    
    output$kmeans_acp_plot <- renderPlot({
      # Préparation des données
      numeric_data <- data %>% select(where(is.numeric)) %>% na.omit()
      
      # Réaliser l'ACP d'abord
      acp_res <- PCA(numeric_data, graph = FALSE)
      
      # Extraire les coordonnées des individus sur les 2 premières dimensions
      ind_coords <- acp_res$ind$coord[, 1:2]
      
      # Appliquer K-means sur les coordonnées ACP (plus stable)
      set.seed(123)
      res_km <- kmeans(ind_coords, centers = input$k_cluster, nstart = 25)
      
      # Créer le graphique avec fviz_cluster en utilisant les coordonnées ACP
      p <- fviz_cluster(list(data = ind_coords, cluster = res_km$cluster),
                        geom = "point", 
                        ellipse.type = "convex", 
                        show.clust.cent = TRUE,
                        pointsize = 1.5,
                        palette = nutrition_palette[1:input$k_cluster],
                        main = paste("Clustering K-means sur les axes principaux (K =", input$k_cluster, ")"),
                        xlab = paste("Dim1 (", round(acp_res$eig[1,2], 1), "%)"),
                        ylab = paste("Dim2 (", round(acp_res$eig[2,2], 1), "%)")) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(color = "#2E7D32", size = 14, face = "bold"),
          axis.title = element_text(color = "#2E7D32", face = "bold"),
          panel.background = element_rect(fill = "#FAFAFA", color = NA)
        )
      
      # Ajuster les limites des axes pour mieux voir les données
      coord_range_x <- range(ind_coords[,1])
      coord_range_y <- range(ind_coords[,2])
      margin_x <- diff(coord_range_x) * 0.1
      margin_y <- diff(coord_range_y) * 0.1
      
      p + coord_cartesian(xlim = c(coord_range_x[1] - margin_x, coord_range_x[2] + margin_x),
                          ylim = c(coord_range_y[1] - margin_y, coord_range_y[2] + margin_y))
    })
    
    output$acp_cluster_comment <- renderText({
      numeric_data <- data %>% select(where(is.numeric)) %>% na.omit()
      acp_res <- PCA(numeric_data, graph = FALSE)
      variance_explained <- round(sum(acp_res$eig[1:2, 2]), 1)
      
      paste0("📈 L'ACP explique ", variance_explained, "% de la variance totale sur les 2 premières dimensions. ",
             "Le clustering K-means avec K = ", input$k_cluster, " groupes est appliqué sur ces dimensions principales, ",
             "permettant d'identifier des profils nutritionnels distincts parmi les produits.")
    })
    
    # Caractérisation des clusters
    output$cluster_characteristics <- renderTable({
      numeric_data <- data %>% select(where(is.numeric)) %>% na.omit()
      acp_res <- PCA(numeric_data, graph = FALSE)
      ind_coords <- acp_res$ind$coord[, 1:2]
      
      set.seed(123)
      res_km <- kmeans(ind_coords, centers = input$k_cluster, nstart = 25)
      
      # Ajouter les clusters aux données originales
      data_with_clusters <- numeric_data
      data_with_clusters$cluster <- as.factor(res_km$cluster)
      
      # Calculer les moyennes par cluster pour les variables les plus importantes
      cluster_summary <- data_with_clusters %>%
        group_by(cluster) %>%
        summarise(
          Effectif = n(),
          .groups = 'drop'
        )
      
      # Ajouter le pourcentage
      cluster_summary$Pourcentage <- round(cluster_summary$Effectif / sum(cluster_summary$Effectif) * 100, 1)
      
      cluster_summary
    }, striped = TRUE, bordered = TRUE, digits = 1)
    
    # Graphique distribution PNNS par cluster
    output$cluster_pnns_plot <- renderPlot({
      numeric_data <- data %>% select(where(is.numeric)) %>% na.omit()
      acp_res <- PCA(numeric_data, graph = FALSE)
      ind_coords <- acp_res$ind$coord[, 1:2]
      
      set.seed(123)
      res_km <- kmeans(ind_coords, centers = input$k_cluster, nstart = 25)
      
      # Récupérer les indices des lignes conservées après na.omit
      complete_rows <- complete.cases(data %>% select(where(is.numeric)))
      data_complete <- data[complete_rows, ]
      data_complete$cluster <- as.factor(res_km$cluster)
      
      # Créer le graphique
      ggplot(data_complete, aes(x = cluster, fill = pnns_groups_1)) +
        geom_bar(position = "fill", color = "white", size = 0.3) +
        scale_fill_manual(values = nutrition_palette) +
        labs(title = "Distribution des groupes PNNS par cluster",
             x = "Cluster", y = "Proportion", fill = "Groupes PNNS") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 0, color = "#2E7D32", face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(size = 10, color = "#2E7D32", face = "bold"),
          legend.text = element_text(size = 8, color = "#2E7D32"),
          plot.title = element_text(color = "#2E7D32", size = 14, face = "bold"),
          axis.title = element_text(color = "#2E7D32", face = "bold"),
          panel.background = element_rect(fill = "#FAFAFA", color = NA)
        ) +
        guides(fill = guide_legend(ncol = 3))
    })
    
    # Profils nutritionnels détaillés par cluster
    output$cluster_profiles <- renderTable({
      numeric_data <- data %>% select(where(is.numeric)) %>% na.omit()
      acp_res <- PCA(numeric_data, graph = FALSE)
      ind_coords <- acp_res$ind$coord[, 1:2]
      
      set.seed(123)
      res_km <- kmeans(ind_coords, centers = input$k_cluster, nstart = 25)
      
      data_with_clusters <- numeric_data
      data_with_clusters$cluster <- as.factor(res_km$cluster)
      
      # Sélectionner les variables nutritionnelles les plus importantes
      nutritional_vars <- intersect(names(numeric_data), 
                                    c("energy_100g", "fat_100g", "saturated.fat_100g", 
                                      "carbohydrates_100g", "sugars_100g", "fiber_100g", 
                                      "proteins_100g", "salt_100g", "sodium_100g"))
      
      if(length(nutritional_vars) > 0) {
        cluster_profiles <- data_with_clusters %>%
          group_by(cluster) %>%
          summarise(across(all_of(nutritional_vars), 
                           ~ round(mean(.x, na.rm = TRUE), 2)),
                    .groups = 'drop')
        
        # Renommer les colonnes pour plus de clarté
        names(cluster_profiles) <- c("Cluster", 
                                     gsub("_100g", " (/100g)", 
                                          gsub("\\.", " ", names(cluster_profiles)[-1])))
        cluster_profiles
      } else {
        # Si les variables nutritionnelles spécifiques ne sont pas trouvées,
        # prendre les 5 premières variables numériques
        first_vars <- names(numeric_data)[1:min(5, ncol(numeric_data))]
        cluster_profiles <- data_with_clusters %>%
          group_by(cluster) %>%
          summarise(across(all_of(first_vars), 
                           ~ round(mean(.x, na.rm = TRUE), 2)),
                    .groups = 'drop')
        cluster_profiles
      }
    }, striped = TRUE, bordered = TRUE, digits = 2)
  })
}

# Lancer l'app
shinyApp(ui = ui, server = server)
