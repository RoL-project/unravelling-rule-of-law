## Jaroslaw Kantorowicz and Bastián González-Bustamante
## Faculty of Governance and Global Affairs, Leiden University
## May-June 2024

## Packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

## Define UI for the app

ui <- fluidPage(theme = shinytheme("flatly"), tags$head(tags$script(src = "https://kit.fontawesome.com/dde009f50f.js")),
                navbarPage("Unravelling the Rule of Law",
                           tabPanel("Diachronic Clusters",
                                    fluidPage(
                                      titlePanel("Diachronic Clusters"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          tags$p(HTML("<b>Pre-processing:</b>")),
                                          tags$p(HTML("<ul><li>Tokenization</li><li>Standard cleaning</li><li>Key multiword collocations*</li></ul>")),
                                          tags$p(HTML("<b>word2vec parameters:</b>")),
                                          tags$p(HTML("<ul><li>Skim-gram with negative sampling</li><li>Training window size of 10</li><li>Vectors of 300 dimensions</li><li>Five epochs and bootstrapping of 30</li><li>Pruning low freq. and noisy words</li><li>Cython and 12-cores</li></ul>")),
                                          tags$p(HTML("<b>word2vec implementations:</b>")),
                                          tags$p(HTML("<ul><li>Chronologically trained models</li><li>Alternative chronologically trained models <small>(1925 onwards)</small></li></ul>")),
                                          tags$p(HTML("<b>BERT implementation:</b>")),
                                          tags$p(HTML("<ul><li>DistilBERT embeddings**</li></ul>")),
                                          radioButtons("select_country1", "Corpus:", 
                                                       choices = c("United Kingdom (filtered)" = "UK", "United States (filtered)" = "US", 
                                                                   "United States (full corpus)" = "US_full")),
                                          selectInput("model1", "Model:", 
                                                      choices = c("Chronologically Trained Model" = "ct", 
                                                                  "Alt. Chronologically Trained" = "alt_ct", 
                                                                  "DistilBERT" = "distilBERT")),
                                          checkboxInput("sameAxis1", "Standardise axis (from zero to one)", value = FALSE),
                                          downloadButton("downloadPlot1", "Download Plot"),
                                          tags$p(HTML("<br><small>* word2vec implementations only</small>")),
                                          tags$p(HTML("<small>** UK only for the moment</small>"))
                                          ),
                                        mainPanel(plotOutput("diachronicClusters", height = "768px", width = "1024px"))
                                        ))
                          ),
                          
                          tabPanel("Models Comparison",
                                   fluidPage(
                                     titlePanel("Models Comparison"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$p(HTML("<b>Pre-processing:</b>")),
                                         tags$p(HTML("<ul><li>Tokenization</li><li>Standard cleaning</li><li>Key multiword collocations*</li></ul>")),
                                         tags$p(HTML("<b>word2vec parameters:</b>")),
                                         tags$p(HTML("<ul><li>Skim-gram with negative sampling</li><li>Training window size of 10</li><li>Vectors of 300 dimensions</li><li>Five epochs and bootstrapping of 30</li><li>Pruning low freq. and noisy words</li><li>Cython and 12-cores</li></ul>")),
                                         tags$p(HTML("<b>word2vec implementations:</b>")),
                                         tags$p(HTML("<ul><li>Chronologically trained models</li><li>Alternative chronologically trained models <small>(1925 onwards)</small></li></ul>")),
                                         tags$p(HTML("<b>BERT implementation:</b>")),
                                         tags$p(HTML("<ul><li>DistilBERT embeddings**</li></ul>")),
                                         radioButtons("select_country2", "Corpus:", 
                                                      choices = c("United Kingdom (filtered)" = "UK", "United States (filtered)" = "US", 
                                                                  "United States (full corpus)" = "US_full")),
                                         selectInput("cluster2", "Cluster:",
                                                     choices = c("Procedural - Judiciary" = "judiciary", "Procedural - Rules" = "rules",
                                                                 "Substantive - Democracy" = "democracy", "Substantive - Rights" = "rights")),
                                         checkboxInput("sameAxis2", "Standardise axis (from zero to one)", value = FALSE),
                                         downloadButton("downloadPlot2", "Download Plot"),
                                         tags$p(HTML("<br><small>* word2vec implementations only</small>")),
                                         tags$p(HTML("<small>** UK only for the moment</small>"))
                                       ),
                                       mainPanel(plotOutput("diachronicModels", height = "768px", width = "1024px"))
                                     ))
                          ),
                          
                          tabPanel("Country Clusters",
                                   fluidPage(
                                     titlePanel("Country Clusters"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$p(HTML("<b>Pre-processing:</b>")),
                                         tags$p(HTML("<ul><li>Tokenization</li><li>Standard cleaning</li><li>Key multiword collocations*</li></ul>")),
                                         tags$p(HTML("<b>word2vec parameters:</b>")),
                                         tags$p(HTML("<ul><li>Skim-gram with negative sampling</li><li>Training window size of 10</li><li>Vectors of 300 dimensions</li><li>Five epochs and bootstrapping of 30</li><li>Pruning low freq. and noisy words</li><li>Cython and 12-cores</li></ul>")),
                                         tags$p(HTML("<b>Monolingual word2vec implementations:</b>")),
                                         tags$p(HTML("<ul><li>Monolingual embeddings <small>(original languages)</small></li></ul>")),
                                         tags$p(HTML("<b>Bilingual Word Embeddings (BWEs) pivot implementations:</b>")),
                                         tags$p(HTML("<ul><li>Machine-translated BWE models</li><li>Procrustes-aligned BWE <small>(English as an anchor)</small></li></ul>")),
                                         tags$p(HTML("<b>RoBERTa implementations:</b>")),
                                         tags$p(HTML("<ul><li><b>To be determined</b></li></ul>")),
                                         selectInput("country3", "Country:", 
                                                     choices = c("Germany" = "germany", "Spain" = "spain", "France" = "france", "Great Britain" = "uk",
                                                                 "Hungary" = "hungary", "Italy" = "italy", "Netherlands" = "netherlands", "Poland" = "poland")),
                                         checkboxInput("sameAxis3", "Standardise axis (from zero to one)", value = FALSE),
                                         downloadButton("downloadPlot3", "Download Plot"),
                                         tags$p(HTML("<br><small>* word2vec implementations only</small>"))
                                       ),
                                       mainPanel(plotOutput("parlamintClusters", height = "768px", width = "1024px"),
                                                 tags$p(HTML("<small><em>Note.</em> The original language corpora were processed with specific Spanish, French, Italian, Dutch, and Polish tokenizers. Hungarian was processed using a generic multilingual tokenizer. The monolingual models' recodification list is 109 multiword collocations, while the BWE consisted of 222 changes since they incorporated some typos corrections. For the original languages, that was not possible since the list of recodifications was translated with OPUS-MT transformers for each language, and typo correction could have affected the translation. Therefore, the machine-translated BWE for Great Britain is not only equivalent to the English-aligned embeddings but also to the monolingual one but with a different number of multiword recodifications during pre-processing.</small>")))
                                     ))
                          ),
                          
                          tabPanel("Cross-National Models",
                                   fluidPage(
                                     titlePanel("Cross-National Models"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$p(HTML("<b>Pre-processing:</b>")),
                                         tags$p(HTML("<ul><li>Tokenization</li><li>Standard cleaning</li><li>Key multiword collocations*</li></ul>")),
                                         tags$p(HTML("<b>word2vec parameters:</b>")),
                                         tags$p(HTML("<ul><li>Skim-gram with negative sampling</li><li>Training window size of 10</li><li>Vectors of 300 dimensions</li><li>Five epochs and bootstrapping of 30</li><li>Pruning low freq. and noisy words</li><li>Cython and 12-cores</li></ul>")),
                                         tags$p(HTML("<b>Monolingual word2vec implementations:</b>")),
                                         tags$p(HTML("<ul><li>Monolingual embeddings <small>(original languages)</small></li></ul>")),
                                         tags$p(HTML("<b>Bilingual Word Embeddings (BWEs) pivot implementations:</b>")),
                                         tags$p(HTML("<ul><li>Machine-translated BWE models</li><li>Procrustes-aligned BWE <small>(English as an anchor)</small></li></ul>")),
                                         tags$p(HTML("<b>RoBERTa implementations:</b>")),
                                         tags$p(HTML("<ul><li><b>To be determined</b></li></ul>")),
                                         selectInput("cluster4", "Cluster:",
                                                     choices = c("Procedural - Judiciary" = "judiciary_parlamint", "Procedural - Rules" = "rules_parlamint",
                                                                 "Substantive - Democracy" = "democracy_parlamint", "Substantive - Rights" = "rights_parlamint")),
                                         tags$p(HTML("<b>ParlaMint sample size:</b>")),
                                         checkboxInput("showCountries", "Short list of countries", value = TRUE),
                                         checkboxInput("sameAxis4", "Standardise axis (from zero to one)", value = FALSE),
                                         downloadButton("downloadPlot4", "Download Plot"),
                                         tags$p(HTML("<br><small>* word2vec implementations only</small>")),
                                       ),
                                       mainPanel(plotOutput("multilingualPlot", height = "768px", width = "1024px"),
                                                 tags$p(HTML("<small><em>Note.</em> The original language corpora were processed with specific Spanish, French, Italian, Dutch, and Polish tokenizers. Hungarian was processed using a generic multilingual tokenizer. The monolingual models' recodification list is 109 multiword collocations, while the BWE consisted of 222 changes since they incorporated some typos corrections. For the original languages, that was not possible since the list of recodifications was translated with OPUS-MT transformers for each language, and typo correction could have affected the translation. Therefore, the machine-translated BWE for Great Britain is not only equivalent to the English-aligned embeddings but also to the monolingual one but with a different number of multiword recodifications during pre-processing.</small>")))
                                     ))
                          ),
                          
                          tabPanel("Proof of Concept",
                                   fluidPage(
                                     titlePanel("Proof of Concept"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$p(HTML("<b>Pre-processing:</b>")),
                                         tags$p(HTML("<ul><li>Tokenization</li><li>Standard cleaning</li><li>Key multiword collocations*</li></ul>")),
                                         tags$p(HTML("<b>word2vec parameters:</b>")),
                                         tags$p(HTML("<ul><li>Skim-gram with negative sampling</li><li>Training window size of 10</li><li>Vectors of 300 dimensions</li><li>Five epochs and bootstrapping of 30</li><li>Pruning low freq. and noisy words</li><li>Cython and 12-cores</li></ul>")),
                                         tags$p(HTML("<b>Monolingual word2vec implementations:</b>")),
                                         tags$p(HTML("<ul><li>Monolingual embeddings <small>(original languages)</small></li></ul>")),
                                         tags$p(HTML("<b>Bilingual Word Embeddings (BWEs) pivot implementations:</b>")),
                                         tags$p(HTML("<ul><li>Machine-translated BWE models</li><li>Procrustes-aligned BWE <small>(English as an anchor)</small></li></ul>")),
                                         tags$p(HTML("<b>RoBERTa implementations:</b>")),
                                         tags$p(HTML("<ul><li><b>To be determined</b></li></ul>")),
                                         selectInput("country5", "Country:", 
                                                     choices = c("Germany" = "proof_germany", "Spain" = "proof_spain", "France" = "proof_france", "Great Britain" = "proof_uk",
                                                                 "Hungary" = "proof_hungary", "Italy" = "proof_italy", "Netherlands" = "proof_netherlands", "Poland" = "proof_poland")),
                                         checkboxInput("sameAxis5", "Standardise axis (from zero to one)", value = FALSE),
                                         downloadButton("downloadPlot5", "Download Plot"),
                                         tags$p(HTML("<br><small>* word2vec implementations only</small>"))
                                       ),
                                       mainPanel(plotOutput("proofConcept", height = "768px", width = "1024px"),
                                                 tags$p(HTML("<small><em>Note.</em> The original language corpora were processed with specific Spanish, French, Italian, Dutch, and Polish tokenizers. Hungarian was processed using a generic multilingual tokenizer. The monolingual models' recodification list is 109 multiword collocations, while the BWE consisted of 222 changes since they incorporated some typos corrections. For the original languages, that was not possible since the list of recodifications was translated with OPUS-MT transformers for each language, and typo correction could have affected the translation. Therefore, the machine-translated BWE for Great Britain is not only equivalent to the English-aligned embeddings but also to the monolingual one but with a different number of multiword recodifications during pre-processing.</small>")))
                                     ))
                          ),
                          
                          tabPanel("About",
                                   fluidPage(
                                     h2("Unravelling the Rule of Law: A Machine Learning Approach"),
                                     p("There are ongoing debates about what the rule of law is and was historically. Legal-philosophical scholars present strikingly different conclusions regarding, among others, the origins and causes of the rule of law, the evolution of its meaning and the most influential thinkers contributing to concept building. One such example of the differences of opinion concerns the thin and thick conceptualisations of the rule of law. In other words, to what extent should the rule of law exclusively refer to the quality of the legal system (thin conceptualisation) and to what extent should it also include human rights and other adjacent concepts (thick conceptualisation)? Relatedly, there is an ongoing and tense discussion revolving around the virtues of the rule of law, ranging from offering predictability to tempering arbitrary power."),
                                     h3("Different approach"),
                                     p("While acknowledging prominent legal-philosophical debates, this project proposes a radically different approach to provide insights into the concept of the rule of law. It employs cutting-edge automated text analysis methods from computer science and information retrieval fields to trace the changing meaning of the rule of law and the prevalence of the associated ideals (virtues) of the rule of law. Moreover, the project provides a tool for detecting and understanding the meaning of the rule of law by different audiences (cross-national differences) and evaluating the importance of various constitutive parts of the rule of law."),
                                     p("In a nutshell, this project applies novel machine learning techniques to:"),
                                     p(HTML("<ul><li>Demonstrate how the meaning of the rule of law has changed over time;</ul></li>")),
                                     p(HTML("<ul><li>Whether the understanding of the rule of law concept differs across jurisdictions;</ul></li>")),
                                     p(HTML("<ul><li>Provide a novel way for creating the rule of law indicators.</ul></li>"))
                                     )),
                          ),
                
                tags$footer(tags$p(HTML("<br><br>")), tags$img(src = "fgga.png", width = "150px", alt = "Leiden"), HTML("<br><br>"),
                            tags$i(class = "fa-brands fa-github"), tags$a(href = "https://github.com/RoL-project/unravelling-rule-of-law", "GitHub repository"),
                            tags$p(tags$i(class = "fa-brands fa-creative-commons"), "2024 Jaroslaw Kantorowicz and Bastián González-Bustamante"))
                )

## Define server logic to draw the plots
server <- function(input, output) {
  
  ## Load and process data
  data <- read.csv("data/tidy/summary_UK_US.csv", sep = ",", encoding = "UTF-8")
  parlamint <- read.csv("data/tidy/summary_estimates_ParlaMint.csv", sep = ",", encoding = "UTF-8")
  proof_of_concept <- read.csv("data/tidy/summary_proof_of_concept.csv", sep = ",", encoding = "UTF-8")
  bootstrap <- 30
  data$model <- ifelse(data$model == "ct", "Chronologically Trained Model",
                       ifelse(data$model == "naive", "Naive Time Model",
                              ifelse(data$model == "overlapping", "Overlapping Model", 
                                     ifelse(data$model == "ct-alt", "Alt. Chronologically Trained", 
                                            ifelse(data$model == "distilBERT", "DistilBERT Embeddings", data$model)))))
  
  ## 95% CIs
  data$se <- data$std / sqrt(bootstrap)
  data$lower_ci <- data$cosine - 1.96 * data$se
  data$upper_ci <- data$cosine + 1.96 * data$se
  parlamint$se <- parlamint$std / sqrt(bootstrap)
  parlamint$lower_ci <- parlamint$cosine - 1.96 * parlamint$se
  parlamint$upper_ci <- parlamint$cosine + 1.96 * parlamint$se
  proof_of_concept$se <- proof_of_concept$std / sqrt(bootstrap)
  proof_of_concept$lower_ci <- proof_of_concept$cosine - 1.96 * proof_of_concept$se
  proof_of_concept$upper_ci <- proof_of_concept$cosine + 1.96 * proof_of_concept$se
  
  ## Create subsets of UK-US data
  ct <- filter(data, model == "Chronologically Trained Model")
  alt_ct <- filter(data, model == "Alt. Chronologically Trained")
  distilBERT <- filter(data, model == "DistilBERT Embeddings")
  
  ## Create subsets of UK-US data
  judiciary <- filter(data, cluster == "Procedural - Judiciary")
  rules <- filter(data, cluster == "Procedural - Rules")
  democracy <- filter(data, cluster == "Substantive - Democracy")
  rights <- filter(data, cluster == "Substantive - Rights")
  
  ## Create subsets of Parlamint data
  germany <- filter(parlamint, country == "Germany")
  spain <- filter(parlamint, country == "Spain")
  france <- filter(parlamint, country == "France")
  uk <- filter(parlamint, country == "Great Britain")
  hungary <- filter(parlamint, country == "Hungary")
  italy <- filter(parlamint, country == "Italy")
  netherlands <- filter(parlamint, country == "Netherlands")
  poland <- filter(parlamint, country == "Poland")
  
  ## Create subsets of Parlamint data
  judiciary_parlamint <- filter(parlamint, cluster == "Procedural - Judiciary")
  rules_parlamint <- filter(parlamint, cluster == "Procedural - Rules")
  democracy_parlamint <- filter(parlamint, cluster == "Substantive - Democracy")
  rights_parlamint <- filter(parlamint, cluster == "Substantive - Rights")
  
  ## Create subsets of proof of concept
  proof_germany <- filter(proof_of_concept, country == "Germany")
  proof_spain <- filter(proof_of_concept, country == "Spain")
  proof_france <- filter(proof_of_concept, country == "France")
  proof_uk <- filter(proof_of_concept, country == "Great Britain")
  proof_hungary <- filter(proof_of_concept, country == "Hungary")
  proof_italy <- filter(proof_of_concept, country == "Italy")
  proof_netherlands <- filter(proof_of_concept, country == "Netherlands")
  proof_poland <- filter(proof_of_concept, country == "Poland")
  
  ## Diachronic Cluster 
  ## Render based on user input
  output$diachronicClusters <- renderPlot({
    selected_data <- get(input$model1)
    
    filtered_data <- selected_data
    
    p1 <- ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), data = subset(filtered_data, country == input$select_country1)) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, position = position_dodge(0.3)) +
      geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
      geom_line(aes(color = cluster), position = position_dodge(0.3), size = 0.5, linetype = "dotted") +
      scale_color_manual(name = "Cluster", values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
      labs(x = "Generation", y = "Cosine", title = paste("Rule of Law in the", input$select_country1), subtitle = unique(filtered_data$model), caption = NULL)
    
    ## Same Axis
    if (input$sameAxis1) {
      p1 <- p1 + scale_y_continuous(lim = c(0, 1))
    }
    
    p1
    
  }, res = 120)
  
  ## Define the download handler
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste0("plot-", input$model1, "-", input$select_country1, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  ## Models Comparison
  ## Render plot based on user input
  output$diachronicModels <- renderPlot({
    selected_models <- get(input$cluster2)
    
    filtered_models <- selected_models
    
    p2 <- ggplot(aes(x = period, y = cosine, color = model, group = model), data = subset(filtered_models, country == input$select_country2)) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, position = position_dodge(0.3)) +
      geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
      geom_line(aes(color = model), position = position_dodge(0.3), size = 0.5, linetype = "dotted") +
      scale_color_manual(name = "Model", values = c("#8da0cb", "#fc8d62", "gray40")) +
      theme_minimal(base_size = 12) + theme(legend.position = "right") +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
      labs(x = "Generation", y = "Cosine", title = paste("Rule of Law in the", input$select_country2), subtitle = unique(filtered_models$cluster), caption = NULL)
    
    ## Same Axis
    if (input$sameAxis2) {
      p2 <- p2 + scale_y_continuous(lim = c(0, 1))
    }
    
    p2
    
  }, res = 120)
  
  ## Define the download handler
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste0("plot-", input$cluster2, "-", input$select_country2, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  ## Country Clusters
  ## Render plot based on user input
  output$parlamintClusters <- renderPlot({
    selected_parlamint_data <- get(input$country3)
    
    filtered_parlamint_data <- selected_parlamint_data
    
    p3 <- ggplot(aes(x = cosine, y= factor(cluster, levels = rev(levels(factor(cluster)))),
                     color = model, group = model), data = filtered_parlamint_data) +
      geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = .2, position = position_dodge(0.3)) +
      geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
      scale_color_manual(name = "Model", values = c("#8da0cb", "#fc8d62", "gray40")) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      labs(y = NULL, x = "Cosine", title = paste("Rule of Law in", unique(filtered_parlamint_data$country)), subtitle = NULL, caption = NULL)
    
    ## Same Axis
    if (input$sameAxis3) {
      p3 <- p3 + scale_x_continuous(lim = c(0, 1))
    }
    
    p3
    
  }, res = 120)
  
  ## Define the download handler
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste0("plot-", input$country3, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  ## Cross-National Models
  ## Renderplot based on user input
  output$multilingualPlot <- renderPlot({
    selected_parlamint <- get(input$cluster4)
    
    filtered_parlamint <- selected_parlamint
    cap_size <- 0.4
    
    ## Conditionally show all countries
    if (input$showCountries) {
      filtered_parlamint <- filter(filtered_parlamint, country == "Great Britain" | country == "France" | country == "Spain" | country == "Netherlands" | country == "Poland"
                                   | country == "Hungary" | country == "Italy" | country == "Germany")
      cap_size <- 0.3
    }
    
    p4 <- ggplot(aes(x = cosine, y = reorder(country, baseline), color = model, group = model), data = filtered_parlamint) +
      geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = cap_size, position = position_dodge(0.8)) +
      geom_point(shape = 18, size = 2, position = position_dodge(0.8)) + 
      scale_color_manual(name = "Model", values = c("#8da0cb", "#fc8d62", "gray40")) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      labs(y = NULL, x = "Cosine", title = paste("Rule of Law and", unique(filtered_parlamint$cluster)), subtitle = NULL, caption = NULL)
    
    ## Same Axis
    if (input$sameAxis4) {
      p4 <- p4 + scale_x_continuous(lim = c(0, 1))
    }
    
    p4
    
  }, res = 120)
  
  ## Define the download handler
  output$downloadPlot4 <- downloadHandler(
    filename = function() {
      paste0("plot-", input$cluster4, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  ## Proof of Concept
  ## Render plot based on user input
  output$proofConcept <- renderPlot({
    selected_proof_data <- get(input$country5)
    
    filtered_proof_data <- selected_proof_data
    
    p5 <- ggplot(aes(x = cosine, y= factor(concept, levels = rev(levels(factor(concept)))),
                     color = model, group = model), data = filtered_proof_data) +
      geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = .2, position = position_dodge(0.3)) +
      geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
      scale_color_manual(name = "Model", values = c("#8da0cb", "#fc8d62", "gray40")) +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      labs(y = NULL, x = "Cosine", title = paste("Rule of Law in", unique(filtered_proof_data$country)), subtitle = "Proof of Concept", caption = NULL)
    
    ## Same Axis
    if (input$sameAxis5) {
      p5 <- p5 + scale_x_continuous(lim = c(0, 1))
    }
    
    p5
    
  }, res = 120)
  
  ## Define the download handler
  output$downloadPlot5 <- downloadHandler(
    filename = function() {
      paste0("plot-", input$country5, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
}

## Run the application 
shinyApp(ui = ui, server = server)
