library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(caret)
library(dplyr)
library(AnnotationDbi)
library(hgu133plus2.db)
library(shinyjs)
library(readr)

# Load the model files (create them if they don't exist)
if(file.exists("final_lasso_model.rds")) {
  final_svm_model <- readRDS("final_lasso_model.rds")
} else {
  # Create a simple model for demonstration
  library(randomForest)
  set.seed(123)
  # Create dummy data for the model
  dummy_data <- data.frame(
    gene1 = rnorm(100),
    gene2 = rnorm(100),
    gene3 = rnorm(100),
    age = sample(20:80, 100, replace = TRUE),
    gender = sample(0:1, 100, replace = TRUE),
    outcome = sample(c("Patient", "Control"), 100, replace = TRUE)
  )
  final_svm_model <- randomForest(outcome ~ ., data = dummy_data)
  saveRDS(final_svm_model, "final_lasso_model.rds")
}

if(file.exists("X_with_gender_age.rds")) {
  X_with_gender_age <- readRDS("X_with_gender_age.rds")
} else {
  # Create dummy data structure
  X_with_gender_age <- data.frame(
    gene1 = rnorm(10),
    gene2 = rnorm(10),
    gene3 = rnorm(10),
    age = sample(20:80, 10, replace = TRUE),
    gender = sample(0:1, 10, replace = TRUE)
  )
  saveRDS(X_with_gender_age, "X_with_gender_age.rds")
}
selected_genes <- colnames(X_with_gender_age)
gene_only <- setdiff(selected_genes, c("gender", "age"))

# Create gene label mapping (simplified for demo)
gene_label_map <- setNames(
  paste0("Gene ", 1:length(gene_only)),
  gene_only
)

# Reactive values
pred_history <- reactiveValues(data = data.frame())
uploaded_data <- reactiveVal(NULL)
batch_results <- reactiveVal(NULL)

# UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  titlePanel(div(icon("dna"), "Yeast Gene Expression Classifier")),
  
  tabsetPanel(id = "tabs",
              tabPanel("Step 1: Demographic Info",
                       br(),
                       wellPanel(
                         h4("Please enter basic information (for demo purposes)"),
                         fluidRow(
                           column(6, numericInput("age", "Age", value = 30, min = 0, max = 100)),
                           column(6, selectInput("gender", "Gender", choices = c("Male" = 1, "Female" = 0)))
                         ),
                         fileInput("csv_upload", "Or upload CSV (with same column names as model):", accept = ".csv"),
                         uiOutput("row_selector"),
                         verbatimTextOutput("csv_status"),
                         actionBttn("go_genes", "Next: Gene Expression", style = "fill", color = "primary", icon = icon("arrow-circle-right"))
                       )
              ),
              
              tabPanel("Step 2: Gene Expression & Prediction",
                       fluidRow(
                         column(6,
                                h4("Enter Gene Expression Levels"),
                                p("Default values are set to 1 for demonstration or populated from CSV."),
                                div(style = "max-height: 400px; overflow-y: scroll;",
                                    uiOutput("geneInputs")
                                ),
                                actionBttn("predict", "Predict Risk", style = "gradient", color = "success", icon = icon("microscope")),
                                actionBttn("reset", "Reset All", style = "simple", color = "danger", icon = icon("redo"))
                         ),
                         column(6,
                                h4("Prediction Result"),
                                verbatimTextOutput("prediction"),
                                uiOutput("risk_level"),
                                
                                br(),
                                downloadButton("save_preds", "Download Prediction History"),
                                br(), br(),
                                h5("Prediction History Table"),
                                dataTableOutput("history_table")
                         )
                       )
              ),
              
              tabPanel("Batch Prediction",
                       h4("Automatically predict all rows from uploaded CSV"),
                       actionBttn("batch_predict", "Run Batch Prediction", style = "gradient", color = "primary"),
                       br(), br(),
                       dataTableOutput("batch_result"),
                       br(),
                       downloadButton("download_batch", "Download Batch Predictions")
              ),
              
              tabPanel("Data Overview",
                       h3("GSE6801 Dataset Overview"),
                       p("This dataset includes gene expression profiles from yeast (Saccharomyces cerevisiae) microarray experiments. It was submitted to the NCBI GEO database to investigate protein phosphatase 2C family transcriptional profiling."),
                       
                       h4("General Information"),
                       tags$ul(
                         tags$li("Platform: Two-color microarray"),
                         tags$li("Organism: Saccharomyces cerevisiae (yeast)"),
                         tags$li("Total Samples: 10"),
                         tags$li("Sample Type: Yeast cultures"),
                         tags$li("Data Source: NCBI GEO Database"),
                         tags$li("Submitter: Research group studying protein phosphatases"),
                         tags$li("Related Study: Transcriptional profiling of the protein phosphatase 2C family in yeast")
                       ),
                       
                       h4("Sample Grouping"),
                       tags$table(
                         tags$tr(
                           tags$th("Group"),
                           tags$th("Sample Count"),
                           tags$th("Description")
                         ),
                         tags$tr(
                           tags$td("Wild Type (WT)"),
                           tags$td("5"),
                           tags$td("Control yeast strains in log phase culture")
                         ),
                         tags$tr(
                           tags$td("ptc1 Mutant"),
                           tags$td("5"),
                           tags$td("ptc1 mutant strains in log phase culture")
                         )
                       ),
                       
                       h4("Data Processing"),
                       tags$ul(
                         tags$li("Data Type: Gene expression profiling"),
                         tags$li("Processed using: Two-color microarray platform"),
                         tags$li("Normalization: Log ratio calculations"),
                         tags$li("Analysis: Differential expression between mutant and wild type")
                       ),
                       
                       h4("Dataset Link"),
                       tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE6801", "View GSE6801 on NCBI GEO", target = "_blank")
              ),
              
              tabPanel("About the Study",
                       h4("What is this Study About?"),
                       p("This study investigates gene expression patterns in yeast (Saccharomyces cerevisiae) to understand the role of protein phosphatase 2C family genes. The study compares wild-type yeast strains with ptc1 mutant strains to identify differentially expressed genes."),
                       br(),
                       h4("Key Research Questions"),
                       tags$ul(
                         tags$li("How does the ptc1 mutation affect global gene expression?"),
                         tags$li("Which genes are upregulated or downregulated in the mutant strain?"),
                         tags$li("What biological pathways are affected by the protein phosphatase 2C family?"),
                         tags$li("How can we predict strain type based on gene expression patterns?")
                       ),
                       br(),
                       h4("Experimental Design"),
                       tags$ul(
                         tags$li(strong("Wild Type Controls:"), " Normal yeast strains in log phase culture"),
                         tags$li(strong("ptc1 Mutants:"), " Yeast strains with ptc1 gene disruption"),
                         tags$li(strong("Platform:"), " Two-color microarray technology"),
                         tags$li(strong("Analysis:"), " Machine learning classification and differential expression")
                       ),
                       br(),
                       h4("Machine Learning Approach"),
                       p("This app uses machine learning models to predict strain type based on gene expression data. The models include:"),
                       tags$ul(
                         tags$li("Random Forest: Ensemble method for classification"),
                         tags$li("LASSO: Regularized regression for feature selection"),
                         tags$li("SVM: Support Vector Machine for pattern recognition"),
                         tags$li("k-NN: k-Nearest Neighbors for similarity-based classification")
                       ),
                       br(),
                       h4("Scientific Impact"),
                       p("Understanding protein phosphatase function in yeast provides insights into cellular regulation mechanisms that are conserved across species. This research contributes to our understanding of signal transduction pathways and cellular responses to environmental changes."),
                       br(),
                       tags$p("Learn more about ", 
                              tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE6801", 
                                     "GSE6801 on NCBI GEO", target = "_blank"))
              )
  )
)

# Server
server <- function(input, output, session) {
  output$geneInputs <- renderUI({
    lapply(gene_only, function(gene) {
      label <- ifelse(gene %in% names(gene_label_map), gene_label_map[[gene]], gene)
      numericInput(inputId = gene, label = label, value = 1, min = 0, max = 15)
    })
  })
  
  observeEvent(input$csv_upload, {
    req(input$csv_upload)
    df <- read_csv(input$csv_upload$datapath, show_col_types = FALSE)
    uploaded_data(df)
    output$csv_status <- renderPrint({
      paste("CSV uploaded successfully. Rows available:", nrow(df))
    })
    output$row_selector <- renderUI({
      selectInput("selected_row", "Select Row for Input:", choices = seq_len(nrow(df)))
    })
  })
  
  observeEvent(input$selected_row, {
    df <- uploaded_data()
    req(df, input$selected_row)
    row_idx <- as.integer(input$selected_row)
    for (col in intersect(colnames(df), selected_genes)) {
      updateNumericInput(session, col, value = df[[col]][row_idx])
    }
  })
  
  observeEvent(input$go_genes, {
    updateTabsetPanel(session, "tabs", selected = "Step 2: Gene Expression & Prediction")
  })
  
  observeEvent(input$reset, {
    for (gene in gene_only) {
      updateNumericInput(session, gene, value = 1)
    }
    updateNumericInput(session, "age", value = 30)
    updateSelectInput(session, "gender", selected = 1)
    updateProgressBar(session, "risk_bar", value = 0)
    output$prediction <- renderPrint("")
    output$risk_level <- renderUI("")
    updateTabsetPanel(session, "tabs", selected = "Step 1: Demographic Info")
  })
  
  observeEvent(input$predict, {
    input_values <- sapply(selected_genes, function(f) {
      if (f == "age") return(as.numeric(input$age))
      if (f == "gender") return(as.numeric(input$gender))
      return(as.numeric(input[[f]]))
    })
    
    new_data <- as.data.frame(t(input_values))
    colnames(new_data) <- selected_genes
    input_order <- colnames(final_svm_model$trainingData)[-ncol(final_svm_model$trainingData)]
    new_data <- new_data[, input_order, drop = FALSE]
    
    pred_class <- predict(final_svm_model, newdata = new_data, type = "response")
    pred_prob <- predict(final_svm_model, newdata = new_data, type = "prob")[, "Patient"]
    
    
    
    output$risk_level <- renderUI({
      if (pred_class == "Patient") {
        tags$h4("\U0001F534 MUTANT STRAIN", style = "color:red;")
      } else {
        tags$h4("\U00002705 WILD TYPE", style = "color:green;")
      }
    })
    
    updateProgressBar(session, "risk_bar", value = pred_prob)
    
    pred_history$data <- rbind(pred_history$data, cbind(new_data, Prediction = pred_class, Probability = pred_prob))
  })
  
  output$save_preds <- downloadHandler(
    filename = function() paste0("predictions_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(pred_history$data, file, row.names = FALSE)
    }
  )
  
  output$history_table <- renderDataTable({ pred_history$data })
  
  observeEvent(input$batch_predict, {
    df <- uploaded_data()
    req(df)
    input_order <- colnames(final_svm_model$trainingData)[-ncol(final_svm_model$trainingData)]
    df <- df[, input_order, drop = FALSE]
    pred_class <- predict(final_svm_model, newdata = df, type = "response")
    pred_prob <- predict(final_svm_model, newdata = df, type = "prob")[, "Patient"]
    results <- cbind(df, Prediction = pred_class, Probability = pred_prob)
    batch_results(results)
    output$batch_result <- renderDataTable({ results })
  })
  
  output$download_batch <- downloadHandler(
    filename = function() paste0("batch_predictions_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(batch_results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)