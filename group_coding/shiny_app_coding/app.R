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

final_svm_model <- readRDS("F:/usyd_file/Science/2025/s1/Data3888/group work/w12/shiny and lasso/final_lasso_model.rds")
X_with_gender_age <- readRDS("F:/usyd_file/Science/2025/s1/Data3888/group work/w12/shiny and lasso/X_with_gender_age.rds")
selected_genes <- colnames(X_with_gender_age)
gene_only <- setdiff(selected_genes, c("gender", "age"))

# Map probe IDs to gene symbols
probe2gene <- AnnotationDbi::select(
  hgu133plus2.db,
  keys = gene_only,
  columns = c("SYMBOL"),
  keytype = "PROBEID"
)

gene_label_map <- setNames(
  ifelse(is.na(probe2gene$SYMBOL), probe2gene$PROBEID, probe2gene$SYMBOL),
  probe2gene$PROBEID
)

# Reactive values
pred_history <- reactiveValues(data = data.frame())
uploaded_data <- reactiveVal(NULL)
batch_results <- reactiveVal(NULL)

# UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  titlePanel(div(icon("dna"), "Alopecia Areata Smart Risk Calculator")),
  
  tabsetPanel(id = "tabs",
              tabPanel("Step 1: Demographic Info",
                       br(),
                       wellPanel(
                         h4("Please enter basic information"),
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
                       h3("GSE68801 Dataset Overview"),
                       p("This dataset includes gene expression profiles from scalp skin punch biopsies of patients with Alopecia Areata (AA) and healthy controls. It was submitted to the NCBI GEO database by Ali Jabbari et al. to investigate the molecular mechanisms underlying AA."),
                       
                       h4("General Information"),
                       tags$ul(
                         tags$li("Platform: Affymetrix Human Genome U133 Plus 2.0 Array (GPL570)"),
                         tags$li("Organism: Homo sapiens (human)"),
                         tags$li("Total Samples: 122"),
                         tags$li("Sample Type: Scalp skin punch biopsies"),
                         tags$li("Data Source: NCBI GEO Database"),
                         tags$li("Submitter: Ali Jabbari and colleagues"),
                         tags$li("Related Publication: Molecular signatures define alopecia areata subtypes and transcriptional biomarkers")
                       ),
                       
                       h4("Sample Grouping"),
                       tags$table(
                         tags$tr(
                           tags$th("Group"),
                           tags$th("Sample Count"),
                           tags$th("Description")
                         ),
                         tags$tr(
                           tags$td("Normal Controls (NC)"),
                           tags$td("36"),
                           tags$td("Scalp skin biopsies from healthy individuals")
                         ),
                         tags$tr(
                           tags$td("Patchy AA (AAP)"),
                           tags$td("54"),
                           tags$td("Includes 28 lesional and 26 non-lesional samples")
                         ),
                         tags$tr(
                           tags$td("Alopecia Totalis (AT)"),
                           tags$td("9"),
                           tags$td("Lesional samples from patients with complete scalp hair loss")
                         ),
                         tags$tr(
                           tags$td("Alopecia Universalis (AU)"),
                           tags$td("23"),
                           tags$td("Lesional samples from patients with complete body hair loss")
                         )
                       ),
                       
                       h4("Data Processing"),
                       tags$ul(
                         tags$li("Data Type: Gene expression profiling"),
                         tags$li("Processed using: Affymetrix platform"),
                         tags$li("Normalization: GCRMA and MAS5 versions available"),
                         tags$li("Raw CEL files also provided")
                       ),
                       
                       h4("Downloads"),
                       tags$ul(
                         tags$li(a(href = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE68nnn/GSE68801/matrix/GSE68801_GCRMA.txt.gz", "Download GCRMA normalized data")),
                         tags$li(a(href = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE68nnn/GSE68801/matrix/GSE68801_MAS5.txt.gz", "Download MAS5 normalized data")),
                         tags$li(a(href = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE68nnn/GSE68801/suppl/GSE68801_RAW.tar", "Download raw CEL files"))
                       ),
                       
                       h4("Dataset Link"),
                       tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE68801", "View GSE68801 on NCBI GEO", target = "_blank")
              ),
              
              tabPanel("About Alopecia",
                       h4("What is Alopecia Areata?"),
                       p("Alopecia Areata is an autoimmune condition in which the immune system mistakenly attacks the hair follicles, leading to hair loss on the scalp and elsewhere on the body. It can affect people of all ages, genders, and ethnic backgrounds."),
                       br(),
                       h4("Key Characteristics"),
                       tags$ul(
                         tags$li("Sudden onset: Hair loss can occur rapidly, often in round or oval patches."),
                         tags$li("Unpredictable: Hair may regrow and then fall out again, with the course of the condition varying greatly between individuals."),
                         tags$li("Non-contagious: It is not caused by infections and is not passed from person to person.")
                       ),
                       br(),
                       h4("Types of Alopecia Areata"),
                       tags$ul(
                         tags$li(strong("Patchy Alopecia Areata:"), " The most common form, with isolated patches of hair loss."),
                         tags$li(strong("Alopecia Totalis:"), " Complete loss of scalp hair."),
                         tags$li(strong("Alopecia Universalis:"), " Complete loss of hair on the scalp and body, including eyebrows and eyelashes.")
                       ),
                       br(),
                       h4("Causes and Triggers"),
                       p("While the exact cause is not fully understood, genetic and environmental factors are believed to contribute. Common triggers include:"),
                       tags$ul(
                         tags$li("Physical or emotional stress"),
                         tags$li("Illness or infections"),
                         tags$li("Hormonal changes")
                       ),
                       br(),
                       h4("Impact on Life"),
                       p("Alopecia Areata is not life-threatening, but it can have significant emotional and psychological effects. Many individuals experience challenges with self-esteem, confidence, and social interaction. Support from family, friends, and communities like AAAF is vital."),
                       br(),
                       h4("Diagnosis and Management"),
                       p("Diagnosis is usually clinical and may involve examining the pattern of hair loss and sometimes blood tests. While there is no permanent cure, treatments such as corticosteroid injections, topical therapies, and emerging immunotherapies may help with hair regrowth."),
                       br(),
                       tags$p("Learn more from the ", 
                              tags$a(href = "https://aaaf.org.au/about-alopecia-areata/", 
                                     "Alopecia Areata Fact Sheet on AAAF", target = "_blank"))
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
    
    pred_class <- predict(final_svm_model, newdata = new_data, type = "raw")
    pred_prob <- predict(final_svm_model, newdata = new_data, type = "prob")[, "Patient"]
    
    
    
    output$risk_level <- renderUI({
      if (pred_class == "Patient") {
        tags$h4("\U0001F534 YES", style = "color:red;")
      } else {
        tags$h4("\U00002705 NO", style = "color:green;")
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
    pred_class <- predict(final_svm_model, newdata = df, type = "raw")
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