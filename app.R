library(shiny)
library(xgboost)
library(ggplot2)
library(dplyr)
library(DT)

data_dir <- "shiny_data"

model_xgb <- xgb.load(file.path(data_dir, "xgb_model.json"))
df <- read.csv(file.path(data_dir, "dataset.csv"), check.names = FALSE)
importance_df <- read.csv(file.path(data_dir, "importance.csv"), check.names = FALSE)

feature_medians_df <- read.csv(
  file.path(data_dir, "feature_medians.csv"),
  row.names = 1,
  check.names = FALSE
)
feature_medians <- feature_medians_df[[1]]
names(feature_medians) <- rownames(feature_medians_df)

feature_names <- read.csv(
  file.path(data_dir, "feature_names.csv"),
  check.names = FALSE
)$feature_name

model_info <- read.csv(file.path(data_dir, "model_info.csv"), check.names = FALSE)

target_name <- "target"
selected_features <- head(importance_df$feature, 8)
class_counts <- table(df[[target_name]])

dataset_preview <- head(df, 15)

feature_description_map <- c(
  "mean radius" = "Priemerný polomer bunkového jadra",
  "mean texture" = "Priemerná textúra obrazu",
  "mean perimeter" = "Priemerný obvod jadra",
  "mean area" = "Priemerná plocha jadra",
  "mean smoothness" = "Priemerná hladkosť okraja",
  "mean compactness" = "Priemerná kompaktnosť",
  "mean concavity" = "Priemerná konkávnosť",
  "mean concave points" = "Priemerný počet konkávnych bodov",
  "mean symmetry" = "Priemerná symetria",
  "mean fractal dimension" = "Priemerný fraktálny rozmer",
  "radius error" = "Odchýlka polomeru",
  "texture error" = "Odchýlka textúry",
  "perimeter error" = "Odchýlka obvodu",
  "area error" = "Odchýlka plochy",
  "smoothness error" = "Odchýlka hladkosti",
  "compactness error" = "Odchýlka kompaktnosti",
  "concavity error" = "Odchýlka konkávnosti",
  "concave points error" = "Odchýlka konkávnych bodov",
  "symmetry error" = "Odchýlka symetrie",
  "fractal dimension error" = "Odchýlka fraktálneho rozmeru",
  "worst radius" = "Najvyšší zaznamenaný polomer",
  "worst texture" = "Najvyššia zaznamenaná textúra",
  "worst perimeter" = "Najvyšší zaznamenaný obvod",
  "worst area" = "Najvyššia zaznamenaná plocha",
  "worst smoothness" = "Najvyššia zaznamenaná hladkosť",
  "worst compactness" = "Najvyššia zaznamenaná kompaktnosť",
  "worst concavity" = "Najvyššia zaznamenaná konkávnosť",
  "worst concave points" = "Najvyšší počet konkávnych bodov",
  "worst symmetry" = "Najvyššia zaznamenaná symetria",
  "worst fractal dimension" = "Najvyšší fraktálny rozmer"
)

all_features_df <- data.frame(
  Atribút = feature_names,
  Popis = unname(feature_description_map[feature_names]),
  stringsAsFactors = FALSE
)

summary_df <- data.frame(
  Atribút = feature_names,
  Priemer = sapply(df[, feature_names, drop = FALSE], mean),
  Medián = sapply(df[, feature_names, drop = FALSE], median),
  Minimum = sapply(df[, feature_names, drop = FALSE], min),
  Maximum = sapply(df[, feature_names, drop = FALSE], max),
  check.names = FALSE
)

summary_df[, -1] <- round(summary_df[, -1], 4)

get_metric_value <- function(metric_name) {
  value <- model_info$value[model_info$metric == metric_name]
  if (length(value) == 0) return(NA)
  as.numeric(value[1])
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f6f8f7;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #1f2937;
      }

      .container-fluid {
        max-width: 1380px;
      }

      .app-title {
        background: linear-gradient(135deg, #5f8f7b 0%, #7aa88f 100%);
        color: white;
        padding: 30px 34px;
        border-radius: 24px;
        margin-top: 18px;
        margin-bottom: 24px;
        box-shadow: 0 12px 28px rgba(95, 143, 123, 0.18);
      }

      .app-title h1 {
        margin: 0;
        font-size: 32px;
        font-weight: 700;
      }

      .app-title p {
        margin-top: 10px;
        margin-bottom: 0;
        font-size: 16px;
        opacity: 0.95;
      }

      .card-box {
        background: white;
        border-radius: 20px;
        padding: 22px 24px;
        box-shadow: 0 8px 20px rgba(15, 23, 42, 0.05);
        margin-bottom: 20px;
        border: 1px solid #e5ece8;
      }

      .section-title {
        font-size: 22px;
        font-weight: 700;
        color: #1f2937;
        margin-bottom: 12px;
      }

      .subtle-text {
        color: #6b7280;
        font-size: 15px;
      }

      .metric-card {
        background: linear-gradient(135deg, #f8fbf9 0%, #eef5f1 100%);
        border: 1px solid #dfe9e4;
        border-radius: 18px;
        padding: 16px;
        text-align: center;
        box-shadow: 0 4px 10px rgba(15, 23, 42, 0.03);
        margin-bottom: 12px;
      }

      .metric-value {
        font-size: 24px;
        font-weight: 700;
        color: #5f8f7b;
      }

      .metric-label {
        font-size: 13px;
        color: #6b7280;
        margin-top: 6px;
      }

      .nav-tabs {
        border-bottom: none !important;
        margin-bottom: 18px;
      }

      .nav-tabs > li > a {
        border: none !important;
        border-radius: 14px !important;
        margin-right: 8px;
        color: #1f2937 !important;
        background: #edf4f1 !important;
        font-weight: 600;
        padding: 10px 18px;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: #5f8f7b !important;
        color: white !important;
      }

      .btn-primary {
        background: #5f8f7b;
        border: none;
        border-radius: 14px;
        padding: 10px 18px;
        font-weight: 600;
        box-shadow: 0 8px 16px rgba(95, 143, 123, 0.14);
      }

      .btn-primary:hover {
        background: #4f7c6a;
      }

      .form-control {
        border-radius: 10px;
        border: 1px solid #d2ddd7;
        box-shadow: none;
      }

      .control-label {
        font-weight: 600;
        color: #374151;
      }

      .prediction-box {
        border-radius: 16px;
        padding: 18px;
        font-weight: 600;
        font-size: 17px;
        margin-bottom: 16px;
        box-shadow: 0 8px 16px rgba(15, 23, 42, 0.05);
      }

      .prediction-benign {
        background: rgba(111, 170, 132, 0.22);
        color: #2d5f3f;
        border: 1px solid rgba(111, 170, 132, 0.35);
      }

      .prediction-malignant {
        background: rgba(217, 119, 119, 0.20);
        color: #8a3d3d;
        border: 1px solid rgba(217, 119, 119, 0.35);
      }

      .table th {
        background: #edf4f1;
        color: #1f2937;
      }

      .dataTables_wrapper {
        font-size: 14px;
      }

      .dataset-info-box {
        background: linear-gradient(135deg, #f8fbf9 0%, #eef5f1 100%);
        border: 1px solid #dfe9e4;
        border-radius: 18px;
        padding: 18px;
        margin-top: 14px;
      }

      .dataset-info-box h4 {
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 18px;
        color: #365748;
        font-weight: 700;
      }
    "))
  ),
  
  div(
    class = "app-title",
    h1("XGBoost – klasifikácia typu nádoru"),
    p("Interaktívna aplikácia pre prezentáciu modelu, dát a predikcie pre nového pacienta.")
  ),
  
  tabsetPanel(
    tabPanel(
      "Hlavná stránka",
      br(),
      fluidRow(
        column(
          7,
          div(
            class = "card-box",
            div(class = "section-title", "O aplikácii"),
            p("Táto aplikácia demonštruje fungovanie klasifikačného modelu XGBoost."),
            p("Model bol natrénovaný v prostredí Python a následne uložený pre použitie v R Shiny."),
            tags$ul(
              tags$li("zobrazenie základných informácií o datasete"),
              tags$li("vizualizácia rozdelenia tried"),
              tags$li("zobrazenie dôležitosti príznakov"),
              tags$li("predikcia pre nového pacienta")
            ),
            div(
              class = "dataset-info-box",
              h4("Použitý dataset"),
              p("Breast Cancer Wisconsin (Diagnostic) Dataset"),
              p("Dataset obsahuje 569 vzoriek a 30 numerických príznakov opisujúcich vlastnosti bunkových jadier."),
              p("Je známy z UCI Machine Learning Repository a pochádza z University of Wisconsin Hospitals.")
            )
          )
        ),
        column(
          5,
          div(
            class = "card-box",
            div(class = "section-title", "Význam cieľovej premennej"),
            tags$ul(
              tags$li("0 – malignant (malígny nádor)"),
              tags$li("1 – benign (benígny nádor)")
            )
          ),
          fluidRow(
            column(
              6,
              div(
                class = "metric-card",
                div(class = "metric-value", get_metric_value("n_samples")),
                div(class = "metric-label", "Počet vzoriek")
              )
            ),
            column(
              6,
              div(
                class = "metric-card",
                div(class = "metric-value", get_metric_value("n_features")),
                div(class = "metric-label", "Počet príznakov")
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(
                class = "metric-card",
                div(class = "metric-value", round(get_metric_value("accuracy"), 3)),
                div(class = "metric-label", "Accuracy")
              )
            ),
            column(
              6,
              div(
                class = "metric-card",
                div(class = "metric-value", round(get_metric_value("roc_auc"), 3)),
                div(class = "metric-label", "ROC-AUC")
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Popis dát",
      br(),
      fluidRow(
        column(
          5,
          div(
            class = "card-box",
            div(class = "section-title", "Základné informácie o datasete"),
            tableOutput("dataset_info")
          )
        ),
        column(
          7,
          div(
            class = "card-box",
            div(class = "section-title", "Rozdelenie tried"),
            plotOutput("class_plot", height = "320px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(
            class = "card-box",
            div(class = "section-title", "Ukážka prvých riadkov datasetu"),
            DTOutput("dataset_preview_table")
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "card-box",
            div(class = "section-title", "Atribúty a ich stručný popis"),
            DTOutput("all_features_table")
          )
        ),
        column(
          6,
          div(
            class = "card-box",
            div(class = "section-title", "Základné štatistiky atribútov"),
            DTOutput("summary_table")
          )
        )
      )
    ),
    
    tabPanel(
      "Význam atribútov",
      br(),
      fluidRow(
        column(
          7,
          div(
            class = "card-box",
            div(class = "section-title", "Top 10 najdôležitejších príznakov"),
            plotOutput("importance_plot", height = "520px")
          )
        ),
        column(
          5,
          div(
            class = "card-box",
            div(class = "section-title", "Tabuľka dôležitosti"),
            DTOutput("importance_table")
          )
        )
      )
    ),
    
    tabPanel(
      "Predikcia",
      br(),
      fluidRow(
        column(
          4,
          div(
            class = "card-box",
            div(class = "section-title", "Vstupné atribúty"),
            p(
              class = "subtle-text",
              "Používateľ zadáva vybrané atribúty. Ostatné hodnoty budú doplnené mediánom datasetu."
            ),
            
            numericInput(selected_features[1], selected_features[1], value = unname(feature_medians[selected_features[1]])),
            numericInput(selected_features[2], selected_features[2], value = unname(feature_medians[selected_features[2]])),
            numericInput(selected_features[3], selected_features[3], value = unname(feature_medians[selected_features[3]])),
            numericInput(selected_features[4], selected_features[4], value = unname(feature_medians[selected_features[4]])),
            numericInput(selected_features[5], selected_features[5], value = unname(feature_medians[selected_features[5]])),
            numericInput(selected_features[6], selected_features[6], value = unname(feature_medians[selected_features[6]])),
            numericInput(selected_features[7], selected_features[7], value = unname(feature_medians[selected_features[7]])),
            numericInput(selected_features[8], selected_features[8], value = unname(feature_medians[selected_features[8]])),
            
            actionButton("predict_btn", "Vytvoriť predikciu", class = "btn-primary")
          )
        ),
        
        column(
          8,
          uiOutput("prediction_box"),
          div(
            class = "card-box",
            div(class = "section-title", "Pravdepodobnosti"),
            tableOutput("prediction_probs"),
            br(),
            plotOutput("prediction_plot", height = "320px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$dataset_info <- renderTable({
    data.frame(
      Ukazovateľ = c(
        "Počet vzoriek",
        "Počet príznakov",
        "Accuracy",
        "Precision",
        "Recall",
        "F1-score",
        "ROC-AUC"
      ),
      Hodnota = c(
        as.integer(get_metric_value("n_samples")),
        as.integer(get_metric_value("n_features")),
        sprintf("%.3f", get_metric_value("accuracy")),
        sprintf("%.3f", get_metric_value("precision")),
        sprintf("%.3f", get_metric_value("recall")),
        sprintf("%.3f", get_metric_value("f1_score")),
        sprintf("%.3f", get_metric_value("roc_auc"))
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m", width = "100%")
  
  output$class_plot <- renderPlot({
    plot_df <- data.frame(
      Trieda = c("malignant (0)", "benign (1)"),
      Pocet = as.numeric(class_counts)
    )
    
    ggplot(plot_df, aes(x = Trieda, y = Pocet, fill = Trieda)) +
      geom_col(width = 0.55, show.legend = FALSE, alpha = 0.8) +
      geom_text(aes(label = Pocet), vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_manual(values = c("malignant (0)" = "#d99a9a", "benign (1)" = "#7aa88f")) +
      labs(
        title = "Rozdelenie tried v datasete",
        x = "Trieda",
        y = "Počet vzoriek"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      ) +
      expand_limits(y = max(plot_df$Pocet) * 1.12)
  })
  
  output$dataset_preview_table <- renderDT({
    datatable(
      dataset_preview,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthMenu = list(c(5, 10, 15), c("5", "10", "15")),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  output$all_features_table <- renderDT({
    datatable(
      all_features_df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = "420px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '32%', targets = 0),
          list(width = '68%', targets = 1)
        )
      )
    )
  })
  
  output$summary_table <- renderDT({
    datatable(
      summary_df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = "420px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  output$importance_plot <- renderPlot({
    top_imp <- importance_df %>%
      slice_max(importance, n = 10) %>%
      arrange(importance)
    
    ggplot(top_imp, aes(x = importance, y = reorder(feature, importance))) +
      geom_col(fill = "#7aa88f") +
      labs(
        title = "Top 10 najdôležitejších príznakov",
        x = "Dôležitosť",
        y = "Príznak"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$importance_table <- renderDT({
    datatable(
      importance_df %>%
        arrange(desc(importance)) %>%
        head(10),
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = '65%', targets = 0),
          list(width = '35%', targets = 1, className = 'dt-right')
        )
      )
    )
  })
  
  prediction_result <- eventReactive(input$predict_btn, {
    new_patient <- as.data.frame(t(feature_medians), check.names = FALSE)
    colnames(new_patient) <- names(feature_medians)
    
    for (feat in selected_features) {
      new_patient[[feat]] <- input[[feat]]
    }
    
    new_patient <- new_patient[, feature_names, drop = FALSE]
    new_matrix <- as.matrix(new_patient)
    
    prob_benign <- as.numeric(predict(model_xgb, new_matrix))
    class_pred <- ifelse(prob_benign >= 0.5, 1, 0)
    
    list(
      class_pred = class_pred,
      prob_benign = prob_benign,
      prob_malignant = 1 - prob_benign
    )
  })
  
  output$prediction_box <- renderUI({
    req(prediction_result())
    
    if (prediction_result()$class_pred == 1) {
      div(
        class = "prediction-box prediction-benign",
        "Predikcia modelu: benign (1)"
      )
    } else {
      div(
        class = "prediction-box prediction-malignant",
        "Predikcia modelu: malignant (0)"
      )
    }
  })
  
  output$prediction_probs <- renderTable({
    req(prediction_result())
    
    data.frame(
      Trieda = c("malignant (0)", "benign (1)"),
      Pravdepodobnosť = round(
        c(prediction_result()$prob_malignant, prediction_result()$prob_benign),
        4
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m", width = "100%")
  
  output$prediction_plot <- renderPlot({
    req(prediction_result())
    
    probs <- data.frame(
      Trieda = c("malignant (0)", "benign (1)"),
      Pravdepodobnosť = c(
        prediction_result()$prob_malignant,
        prediction_result()$prob_benign
      )
    )
    
    ggplot(probs, aes(x = Trieda, y = Pravdepodobnosť, fill = Trieda)) +
      geom_col(width = 0.55, show.legend = FALSE, alpha = 0.82) +
      geom_text(
        aes(label = round(Pravdepodobnosť, 4)),
        vjust = -0.5,
        fontface = "bold",
        size = 5
      ) +
      scale_fill_manual(values = c("malignant (0)" = "#d99a9a", "benign (1)" = "#7aa88f")) +
      ylim(0, 1) +
      labs(
        title = "Pravdepodobnosti predikcie",
        x = "Trieda",
        y = "Pravdepodobnosť"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)