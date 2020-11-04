Body <- function() {
  bs4DashBody(
    useShinyjs(),
    br(),
    # First Panel
    uiOutput("setting_tabcard"),
    # Second Panel
    bs4TabItems(
      bs4TabItem(
        tabName = "pca",
        bs4TabCard(
          id = "pca_tabcard",
          side = "left",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          bs4TabPanel(
            tabName = "Inspect Original Data",
            p("Here is the raw data from the CSV file"),
            shiny::div(DT::dataTableOutput("contents"))
          ),
          bs4TabPanel(
            tabName = "Summary Original Data",
            p("Here is the summary data from the CSV file"),
            bs4Card(
              title = "Verbatim",
              verbatimTextOutput("summary_verbatim"),
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              width = 12
            ),
            shiny::div(DT::dataTableOutput("summary_plot"))
          ),
          bs4TabPanel(
            tabName = "Correlation Plot of Original Data",
            uiOutput("choose_columns_biplot"),
            p(
              "This plot may need a few times to present when analyzing large datasets. You may need to exclude extremely correlated variables from the PCA."
            ),
            p("Here is the correlation plot."),
            shiny::div(plotOutput("correlation"))
          ),
          bs4TabPanel(
            tabName = "Summary New Data From PCA",
            p("Here is the summary the new data from the PCA function"),
            shiny::div(DT::dataTableOutput("summary_pca"))
          ),
          bs4TabPanel(
            tabName = "Bar Plot of The Variances",
            p(
              "The bar plot shows the variances of each principal component, and the cumulative variance explained by each principal component (% percentage) "
            ),
            shiny::div(plotOutput("var_plot", height = "300px")),
          ),
          bs4TabPanel(
            tabName = "PC plot",
            uiOutput("select_pc_plot"),
            p("Here is the summary the new data from the PCA function"),
            shiny::div(plotOutput("pc_plot"))
          ),
          bs4TabPanel(
            tabName = "New Data From PCA",
            p(
              "You can select the percentage of variance at \"PCA setting tab\" (default is 90%)"
            ),
            p("Here is the new data from the PCA function"),
            shiny::div(DT::dataTableOutput("new_data_pca"))
          ),
          bs4TabPanel(
            tabName = "Correlation Plot of New Data From PCA ",
            p("Here is the new data from the PCA function Plot"),
            shiny::div(plotOutput("new_data_plot"))
          )
        )
      ),
      bs4TabItem(
        tabName = "eig_face",
        bs4TabCard(
          id = "eig_tabcard",
          side = "left",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          bs4TabPanel(
            tabName = "Inspect Image Data",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            fluidRow(
              column(1, offset = 0, uiOutput("previous_1")),
              column(4, ),
              column(2, textOutput("pages1")),
              column(4, ),
              column(1, offset = 0, uiOutput("next_1")),
              column(2, ""),
              column(8, imageOutput("imgInputData")),
              column(2, ""),
              column(12, ""),
              column(12, "")
            )
          ),
          bs4TabPanel(
            tabName = "The Cumulative Plot",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            p(
              "The scree plot shows the cumulative variance explained by the number of eigenfaces (% percentage)."
            ),
            shiny::div(plotOutput("cumvar_img_plot")),
          ),
          bs4TabPanel(
            tabName = "Average Face",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            fluidRow(
              column(2, ""),
              column(8, imageOutput("avg_face")),
              column(2, ""),
              column(12, ""),
              column(12, "")
            )
          ),
          bs4TabPanel(
            tabName = "Inspect Eigenfaces",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            fluidRow(
              column(1, offset = 0, uiOutput("previous_2")),
              column(4, ),
              column(2, textOutput("pages2")),
              column(4, ),
              column(1, offset = 0, uiOutput("next_2")),
              column(2, ""),
              column(8, imageOutput("imgOutputEig")),
              column(2, ""),
              column(12, ""),
              column(12, "")
            )
          ),
          bs4TabPanel(
            tabName = "Inspect Image of Training Set",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            fluidRow(
              column(1, offset = 0, uiOutput("previous_3")),
              column(4, ),
              column(2, textOutput("pages3")),
              column(4, ),
              column(1, offset = 0, uiOutput("next_3")),
              column(2, ""),
              column(8, imageOutput("imgOutputTraining")),
              column(2, ""),
              column(12, ""),
              column(12, "")
            )
          ),
          bs4TabPanel(
            tabName = "Projection Coefficients in Eigen Space of Training Set",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            uiOutput("select_pic_no_train"),
            p("Here is the projection of picture numbere X into eigen space"),
            shiny::div(plotOutput("projection"))
          ),
          bs4TabPanel(
            tabName = "Result of Classification",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            p("Here is the result from classification"),
            shiny::div(DT::dataTableOutput("classify_result"))
          ),
          bs4TabPanel(
            tabName = "Recognize",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            uiOutput("select_pic_no_test"),
            p("Here is the result from recognization"),
            shiny::div(DT::dataTableOutput("recognize_result"))
          ),
          bs4TabPanel(
            tabName = "Evaluation with N-fold cross validation",
            p("It may need a few times to present when analyzing large datasets."),
            tags$hr(),
            fluidRow(column(6, uiOutput("n_fold")), column(6, uiOutput("n_repetition"))),
            tags$hr(),
            shiny::div(DT::dataTableOutput("eva"))
          )
        )
      )
    )
  )
}