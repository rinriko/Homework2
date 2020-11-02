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
            p("This plot may need a few times to present when analyzing large datasets. You may need to exclude extremely correlated variables from the PCA."),
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
            p("The screen plot shows the variances of each principal component, and the cumulative variance explained by each principal component (% percentage) "),
            plotOutput("var_plot", height = "300px"),
          ),
          bs4TabPanel(
            tabName = "PC plot",
            uiOutput("select_pc_plot"),
            p("Here is the summary the new data from the PCA function"),
            shiny::div(plotOutput("pc_plot"))
          ),
          bs4TabPanel(
            tabName = "New Data From PCA",
            p("You can select the percentage of variance at \"PCA setting tab\" (default is 90%)"),
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
      bs4TabItem(tabName = "eig_face",
        # bs4TabCard(
        #   id = "pca_tabcard",
        #   side = "left",
        #   width = 12,
        #   collapsible = TRUE,
        #   collapsed = FALSE,
        #   closable = FALSE
        #   bs4TabPanel(
        #     tabName = "Inspect Original Data",
        #   )
        #   ) 
          )
    )
  )
}