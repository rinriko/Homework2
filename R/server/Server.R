#Ref :https://github.com/benmarwick/Interactive_PCA_Explorer
Server <- function(input, output, session) {
  observe({
    print(input)
    print("file_pca")
    print(input$file_pca)
    print("dataset_pca")
    print(input$dataset_pca)
    print(input$grouping_var)
  })
  
  values <- reactiveValues(check_file_pca = NULL)
  
  observeEvent(input$dataset_btn_pca, {
    values$check_file_pca = FALSE
  })
  
  observeEvent(input$file_btn_pca, {
    values$check_file_pca = TRUE
  })
  
  output$select_pc_plot <- renderUI({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      fluidRow(
        column(12, p("Select the PCs to plot")),
        column(6, uiOutput("pcs_plot_x")),
        column(6,  uiOutput("pcs_plot_y")),
        column(12, tags$hr())
      )
    }
  })
  
  dataInput <- reactive({
    if (is.null(values$check_file_pca)) {
      df <- NULL
      return(df)
    }
    else if (values$check_file_pca == FALSE) {
      if (is.null(input$dataset_pca))
        return(NULL)
      df <- switch(
        input$dataset_pca,
        "Please select" = NULL,
        "rock" = rock,
        "pressure" = pressure,
        "cars" = cars
      )
      return(df)
    } else if (values$check_file_pca == TRUE) {
      req(input$file_pca)
      tryCatch({
        df <- read.csv(
          input$file_pca$datapath,
          header = (input$header == "Yes"),
          sep = input$sep,
          quote = input$quote,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(df)
    }
    else{
      df <- NULL
      return(df)
    }
  })
  
  output$summary_verbatim <- renderPrint({
    if (is.null(dataInput())) {
      return(invisible())
    } else{
      dataset <- dataInput()
      return(summary(dataset))
    }
  })
  
  output$setting_tabcard <- renderUI({
    if(input$sidebar_menu == "pca"){
      bs4TabCard(
      id = "setting_tabcard",
      side = "left",
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      closable = FALSE,
      
      bs4TabPanel(
        tabName = "Use Dataset",
        active = TRUE,
        fluidRow(column(
          10,
          selectInput(
            inputId = "dataset_pca",
            label = "Choose a dataset:",
            choices = c("Please select", "rock", "pressure", "cars")
          )
        ),
        column(
          2,
          actionBttn(
            inputId = "dataset_btn_pca",
            label = "Process",
            style = "simple",
            color = "success"
          )
        ))
      ),
      bs4TabPanel(
        tabName = "Import File",
        active = FALSE,
        fluidRow(column(
          10,
          fileInput(
            "file_pca",
            "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        column(
          2,
          actionBttn(
            inputId = "file_btn_pca",
            label = "Process",
            style = "simple",
            color = "success"
          )
        )),
        tags$hr(),
        tags$h5("Settings"),
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = 'header',
              label = 'Header',
              choices = c(
                'Columns have headers' = 'Yes',
                'Columns do not have headers' =
                  'No'
              ),
              selected = 'Yes'
            )
          ),
          column(4, radioButtons(
            'sep',
            'Separator',
            c(
              Comma = ',',
              Semicolon = ';',
              Tab = '\t'
            ),
            ','
          )),
          column(4, radioButtons(
            'quote',
            'Quote',
            c(
              None = '',
              'Double Quote' = '"',
              'Single Quote' = "'"
            ),
            '"'
          ))
        )
      ),
      bs4TabPanel(
        tabName = "PCA Settings",
        active = FALSE,
        p("Select options for the PCA computation (we are using the pca manual function here)"),
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = 'center',
              label = 'Center',
              choices = c(
                'Shift variables to be zero centered' = TRUE,
                'Do not shift variables' = FALSE
              ),
              selected = TRUE
            ),
            radioButtons(
              'scale.',
              'Scale',
              choices = c(
                'Scale variables to have unit variance' = TRUE,
                'Do not scale variables' = FALSE
              ),
              selected = FALSE
            )
          ),
          column(
            4,
            radioButtons(
              'showall',
              'Select features to obtains new data',
              choices = c(
                'Show all featurees' = TRUE,
                'Select the minimum percent of data to obtain the number of features' = FALSE
              ),
              selected = FALSE
            ),
            uiOutput("select_threshold_percent")
          ),
          column(4, uiOutput("grouping_var"))
        )
      )
    )
    }
  })

  # display a summary of the CSV contents
  output$summary_plot <-  renderDataTable({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      dataset <- dataInput()
      print_data <-
        format(
          psych::describe(dataset),
          nsmall = 2L,
          digits = 3L,
          scientific = FALSE
        )
      return(t(print_data))
    }
  }, style = "bootstrap4")
  
  output$contents <- renderDataTable({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      return(dataInput())
    }
  }, style = "bootstrap4")
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  # Check boxes to choose columns
  output$choose_columns_biplot <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      data <- dataInput()
      colnames <- names(data)
      fluidRow(# Create the checkboxes and select them all by default
        column(
          12,
          checkboxGroupInput(
            "columns_biplot",
            "Choose up to five columns to display on the scatterplot matrix",
            choices  = colnames,
            selected = colnames[1:5]
          )
        ), column(12, tags$hr()))
      
    }
  }) 
  output$select_threshold_percent <- renderUI({
    if (input$showall == FALSE) {
      sliderInput(
        "threshold_percent",
        "Minimum Percentage:",
        min = 1,
        max = 100,
        value = 90
      )
      
    }
  })
  # corr plot
  output$correlation <- renderPlot({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      data <- dataInput()
      # Keep the selected columns
      columns_biplot <-    input$columns_biplot
      print(columns_biplot)
      print(colnames(data))
      if (is.null(input$columns_biplot)) {
        return(NULL)
      }
      else{
        data_subset_biplot <- data[, input$columns_biplot, drop = FALSE]
        return(ggpairs(data_subset_biplot))
      }
    }
    
  })
  
  pcaObj <- reactive({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      dataset <- dataInput()
      pca_result <-
        pca(
          dataset,
          center = as.logical(input$center),
          scale. = as.logical(input$scale.),
          threshold_percent = input$threshold_percent,
          showall = as.logical(input$showall)
        )
      print(pca_result)
      return(pca_result)
    }
  })
  
  pcaObjforPlot <- reactive({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      dataset <- dataInput()
      pca_result <-
        pca(
          dataset,
          center = as.logical(input$center),
          scale. = as.logical(input$scale.),
          threshold_percent = input$threshold_percent,
          showall = TRUE
        )
      return(pca_result)
    }
  })
  
  output$new_data_pca <- renderDataTable({
    if (is.null(pcaObj())) {
      return(NULL)
    } else{
      new_data <- pcaObj()
      return(new_data$finalData)
    }
  }, style = "bootstrap4")
  
  output$new_data_plot <- renderPlot({
    if (is.null(pcaObj())) {
      return(NULL)
    } else{
      pcaObject <- pcaObj()
      data <- pcaObject$finalData
      print(data)
      columns_biplot <- paste0("PC", seq_len(ncol(data)))
      new_data_subset_biplot <- data[, columns_biplot, drop = FALSE]
      ggpairs(new_data_subset_biplot)
    }
  })
  
  output$summary_pca <- renderDataTable({
    if (is.null(pcaObj())) {
      return(NULL)
    } else{
      pcaObject <- pcaObj()
      return(format(
        pcaObject$summary,
        nsmall = 2L,
        digits = 3L,
        scientific = FALSE
      ))
    }
  }, style = "bootstrap4")
  
  output$grouping_var <- renderUI({
    if (is.null(dataInput())) {
      p("Available when import data or select dataset.")
    }
    else{
      dataset <- dataInput()
      # for grouping we want to see only cols where the number of unique values are less than 10% the number of observations
      grouping_cols <-
        sapply(seq(1, ncol(dataset)), function(i)
          length(unique(dataset[, i])) < nrow(dataset) / 10)
      
      dataset_group_cols <- dataset[, grouping_cols, drop = FALSE]
      p("Select the grouping variable.")
      p(
        "Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."
      )
      # selectInput(
      #   inputId = "grouping_var",
      #   label = "Grouping variable:",
      #   choices = c("None", names(dataset_group_cols))
      # )

      selectInput(
        inputId = "grouping_var",
        label = "Grouping variable:",
        choices = c("None", names(dataset))
      )
    }
  })
  
  output$pcs_plot_x <- renderUI({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      pcaObject <- pcaObjforPlot()
      data <- pcaObject$finalData
      selectInput(
        inputId = "pcs_plot_x",
        label = "X axis:",
        choices = colnames(data),
        selected = 'PC1'
      )
    }
  })
  
  output$pcs_plot_y <- renderUI({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      pcaObject <- pcaObjforPlot()
      data <- pcaObject$finalData
      d <- colnames(data)
      c <- d[-match(input$pcs_plot_x, colnames(data))]
      # drop down selection
      selectInput(
        inputId = "pcs_plot_y",
        label = "Y axis:",
        choices = c,
        selected = 'PC2'
      )
    }
    
  })
  
  output$var_plot <- renderPlot({
    if (is.null(dataInput())) {
      return(NULL)
    }
    else{
      dataset <- dataInput()
      if (is.null(pcaObjforPlot())) {
        return(NULL)
      } else{
        pcaObject <- pcaObjforPlot()
        cumvar <- paste(round(pcaObject$summary.percent, 1), "%")
        
        eig_df <- data.frame(cumvar)
        PCs <- colnames(pcaObject$finalData)
        
        print(PCs)
        ggplot(eig_df, aes(
          reorder(PCs, -pcaObject$eigenvalues),
          pcaObject$eigenvalues
        )) +
          geom_bar(stat = "identity",
                   fill = "white",
                   colour = "black") +
          geom_text(label = cumvar,
                    size = 4,
                    vjust = -0.4) +
          theme_bw(base_size = 14) +
          xlab("Principal Components") +
          ylab("Variances") +
          ylim(0, (max(pcaObject$eigenvalues) * 1.1))
      }
      
    }
  })
  
  output$pc_plot <- renderPlot({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      pcaObject <- pcaObjforPlot()
      if (is.null(input$pcs_plot_x) ||
          is.null(input$pcs_plot_y)) {
        return(NULL)
      } else{
        x <- match(input$pcs_plot_x,
                   colnames(pcaObject$finalData))
        x <- as.numeric(x)
        y <-
          match(input$pcs_plot_y,
                colnames(pcaObject$finalData))
        y <- as.numeric(y)
        if (is.null(input$grouping_var)) {
          # ggbiplot_pca(
          #   pcaObject,
          #   obs.scale = 1,
          #   var.scale = 1,
          #   labels = rownames(pcaObject),
          #   choices = c(x, y),
          #   ellipse = TRUE,
          #   circle = TRUE
          # ) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
          #                                             legend.position = 'top')
          if (is.null(dataInput())) {
            return(NULL)
          } else{
            dataset <- dataInput()
            data <- na.omit(dataset)
            autoplot_pca(pcaObject, data = data, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=x,y=y)
          }
        }
        else if (input$grouping_var == "None") {
          # ggbiplot_pca(
          #   pcaObject,
          #   obs.scale = 1,
          #   var.scale = 1,
          #   labels = rownames(pcaObject),
          #   choices = c(x, y),
          #   ellipse = TRUE,
          #   circle = TRUE
          # ) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
          #                                             legend.position = 'top')
          if (is.null(dataInput())) {
            return(NULL)
          } else{
            dataset <- dataInput()
            data <- na.omit(dataset)
            autoplot_pca(pcaObject, data = data, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=x,y=y)
          }
        }
        else{
          if (is.null(dataInput())) {
            return(NULL)
          } else{
            dataset <- dataInput()
            data <- na.omit(dataset)
            grouping <- data[input$grouping_var]
            colnames(grouping) <- c("lists")

            autoplot_pca(pcaObject, data = data, colour = input$grouping_var,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, x=x,y=y)
            # ggbiplot_pca(
            #   pcaObject,
            #   obs.scale = 1,
            #   var.scale = 1,
            #   labels = rownames(pcaObject),
            #   choices = c(x, y),
            #   groups = grouping$lists,
            #   ellipse = TRUE,
            #   circle = TRUE
            # ) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
            #                                             legend.position = 'top')
          }
        }
      }
    }
  })
}