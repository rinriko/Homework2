sapply(list.files(pattern="[.]R$", path="R/ui", full.names=TRUE), source);
sapply(list.files(pattern="[.]R$", path="R/server", full.names=TRUE), source);

if(interactive()){
  library(shiny)
  library(bs4Dash)
  library(DT)
  library(shinyWidgets)
  library(shinyjs)
  library(GGally)
  library(ggplot2)
  library(ggfortify)
  # library(ggbiplot)
  
  shiny::shinyApp(
    ui = DashBoard(),
    server = Server
  )
}
