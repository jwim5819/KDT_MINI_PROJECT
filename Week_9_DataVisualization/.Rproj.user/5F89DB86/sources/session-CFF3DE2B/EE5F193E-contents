library(shiny)
library(ggplot2)
library(palmerpenguins)

server <- function(input, output){
  output$mytable_1 <- renderDataTable({
    penguins[, input$showvars, drop = F]
  })
  output$mytable_2 <- renderDataTable({
    state.x77
  }, options = list(bSortClasses = T))
  output$mytable_3 <- renderDataTable({
    airquality
  }, options = list(aLengthMenu = c(5, 30, 50),
                    iDisplayLength = 10))
}

ui <- pageWithSidebar(
  headerPanel(
    h1('Datatable Example')
  ),
  sidebarPanel(
    checkboxGroupInput('showvars',
                       'choose column',
                       names(penguins),
                       selected = names(penguins)),
    helpText('choose tap')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('penguins',
               dataTableOutput('mytable_1')),
      tabPanel('state.x77',
               dataTableOutput('mytable_2')),
      tabPanel('airquality',
               dataTableOutput('mytable_3'))
  )
  )
)
  
shinyApp(ui, server)
