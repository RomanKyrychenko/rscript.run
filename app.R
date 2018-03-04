suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(ggplot2)
})

source("global.R")

if (interactive()) {
  
  shinyApp(
    ui = fixedPage(
      useShinyjs(),
      extendShinyjs(text = jsCode),
      tags$head(tags$style(HTML(css_style))),
      basicPage(conditionalPanel(condition="input.country.length>1",selectInput("country", "", categories))),
      conditionalPanel(condition="input.country=='Природний рух населення'", uiOutput("part2")),
      actionButton("show", "Show modal dialog")),
    server = function(input, output, session) {
      output$part2 <- renderUI({
        navbarPage(title = "ukrdata.com",
                  tabPanel("l",
                             sidebarLayout(
                               sidebarPanel(
                                 sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100),
                                 selectInput("si",choices = "d",label = ""),
                                 actionButton("ab","s"),
                                 actionButton("ab2","s"),
                                 actionButton("ab3","s")
                               ),
                               mainPanel(plotOutput("distPlot"))
                             )
          ),
          tabPanel("l2",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("obs2", "Number of observations:", min = 10, max = 500, value = 100)
                     ),
                     mainPanel(plotOutput("Plot"))
                   )
          )
        )
        })
      observe({if(input$country == "Природний рух населення"){
        hide("country")
        js$pageCol()
      }})
      observeEvent(input$show, {
        showModal(modalDialog(
          title = "Important message", "This is an important message!", easyClose = TRUE, footer = NULL
        ))
      })
      vals <- reactiveValues(data = NULL)

      dataModal <- function(failed = FALSE) {
        modalDialog(
          selectInput("format", "Оберіть формат даних", choices = c("xlsx", "csv", "json")),
          span('(Try the name of a valid data object like "mtcars", ',
               'then a name of a non-existent object like "abc")'),
          downloadButton('downloadData', 'Download'),
          if (failed) div(tags$b("Invalid name of data object", style = "color: red;")),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        )
      }
      observeEvent(input$show, {
        showModal(dataModal())
      })
      observeEvent(input$ok, {
        #if (!is.null(input$format) && nzchar(input$format) &&
        #    exists(input$format) && is.data.frame(peoples)) {
          vals$data <- peoples
          removeModal()
        #} else {
        #  showModal(dataModal(failed = TRUE))
        #}
      })
        output$distPlot <- renderPlot({
          ggplot(peoples, aes(Область, постійне)) + geom_bar(stat = "identity") + coord_flip() +
            theme_minimal() + theme(
              text=element_text(family = "PT Sans"),
              legend.position = "bottom",
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(linetype = "dashed", size = .05, color="grey"),
              axis.title.y = element_text(family = "PT Sans", size = 16),
              axis.text = element_text(family = "PT Sans", size = 14)
            )
        })
        output$Plot <- renderPlot({
          peoples <- readxl::read_excel("kn0617_u.xls")[c(5:29),c(1,2,4)] 
          names(peoples) <- c("Область","постійне","наявне")
          peoples$постійне <- as.numeric(peoples$постійне)
          peoples$наявне <- as.numeric(peoples$наявне)
          plot(peoples$постійне,peoples$наявне)
        })
        output$dataInfo <- renderPrint({
          if (is.null(vals$data))
            "No data selected"
          else
            summary(vals$data)
        })
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste(peoples, input$format, sep = ".")
          },
          content = function(file) {
            sep <- switch(input$format, "csv" = ",", "tsv" = "\t")
            write.table(peoples, file, sep = sep, row.names = FALSE)
          }
        )
      })
}
