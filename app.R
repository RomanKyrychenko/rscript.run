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
      actionButton("show", "Show modal dialog"),
      radioButtons("filetype", "File type:", choices = c("csv", "tsv")),
      downloadButton('downloadData', 'Download')),
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
      observe({if(input$country=="Природний рух населення"){
        hide("country")
        js$pageCol()
      }})
      observeEvent(input$show, {
        showModal(modalDialog(
          title = "Important message",
          "This is an important message!", easyClose = TRUE, footer = NULL
        ))
      })
      vals <- reactiveValues(data = NULL)
      
      # Return the UI for a modal dialog with data selection input. If 'failed' is
      # TRUE, then display a message that the previous value was invalid.
      dataModal <- function(failed = FALSE) {
        modalDialog(
          textInput("dataset", "Choose data set",
                    placeholder = 'Try "mtcars" or "abc"'
          ),
          span('(Try the name of a valid data object like "mtcars", ',
               'then a name of a non-existent object like "abc")'),
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
        if (!is.null(input$dataset) && nzchar(input$dataset) &&
            exists(input$dataset) && is.data.frame(get(input$dataset))) {
          vals$data <- get(input$dataset)
          removeModal()
        } else {
          showModal(dataModal(failed = TRUE))
        }
      })
        output$distPlot <- renderPlot({
          ggplot(peoples,aes(Область,постійне)) + geom_bar(stat = "identity") + coord_flip() +
            theme_minimal() + theme(
              text=element_text(family="PT Sans"),
              legend.position = "bottom",
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(linetype = "dashed",size = .05, color="grey"),
              axis.title.y = element_text(family="PT Sans",size = 16),
              axis.text = element_text(family="PT Sans",size = 14)
            )
        })
        output$Plot <- renderPlot({
          peoples <- readxl::read_excel("kn0617_u.xls")[c(5:29),c(1,2,4)] 
          names(peoples) <- c("Область","постійне","наявне")
          peoples$постійне <- as.numeric(peoples$постійне)
          peoples$наявне <- as.numeric(peoples$наявне)
          plot(peoples$постійне,peoples$наявне)
        })
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste(peoples, input$filetype, sep = ".")
          },
          content = function(file) {
            sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
            write.table(peoples, file, sep = sep, row.names = FALSE)
          }
        )
      })
}
