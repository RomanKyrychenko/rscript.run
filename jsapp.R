library(shiny)
require(digest)
require(dplyr)

#source('helpers.R')

library(shinyjs)

#source('helpers.R')
shinyUI(fluidPage(
  useShinyjs(),
  div(
    id = "login_page",
    titlePanel("Welcome to the experiment!"),
    br(),
    sidebarLayout(
      
      sidebarPanel(
        h2("Login"),
        p("Welcome to today's experiment. Please use the user name provided on the instructions to login into the experiment."),
        hidden(
          div(
            id = "login_error",
            span("Your user name is invalid. Please check for typos and try again.", style = "color:red")
          )
        )
      ),
      
      mainPanel(
        textInput("user", "User", ""),
        textInput("password", "Password", ""),
        actionButton("login", "Login", class = "btn-primary")
      )
    )
  ),
  
  hidden(
    div( id = "instructions",
         h3("Here we post instructions for subjects..."),
         p("In this experiment you will have to guess the in wich direction
           a coin that is tossed repeatedly is biased. You will observe whether
           the coin landed heads or tails over several tosses.... Bla bla"),
         actionButton("confirm", label = "Ok, I got it... let's start")
         )
    ),
  
  hidden(
    div(
      id = "form",
      titlePanel("Main experimental screen"),
      
      sidebarLayout(
        
        sidebarPanel(
          p("Indicate whether you think the coin that was tossed is more likely to land heads or tails based on the throws shown to you on the right."),
          radioButtons("guess",
                       label = h3("Your based on the tosses so far"),
                       choices = list("Heads" = "Heads", "Tails" = "Tails"),
                       selected = NULL),
          actionButton("submit", "Submit", class = "btn-primary")
        ),
        
        mainPanel(
          h4(textOutput("round_info")),
          dataTableOutput(outputId="table")
        )
      )
    )
  ),
  
  hidden(
    div(
      id = "end",
      titlePanel("Thank you!"),
      
      sidebarLayout(
        
        sidebarPanel(
          p("You have reached the end of the experiment. Thank you for your participation."),
          h4("Your payoff details:"),
          textOutput("round")
        ),
        
        mainPanel(
          h4("Overview over your choices"),
          dataTableOutput(outputId="results")
        )
      )
    )
  )
  )
  )

shinyServer(
  function(input, output, session) {
    
    ##########################################################
    ########### PART I: LOGIN ################################
    ##########################################################
    
    # When the Login button is clicked, check whether user name is in list
    observeEvent(input$login, {
      
      # User-experience stuff
      shinyjs::disable("login")
      
      # Check whether user name is correct
      # Fix me: test against a session-specific password here, not username
      user_ok <- input$password==session_password
      
      # If credentials are valid push user into experiment
      if(user_ok){
        shinyjs::hide("login_page")
        shinyjs::show("instructions")
        
        # Save username to write into data file
        output$username <- renderText({input$user})
      } else {
        # If credentials are invalid throw error and prompt user to try again
        shinyjs::reset("login_page")
        shinyjs::show("login_error")
        shinyjs::enable("login")
      }
      
    })
    
    ##########################################################
    ########### PART II: INSTRUCTIONS ########################
    ##########################################################
    
    observeEvent(input$confirm, {
      hide("instructions")
      show("form")
    })
    
    ##########################################################
    ########### PART III: MAIN EXPERIMENT ####################
    ##########################################################
    
    ## Initialize reactive values
    # round is an iterator that counts how often 'submit' as been clicked.
    values <- reactiveValues(round = 1)
    # df will carry the responses submitted by the user
    values$df <- NULL
    
    ##########################################################
    ########## PART IIIa: MAIN HANDLER #######################
    ##########################################################
    
    ## This is the main experiment handler
    # Observe the submit button, if clicked... FIRE
    observeEvent(input$submit, {
      
      # Increment the round by one
      isolate({
        values$round <- values$round +1
      })
      
      # Call function formData() (see below) to record submitted response
      newLine <- isolate(formData())
      
      # Write newLine into data frame df
      isolate({
        values$df <- rbind(values$df, newLine)
      })
      
      # Has the user reached the end of the experiment?
      # If so then...
      if(values$round > n_guesses){
        
        # Draw a round from all rounds with equal probability
        # Note: the username must be numeric here, because it serves
        # as a seed for the RNG. See comment below.
        isolate(values$payroll <- payoffRound(as.numeric(input$user)))
        
        # Based on the drawn round determine the payoff. People get a Euro for having guessed correctly.
        output$round <- renderText({
          paste0("The computer selected round ", values$payroll,
                 ". Because you guessed ",ifelse(values$df[values$payroll, 3]==true_state[values$payroll], "correctly ", "incorrectly "),
                 "we will add ", ifelse(values$df[values$payroll, 3]==true_state[values$payroll], prize, 0),
                 " Euro to your show-up fee. Your total payoff will therefore equals ",
                 ifelse(values$df[values$payroll, 3]==true_state[values$payroll], prize, 0) + show_up, " Euro.")
        })
        isolate(values$df[, 5] <- ifelse(values$df[values$payroll, 3]==true_state[values$payroll], prize, 0) + show_up)
        
        # The function saveData() writes the df to disk.
        # This can be a remote server!
        saveData(values$df)
        
        # Say good-bye
        hide(id = "form")
        show(id = "end")
      }
    })
    
    ## Utilities & functions
    
    # I take formData from Dean with minor changes.
    # When it is called, it creates a vector of data.
    # This will be a row added to values$df - one for each round.
    #
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(round = values$round-1, data, timestamp = humanTime(), payoff = NA)
      data <- t(data)
      data
    })
    
    # The coin flips shown on the right.
    # Note I have added a small delay with progress bar, just to give
    # users a more natural look-and-feel, since throwing coins usually takes time.
    # I have disabled all of the features of DT below, because they distract users
    output$table <- renderDataTable({
      if(values$round > 1 && values$round <= n_guesses){
        withProgress(message = 'Flipping the coin.',
                     detail = 'Please wait...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.02)
                       }
                     })
      }
      idx.row <- sum(!is.na(flips[, min(values$round, n_guesses)]))
      idx.col <- min(values$round, n_guesses)
      data.frame(Wurf = seq(1, idx.row), Seite= flips[1:idx.row, idx.col])
    },
    options = list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE
    )
    )
    
    # This renders the table of choices made by a participant that is shown
    # to them on the final screen
    output$results <- renderDataTable({
      out <- data.frame(Round = rep(seq(1,n_rounds), each = guesses_per_round),
                        Guess = rep(seq(1, guesses_per_round), times = n_rounds),
                        choice = values$df[,3],
                        actual = rep(true_state, each = guesses_per_round)
      )
      colnames(out) <- c("Round", "Guess no.", "Your choice", "Correct/True value")
      out
    },
    options = list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE
    )
    )
    
    # Tell user where she is
    output$round_info <- renderText({
      paste0("Round ", ceiling(values$round/guesses_per_round), " of ", n_rounds)
    })
    
  }
)

