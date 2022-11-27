# App to play Tower of Hanoi game in R shiny
# Author: Emma Kroell
# Last updated: November 26, 2022

library(shiny)
library(shinyjs)
library(tidyverse)
source("tower_game_shiny.R")

ui <- fluidPage(
  titlePanel(
    # app title
    "Tower of Hanoi game"
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
      useShinyjs(),   # lets us do cool shiny stuff like hide and show inputs
      sliderInput("n","Height of tower",value=3,min=1,max=7,step=1),  # choose tower height
      actionButton("start","Start Game/Reset"),   # start game
      hr(),
      hidden(radioButtons("move","Your move", seq(1,3),
                          selected = character(0))),   # move choices
      hr(),
      hidden(textOutput("text")),   # move counter
      hr(),
      HTML("The Tower of Hanoi is a puzzle where your goal is to move the blocks
        from spot 1 to spot 3, in the same order. However, you must follow three rules:
        <br> 1. You can only move one block at a time.
        <br> 2. You can only move the top block in each stack.
        <br> 3. You can only stack smaller blocks on top of large ones.")
    ),
    mainPanel(plotOutput("towerplot"))   # plot of towers
  )
)

server <- function(input, output,session) {

  values <- reactiveValues(
    n = NULL,   # Height of tower
    tower = NULL,   # tower matrix
    move_options = NULL,   # move options each time
    move_counter = NULL   # number of moves taken
  )

  # START GAME
  observeEvent(input$start,{
    # set up towers
    values$n <- as.numeric(input$n)
    values$tower <- matrix(c(values$n:1,rep(0,values$n),rep(0,values$n)),ncol=3)
    # plot initial towers
    output$towerplot <- renderPlot({
      plot_towers(values$tower)
    }, height=400)

    # set moves counter to 0
    values$move_counter <- 0

    # show options for player to click
    # update buttons first
    values$move_options <- tower_move_choices(values$tower)
    updateRadioButtons(session,"move","Your move",
                       choiceValues = values$move_options$move_number,
                       choiceNames = values$move_options$label,
                       selected = character(0))
    show("move")
  })

  # PLAYER MOVES
  observeEvent(input$move,{
    if (input$move > 0) {
      # update counter
      values$move_counter <- values$move_counter + 1

      # make move player chose
      values$tower <- tower_move(values$tower,
                                 values$move_options$move_from[as.numeric(input$move)],
                                 values$move_options$move_to[as.numeric(input$move)])


      show("text")   # show counter
      output$text <- renderText({paste("Number of moves:",values$move_counter)})

      # Check win conditions
      if (all(values$tower[,3]==seq(values$n,1))){
        # player wins
        if (values$move_counter == (2^values$n-1)){
          output$towerplot <- renderPlot({
            ggplot() + theme_void() +
              annotate("text", x=2.5, y=2.5,
                       label= "You win! \n You played optimally :D", size = 16)
          }, height=400)
        } else {
          output$towerplot <- renderPlot({
            ggplot() + theme_void() +
              annotate("text", x=2.5, y=2.5,
                       label= paste("You win! You took \n",
                                    values$move_counter - (2^values$n-1),
                                    "more moves than optimal."),
                       size = 14)
          }, height=400)
        }

        hide("move")
      } else {
        # otherwise update plot
        output$towerplot <- renderPlot({
          plot_towers(values$tower)
        }, height=400)


        delay(100, {
          # reset buttons for next player move
          values$move_options <- tower_move_choices(values$tower)
          updateRadioButtons(session,"move","Your move",
                             choiceValues = values$move_options$move_number,
                             choiceNames = values$move_options$label,
                             selected = character(0))
        })
      }
    }})
}

shinyApp(ui, server)
