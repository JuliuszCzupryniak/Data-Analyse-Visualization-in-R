#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(EnglishPremierLeague)
library(sqldf)
referees <- sqldf('select distinct(Referee) from referee_cards')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Premier League Analysis by Juliusz Czupryniak"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="chosenReferee",
            label="Choose Referee",
            choices=referees,
            selected="M Dean"),
            selectInput(inputId="Season",
                        label="Choose Season",
                        choices=shots$Season,
                        selected=19),
            selectInput(inputId="HA",
                        label="Choose Home or Away",
                        choices=c("as Home","as Away"),
                        selected="as Home")
            


        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Home_cards",width = "100%",height = "900px"),
        )
    )
))
