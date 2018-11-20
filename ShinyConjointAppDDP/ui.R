library(shiny)
library(plotly)
library(rhandsontable)
library(tidyr)
library(dplyr)

shinyUI(fluidPage(

# Application title
titlePanel("Sandwich Preference App"),

h3("This app allows you to analyze the preferences of customers towards sandwiches.",
   "Utilizing results of a conjoint study, it allows you to simulate market scenarios",
   "and answer many interesting business questions"),

fluidRow(
        column(7,
        wellPanel(
                helpText("Try changing the specifications of the sandwitches",
                         "and observing how the preferances change.",
                         "You can also compose your new sandwich and estimate",
                         "its future preference. Try right clicking the table",
                         "to add or delete a product."),
                h3("Menu Table", style="text-align:center"),
                br(),
                rHandsontableOutput("scenarioTable", height = "300px")
        )),
        column(5,
        wellPanel(
                h4("Customers' preferences", style="text-align:center"),
                br(),
                plotlyOutput("preferencePlot"),
                helpText("The plot presents the customers' preferences for sandwitches",
                         "from the menu. Try changing the menu by altering the products,",
                         "adding new ones or deleting current positions and see how the",
                         "preferences change")
        ))
),
helpText("Note: this app is based on simulated conjoint data. For more info on conjoint analysis, please see",
         a("this wikipedia article", href="https://en.wikipedia.org/wiki/Conjoint_analysis"))
))
