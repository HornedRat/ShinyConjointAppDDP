library(shiny)
library(plotly)
library(rhandsontable)
library(tidyr)
library(dplyr)

data <- read.csv("ConjointFile.csv")

catFile <- data %>%
        gather(key="level", value = "utility", -1)

categories <- read.csv("Categories.csv")
scenario <- read.csv("DefaultScenario.csv")

scenario$name <- as.character(scenario$name)
for (c in 2:ncol(scenario)) {
        lvls <- categories[categories$attribute == names(scenario)[c], "level"]
        scenario[,c] <- factor(scenario[,c], levels = lvls)
}

###### FUNCTIONS FOR PREFERANCE
attractiveness <- function(product, data = catFile) {
        #product is a character vector listing the levels of attributes, for example:
        #scenario[scenario$name == "VW", "level"]
        #returns a TIBBLE
        data %>%
                filter(level %in% product) %>%
                group_by(id) %>%
                summarise(attractivness = sum(utility))
}


#Counting share of preference for defined products
countPref <- function(scenario, data=catFile) {
        
        #computes attractiveness of each product for each respondent
        #initiate an empty df
        products <- unique(scenario$name)
        attracts <- data.frame(matrix(ncol = length(products),
                                      nrow=length(unique(data$id))))
        
        #loops through products
        #computes attractiveness for each product for each respondent
        for(p in 1:length(products)) {
                temp <- scenario[scenario$name == products[p], "level"]
                attracts[,p] <- attractiveness(temp)[,2]
        }
        
        #chooses most preferred product for each respondent
        choice <- apply(attracts, 1, function(r) {
                which(r == max(r))[1]
                #added [1] to prevent situation with >1 max values (happens with several identival products)
                #in this situation only the first one of those will be chosen
        })
        
        #returns the product names along with their prefs
        scenPref <- data.frame(Product = products)
        scenPref$pref <- NA
        
        for(s in 1:nrow(scenPref)) {
                p <- sum(choice == s)/ length(choice)
                scenPref$pref[s] <- p
        }
        
        return(scenPref)
}


#
shinyServer(function(input, output, session) {
        #creates a reactive object for storing values
        values <- reactiveValues()
        
        #observes for changes in the scenario table
        observe({
                if (!is.null(input$scenarioTable)) {
                        DF <- hot_to_r(input$scenarioTable)
                } else {
                        if (is.null(values[["DF"]])) {
                                DF <- scenario
                        } else {
                                DF <- values[["DF"]]
                        }
                }
                scenario.r <- gather(DF, key = "attribute", "level", -name)
                preference <- countPref(scenario.r)
                
                values[["DF"]] <- DF
                values[["preference"]] <- preference
        })
        
        output$scenarioTable <- renderRHandsontable({
                DF <- values[["DF"]]
                if (!is.null(DF))
                        rhandsontable(DF, stretchH = "all")
        })
        
        output$preferencePlot <- renderPlotly({
                p <- plot_ly(values[["preference"]],
                             labels = ~Product,
                             values = ~pref,
                             type = 'pie')
                
                p })
        
})
