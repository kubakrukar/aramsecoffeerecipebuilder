#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# https://aramse.coffee/recipe/

library(shiny)
library(tidyverse)
library(rhandsontable)
library(lubridate)
library(here)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    verticalLayout(
        rHandsontableOutput("hot"),
        actionButton("saveBtn", "Save"),
        plotOutput("recipePlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    fname = tempfile(fileext = ".csv")
    
    observe({
        # remove button and isolate to update file automatically
        # after each table change
        input$saveBtn
        hot = isolate(input$hot)
        if (!is.null(hot)) {
            write.csv(hot_to_r(input$hot), fname)
            print(fname)
        }
    })
    
    output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            DF <-  read.csv(here("CoffeeRecipeBuilder", "recipe.csv"), stringsAsFactors = FALSE)
            DF <- DF %>% transmute(
                `Event Type` = factor(event_type, 
                                      levels = c("Bloom", "Break Crust", "Cap On", "Distribute",
                                                 "Draw Down", "Grind", "Invert", "Pour",
                                                 "Press", "Swirl", "Stop Brew", "Stir")),
                `Start Time` = time_start,
                `End Time`   = time_end,
                `Quantity`   = quantity,
                `Note`       = as.character(note)
            )
            
            
        }
        
        # https://jrowen.github.io/rhandsontable/
        
        DF %>% 
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    
    # https://stackoverflow.com/questions/32786640/how-do-i-set-time-range-hhmmss-on-y-axis-using-ggplot2
    # https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
    # https://stackoverflow.com/questions/22377913/ggplot2-and-chron-barplot-of-time-data-scale-x-chron
    
    
    output$recipePlot <- renderPlot({
        
        # DATA TRANSFORMATION ####
        DF = hot_to_r(input$hot)
        
        # convert time
        DF <- DF %>% mutate(
            `Start Time` = as.POSIXct(`Start Time`, format = "%M:%S", tz = "GMT"),
            `End Time`   = as.POSIXct(`End Time`, format = "%M:%S", tz = "GMT"),
        )
        
        # calculate position of labels
        DF <- DF %>% mutate(
            vert.position = 1, 
            vert.position = ifelse(
                diff(`Start Time`) > 15, 1, 1.5)
        )
        
        # duplicate rows with notes (for lower vertical position)
        for (j in nrow(DF):1){
            i <- j-1
            if (DF[j, "Note"] != "empty") {
                DF[i,] <- DF[j,]
                DF[i, "Note"] <- "empty"
                DF[j, "vert.position"] <- -1
                DF[j, "End Time"] <- NA
                DF[j, "Quantity"] <- NA
            }
        }
        
        # offset also the lower vertical positions if they're too close to each other
        if (nrow(filter(DF, vert.position < 0)) > 1){
        
        DF[DF$vert.position < 0, "vert.position"] <- ifelse(
            abs(
                diff(DF[DF$vert.position < 0, "Start Time"])
            ) > 15,
            -1,
            -1.5)
        }
        
        # PLOT BUILDING ####
        
        
        recipe <- ggplot(DF, 
                         aes(x = `Start Time`, 
                             y = 0, 
                             label = `Event Type`)) + theme_classic()
        
        # Plot horizontal black line for timeline
        recipe <- recipe + geom_hline(yintercept = 0, 
                                      color = "black", 
                                      size = 0.3)
        
        # Plot vertical segment lines for milestones
        recipe <- recipe + geom_segment(aes(y    = vert.position,
                                            yend = 0,
                                            xend = `Start Time`), 
                                        color='black', 
                                        size=0.2)
        
        recipe
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
