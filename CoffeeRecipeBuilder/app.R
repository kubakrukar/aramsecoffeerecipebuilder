#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# https://aramse.coffee/recipe/

# https://jrowen.github.io/rhandsontable/
# https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
# https://stackoverflow.com/questions/22377913/ggplot2-and-chron-barplot-of-time-data-scale-x-chron




library(shiny)
library(tidyverse)
library(rhandsontable)
library(lubridate)
library(here)
library(hms)
library(showtext)
library(ggtext)

font_add_google("Nunito", bold.wt = 900)
showtext_auto()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Recipe builder based on the Aramse framework"),

    # the rest
    verticalLayout(
        br(),
        p("* enter all events in chronological order",  style = "font-family: 'times'; font-si16pt"),
        p("* right-click to add and remove rows"),
        p("* the format for Start and End Time is m:s. The maximum number of seconds in this format is 60."),
        p("* Event Type must be one of following: 
          Bloom, Distribute, Invert, Swirl, Break Crust, Draw Down, Pour, Stop Brew, Cap On, Grind, Press, Stir, Range"),
        p("* ranges must be entered as separate rows (Event Type: Range) with Start and End Time; 
          the Start Time MUST be the same as the End Time of the previous event (the one that you want to extend by a Range)"),
        p("* events with an End Time get a horizontal coloured line; events without `End Time` are considered short events, with a single vertical line"),
        p("* if you don't want to display the note for a particular event, delete all text or write `NA` in the `Note` column"),
        p("* you can break lines in the Notes with option+enter on a mac"),
        br(),
        rHandsontableOutput("hot2"),
        br(),
        rHandsontableOutput("hot"),
        actionButton("saveBtn", "Save"),
        br(),
        textInput("tableScale", 
                     "Manually change the length of the scale (m:s). 
                     If left empty, the scale length is rounded up to the next full minute based on the times in the table.", 
                     value = NA),
        br(),
        checkboxInput("metaON", "Display recipe description", value = TRUE),
        plotOutput("recipePlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    fname = tempfile(fileext = ".csv")
    fname2 = tempfile(fileext = ".csv")
    
    observe({
        # remove button and isolate to update file automatically
        # after each table change
        # input$saveBtn
        hot = isolate(input$hot)
        if (!is.null(hot)) {
            write.csv(hot_to_r(input$hot), fname)
            print(fname)
        }
        
        hot2 = isolate(input$hot2)
        if (!is.null(hot2)) {
            write.csv(hot_to_r(input$hot2), fname2)
            print(fname2)
        }
    })
    
    # events by Aramse ####
    events <- tribble(
        ~`Event Type`, ~colour, ~abbreviation, ~is.range,
        "Bloom", "#a0dffc", "B", FALSE,
        "Distribute", "#617f63", "Di", FALSE,
        "Invert", "#617f63", "In", FALSE,
        "Swirl", "#f6c66a", "S", FALSE,
        "Break Crust", "#f6c66a", "BC", FALSE,
        "Draw Down", "#d4c7bc", "Dr", FALSE,
        "Pour", "#a0dffc", "P", FALSE,
        "Stop Brew", "#f09385", "SB", FALSE,
        "Cap On", "#617f63", "Ca", FALSE,
        "Grind", "#617f63", "G", FALSE,
        "Press", "#f6c66a", "Pr", FALSE,
        "Stir", "#f6c66a", "St", FALSE,
        "Range", NA, NA, TRUE
    )
    
    output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            DF <-  read.csv(here("CoffeeRecipeBuilder", "hoffman.csv"), stringsAsFactors = FALSE)
            DF <- DF %>%  
                transmute(
                `Event Type` = factor(event_type, 
                                      levels = events$`Event Type`),
                `Start Time` = time_start,
                `End Time`   = time_end,
                `Quantity`   = as.character(quantity),
                `Note`       = as.character(note)
            )
        }
        
        DF %>% 
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$hot2 = renderRHandsontable({
        if (!is.null(input$hot2)) {
            META = hot_to_r(input$hot2)
        } else {
            META <-  read.csv(here("CoffeeRecipeBuilder", "hoffman.csv"), stringsAsFactors = FALSE)
            META <- META %>% select(recipe,brewer,coffee,grind,water)
            META <- META %>%  
                transmute(
                    `Recipe` = as.character(recipe),
                    `Brewer` = as.character(brewer),
                    `Coffee` = as.character(coffee),
                    `Grind`  = as.character(grind),
                    `Water`  = as.character(water)
                )
            META <- META[1,]
        }

        META %>% 
            rhandsontable(rowHeaders = NULL) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$recipePlot <- renderPlot({
        
        # set variables for vertical size ####
        # adjust these values to manipulate vertical distances between elements
        # you must also adjust height argument of the renderPlot({}, height = )
        # (at the bottom of the code, value in pixels)
        
        upper.bound       <- 20     # the upper boundary of the plot
        lower.bound       <- -20    # the lower boundary of the plot
        major.tick.size   <- -3     # ticks every minute
        minor.tick.size   <- -1.5   # ticks every 15 s
        mini.tick.size    <- -1     # ticks every 5 s
        shorter.up.line   <- 5      # length of shorter vertical lines with event abbreviations      
        longer.up.line    <- 7     # length of longer vertical lines with event abbreviations
        shorter.down.line <- -10     # length of shorter lines with notes
        longer.down.line  <- -14     # length of longer lines with notes
        above.timeline    <- 0.7      # how far above timeline are horizontal events plotted
        q.above.timeline  <- above.timeline + 1

        # data transformations ####
        DF = hot_to_r(input$hot)
        META = hot_to_r(input$hot2)
        
        # convert time
        DF <- DF %>% mutate(
            `Start Time` = as.POSIXct(`Start Time`, format = "%M:%S", tz = "GMT"),
            `End Time`   = as.POSIXct(`End Time`, format = "%M:%S", tz = "GMT")
        ) %>% mutate(
            `Start Time` = as_hms(`Start Time`),
            `End Time`   = as_hms(`End Time`)
        )
        
        # calculate position of labels
        DF <- DF %>% mutate(
            vert.position = shorter.up.line
        )
        
        # offset the vertical positions if they're too close to each other
        if (nrow(filter(DF, vert.position > 0)) > 1){
            
            DF[DF$vert.position > 0, "vert.position"] <- ifelse(
                lead(DF[DF$vert.position > 0, "Start Time"]) - DF[DF$vert.position > 0, "Start Time"] < 15,
                longer.up.line,
                shorter.up.line)
            
            DF[is.na(DF$vert.position), "vert.position"] <- shorter.up.line
        }
        
        # new column for events with notes (for lower vertical lines)
        DF <- DF %>% 
            mutate(
            vert.position2 = ifelse(Note != "" & Note != "NA" & !is.na(Note), shorter.down.line, NA)
        )
        
        # offset also the lower vertical positions if they're too close to each other
        if (sum(!is.na(DF$vert.position2)) > 1){
        
            hack <- lead(DF[!is.na(DF$vert.position2), "Start Time"])    # hack to ensure the last note always stays at position -1
            hack[length(hack)] <- as_hms("24:00:00")                     # because lead() returns NA for the last value which results in NA
                                                                         # in the next ifelse()
            DF[!is.na(DF$vert.position2), "vert.position2"] <- ifelse(
                 hack - DF[!is.na(DF$vert.position2), "Start Time"] < 15,
                longer.down.line,
                shorter.down.line)
        }
        
        # offset starting time to avoid overlap of vertical lines
        if (nrow(filter(DF, vert.position > 0)) > 1){
            
            for (i in 1:nrow(DF)){
                j <- i+1
                if (DF[i, "vert.position"] == longer.up.line){
                    DF[j, "Start Time"] <- DF[j, "Start Time"] + 1
                }
            }
        }
        
        # create events to plot ####
        # by combining events and DF
        
        d <- left_join(DF, events, by = "Event Type")
        d <- d %>% mutate(colour = ifelse(is.na(colour), lag(colour), colour),
                          has.horizontal = ifelse(is.na(`End Time`), FALSE, TRUE))

        
        # build plot ####
        
        # length of scale, in seconds
        tablescale.exact <- max(c(as.numeric(d[,"Start Time"]), 
                                  as.numeric(d[,"End Time"])), 
                                na.rm=TRUE)
        tablescale       <- tablescale.exact + (60 - tablescale.exact %% 60)                     # takes the next multiple of 60
        scalemax         <- ifelse(input$tableScale == "", 
                                   tablescale, 
                                   as.numeric(
                                       as_hms(
                                           as.POSIXct(input$tableScale, format = "%M:%S", tz = "GMT"))))
        
        # create color scale
        colours <- setNames(d$colour, d$colour)
        
        # ggplot ####
        recipe <- ggplot(d, 
                         aes(x = `Start Time`,
                             y = 0, 
                             label = `Event Type`)) + 
            theme_classic() +
            scale_x_time(limits = c(- (scalemax / 70), scalemax)) +
            scale_y_continuous(limits = c(lower.bound, upper.bound)) +
            theme(axis.line.y   = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x  = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x  = element_blank(),
                   legend.position = "none"
            )
        
        # show minor ticks
        recipe <- recipe + 
            geom_segment(data = data.frame(t= seq(hms(0),scalemax,15)),
                         aes(y = minor.tick.size,
                             yend = 0,
                             x = t,
                             xend = t,
                             label = NULL),
                         size = 1,
                         color='black')
        
        # show mini ticks
        recipe <- recipe + 
            geom_segment(data = data.frame(t= seq(hms(0),scalemax,5)),
                         aes(y = mini.tick.size,
                             yend = 0,
                             x = t,
                             xend = t,
                             label = NULL),
                         size = 0.5,
                         color='black')
        
        
        
        
        # Plot horizontal black line for timeline
        recipe <- recipe + geom_hline(yintercept = 0, 
                                      color = "black", 
                                      size = 1)
        
        # Plot vertical segment lines for milestones
        recipe <- recipe + geom_segment(data =filter(d, is.range == FALSE),
                                        aes(y    = vert.position,
                                            yend = above.timeline,
                                            xend = `Start Time`,
                                            colour = colour), 
                                        size=1) + 
            geom_text(data =filter(d, is.range == FALSE),
                      aes(x = `Start Time`,
                          y = vert.position,
                          label = abbreviation),
                      hjust = "left",
                      vjust = "top",
                      size = 8,
                      nudge_x = 1,
                      family = "Nunito",
                      fontface = "bold"
                      )
        
        # Plot lower vertical segment lines for notes
        if( nrow(d[!is.na(d$vert.position2),]) > 0){
        recipe <- recipe + geom_segment(data = filter(d, !is.na(vert.position2)), 
                                        aes(y    = vert.position2,
                                            yend = above.timeline,
                                            xend = `Start Time`,
                                            colour = colour), 
                                        size=1) +
            geom_textbox(data = filter(d, !is.na(vert.position2)),
                                    aes(x = `Start Time`,
                                        y = vert.position2,
                                        label = `Note`),
                      size = 5,
                      hjust = 0,
                      vjust = 0,
                      nudge_x = 1,
                      width = unit(10, "cm"),
                      box.padding = unit(c(0, 0, 0, 0), "pt"),
                      box.r = unit(0, "pt"),
                      box.colour = "white",
                      family = "Nunito")
        }
        
        # plot dots at the timeline for evens
        #recipe <- recipe + geom_point(aes(y=0), size=3)
        
        # show major ticks
        recipe <- recipe + 
            geom_segment(data = data.frame(t= seq(hms(0),scalemax,60)),
                         aes(y = major.tick.size,
                             yend = 0,
                             x = t,
                             xend = t,
                             label = NULL),
                         size = 1,
                         color='black') +
            geom_text(data = data.frame(t= seq(hms(0),scalemax,60)),
                      aes(x = t,
                          y = major.tick.size,
                          label = t/60),
                      size=8, 
                      hjust="left",
                      vjust = "bottom",
                      nudge_x = - (scalemax / 70),
                      color='black',
                      family = "Nunito",
                      fontface = "bold")
        
        # plot horizontal lines for events
        recipe <- recipe + geom_segment(data = filter(d, has.horizontal == TRUE, 
                                                      is.range == FALSE),
                                        aes(y = above.timeline,
                                            yend = above.timeline,
                                            x = `Start Time` + 0.3,
                                            xend = `End Time`- 1,
                                            colour = colour), 
                                        size = 3, 
                                        lineend = "round") +
            geom_text(data = filter(d, has.horizontal == TRUE, is.range == FALSE),
                      aes(x = `End Time`,
                          y = q.above.timeline,
                          label = Quantity,
                          hjust = "right"),
                      nudge_x = - 1.5,
                      size  = 5, 
                      color ='black',
                      family = "Nunito")
        
        # plot horizontal ranges
        recipe <- recipe + geom_segment(data = filter(d, 
                                                      has.horizontal == TRUE, 
                                                      is.range == TRUE),
                                        aes(y = above.timeline,
                                            yend = above.timeline,
                                            x = `Start Time` + 0.3,
                                            xend = `End Time` - 1,
                                            colour = colour), 
                                        size = 3, 
                                        alpha = 0.6, 
                                        lineend = "round"
        ) + scale_color_manual(values=colours)
        
        # plot dots for short events
        recipe <- recipe + geom_point(data = filter(d, has.horizontal == FALSE),
                                      aes(x = `Start Time`,
                                          y = above.timeline,
                                          colour = colour),
                                      size = 3)
        
        # plot metadata ####
        if (input$metaON == TRUE){
            META <- pivot_longer(META, cols = everything())
            META2 <- META
            META2$value <- paste0("**", META2$value, "**")
            META2 <- data.frame(x = paste(META2$name, META2$value, sep = ": "))
            META2 <- data.frame(x = paste(META2[1,], META2[2,], 
                                          META2[3,], META2[4,], META2[5,], sep = "<br><br>"))
            
            recipe <-  recipe + geom_textbox(data = META2,
                                             aes(x = 0,
                                                 y = upper.bound,
                                                 label = x),
                                             inherit.aes = FALSE,
                                             width = unit(10, "cm"),
                                             vjust = 1,
                                             hjust = 0,
                                             box.colour = "#ffcbbf",
                                             fill = "#ffcbbf",
                                             box.padding = unit(c(0.5,0.5,0.5,0.5), "cm"),
                                             box.r = unit(20, "pt"),
                                             size = 5,
                                             famil = "Nunito")
        }
        
        recipe
    },
    height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)

