#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library (shinythemes)

load("Demographics.RData")
load("nc_new.RData")


# Define UI for application that draws a map of NC with differences in 
# beta coefficients between Democrats and Republicans at different times
ui <- fluidPage(theme = shinytheme("flatly"),
    
    navbarPage("Spatio-Temporal NC Votes",
      tabPanel("Inference",
  
       # Application title
       titlePanel("Inferring Demographic Effects on NC Voter Turnout"),
       
       sidebarLayout(
         # Sidebar panel for demographic inputs ----
         sidebarPanel(
           
           
           # Input: Selector for year to plot----
           selectInput("year", "Year:", 
                       c("2004" = "2004",
                         "2008" = "2008",
                         "2012" = "2012")
                       ),
           
           # Input: Selector for race to plot ----
           selectInput("demo", "Demographic:", 
                       c("White" = "White",
                         "Black" = "Black",
                         "Asian" = "Asian",
                         "Hispanic" = "Hispa",
                         "Gender" = "Femal",
                         "Age" = "Age")
                       )
         ),
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("mapPlot")
            )
         )
       ),
      
    tabPanel("Prediction")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$mapPlot <- renderPlot({
    #Capture user query string
    query_demodiff = paste0(input$demo,"Diff")
    query_demodiffyear = paste0(input$demo,"Diff", input$year)
    
    # Generate demographic+year string to query dataframe
    if (input$year == 2004){year_tic = 1}
    if (input$year == 2008){year_tic = 2}
    else year_tic = 3
    
    nc_new[query_demodiffyear] = Demographics[seq(year_tic,39,by=3), query_demodiff]
    
    #Plot demographic coefficient differences on NC map with new districts
    plot(nc_new[,query_demodiffyear], main = paste0("Differences in Beta Coefficients for ",
                                                    (input$year)))
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

