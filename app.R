#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a map of NC with differences in 
# beta coefficients between Democrats and Republicans at different times

library(shiny)
library (shinythemes)
library(knitr)

ui <- fluidPage(theme = shinytheme("flatly"),
    
    navbarPage("Spatio-Temporal NC Votes",
      tabPanel("Prediction",
  
       # Application title
       titlePanel("Inferring Demographic Effects on NC Voter Turnout"),
       
       sidebarLayout(
         # Sidebar panel for demographic inputs ----
         sidebarPanel(
           
           # Input: Selector for year to plot----
           selectInput("year", "Year:", 
                       c("2004" = 1,
                         "2006" = 2,
                         "2008" = 3,
                         "2010" = 4,
                         "2012" = 5,
                         "2014" = 6,
                         "2016" = 7)
           ),
           
           # Input: Selector for county to hold out ----
           selectInput("county", "Holdout County:", 
                       c("ASHE" = 1,
                         "ALLEGHANY"=2,
                         "SURRY"=3,
                         "CURRITUCK"=4,
                         "NORTHAMPTON"=5,
                         "HERTFORD"=6,
                         "CAMDEN"=7,
                         "GATES"=8,
                         "WARREN"=9,
                         "STOKES"=10,
                         "CASWELL"=11,
                         "ROCKINGHAM"=12,
                         "GRANVILLE"=13,
                         "PERSON"=14,
                         "VANCE"=15,
                         "HALIFAX"=16,
                         "PASQUOTANK"=17,
                         "WILKES"=18,
                         "WATAUGA"=19,
                         "PERQUIMANS"=20,
                         "CHOWAN"=21,
                         "AVERY"=22,
                         "YADKIN"=23,
                         "FRANKLIN"=24,
                         "FORSYTH"=25,
                         "GUILFORD"=26,
                         "ALAMANCE"=27,
                         "BERTIE"=28,
                         "ORANGE"=29,
                         "DURHAM"=30,
                         "NASH"=31,
                         "MITCHELL"=32,
                         "EDGECOMBE"=33,
                         "CALDWELL"=34,
                         "YANCEY"=35,
                         "MARTIN"=36,
                         "WAKE"=37,
                         "MADISON"=38,
                         "IREDELL"=39,
                         "DAVIE"=40,
                         "ALEXANDER"=41,
                         "DAVIDSON"=42,
                         "BURKE"=43,
                         "WASHINGTON"=44,  
                         "TYRRELL"=45,
                         "MCDOWELL"=46,
                         "RANDOLPH"=47,     
                         "CHATHAM"=48,     
                         "WILSON"=49,       
                         "ROWAN"=50,        
                         "PITT"=51,
                         "CATAWBA"=52,
                         "BUNCOMBE"=53,
                         "JOHNSTON"=54,
                         "HAYWOOD"=55,
                         "DARE"=56,
                         "BEAUFORT"=57,
                         "SWAIN"=58,
                         "GREENE"=59,
                         "LEE"=60,
                         "RUTHERFORD"=61,
                         "WAYNE"=62,
                         "HARNETT"=63,
                         "CLEVELAND"=64,
                         "LINCOLN"=65,
                         "JACKSON"=66,
                         "MOORE"=67,
                         "MECKLENBURG"=68,
                         "CABARRUS"=69,
                         "MONTGOMERY"=70,
                         "STANLY"=71,
                         "HENDERSON"=72,
                         "GRAHAM"=73,
                         "LENOIR"=74,
                         "TRANSYLVANIA"=75,
                         "GASTON"=76,
                         "POLK"=77,
                         "MACON"=78,
                         "SAMPSON"=79,
                         "PAMLICO"=80,
                         "CHEROKEE"=81,
                         "CUMBERLAND"=82,
                         "JONES"=83,
                         "UNION"=84,
                         "ANSON"=85,
                         "HOKE"=86,
                         "HYDE"=87,
                         "DUPLIN"=88,
                         "RICHMOND"=89,
                         "CLAY"=90,
                         "CRAVEN"=91,
                         "SCOTLAND"=92,
                         "ONSLOW"=93,
                         "ROBESON"=94,
                         "CARTERET"=95,
                         "BLADEN"=96,
                         "PENDER"=97,
                         "COLUMBUS"=98,
                         "NEW HANOVER"=99,
                         "BRUNSWICK"=100)
           ),
           
           # Input: Number of MCMC samples
           numericInput("n.samples", "MCMC Samples:", 100, min = 50, max = 10000),
           
           # Input: Number of burn-in
           numericInput("burn.in", "Burn-in:", 0, min = 0, max = 400),
           
           # Input: Beta normal prior mean
           numericInput("beta_mean_prior", "Beta Prior Mean:", 0.0, min = -10.0, max = 10.0),
           
           # Input: Beta normal prior variance
           numericInput("beta_var_prior", "Beta Prior Variance:", 10.0, min = 0.0, max = 1000.0),
           
           # Input: Phi uniform prior
           numericInput("phi_prior", "Phi Prior:", 1.0, min = 1.0, max = 100.0),
           
           # Input: Variance inv-gamma alpha prior
           numericInput("var_a_prior", "Variance Alpha Prior:", 5.0, min = 0.0, max = 100.0),
           
           # Input: Variance inv-gamma beta prior
           numericInput("var_b_prior", "Variance Beta Prior:", 1.0, min = 0.0, max = 100.0),
           
           # Input: Tau inv-gamma alpha prior
           numericInput("tau_a_prior", "Tau-Sq Alpha Prior:", 10.0, min = 0.0, max = 100.0),
           
           # Input: Tau-sq inv-gamma beta prior
           numericInput("tau_b_prior", "Tau-Sq Beta Prior:", 1.0, min = 0.0, max = 100.0),
          
           # Input: Error variance inv-Wishart df prior
           numericInput("error_df_prior", "Error df Prior:", 2.0, min = 0.0, max = 100),
           
           # Input: Error variance inv-Wishart scale prior
           numericInput("error_scale_prior", "Error Scale Prior:", .001, min = 0, max = 10)
           
         ),
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("actualPlot"),
             plotOutput("predPlot"),
             plotOutput("hoPlot")
            )
         )
       ),
      
    tabPanel("Write-Up",
             includeHTML('writeup.html'),
             imageOutput("preImage")
             )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  library(markdown)
  library(sf)
  library(spBayes)
  
  #Dictionary to look up county names
  numbers = seq(1,100,1)
  counties = c("ASHE",
               "ALLEGHANY",
               "SURRY",
               "CURRITUCK",
               "NORTHAMPTON",
               "HERTFORD",
               "CAMDEN",
               "GATES",
               "WARREN",
               "STOKES" ,
               "CASWELL" ,
               "ROCKINGHAM" ,
               "GRANVILLE" ,
               "PERSON" ,
               "VANCE" ,
               "HALIFAX" ,
               "PASQUOTANK" ,
               "WILKES" ,
               "WATAUGA" ,
               "PERQUIMANS" ,
               "CHOWAN" ,
               "AVERY" ,
               "YADKIN" ,
               "FRANKLIN" ,
               "FORSYTH" ,
               "GUILFORD" ,
               "ALAMANCE" ,
               "BERTIE" ,
               "ORANGE" ,
               "DURHAM" ,
               "NASH" ,
               "MITCHELL" ,
               "EDGECOMBE" ,
               "CALDWELL" ,
               "YANCEY" ,
               "MARTIN" ,
               "WAKE" ,
               "MADISON" ,
               "IREDELL" ,
               "DAVIE" ,
               "ALEXANDER" ,
               "DAVIDSON" ,
               "BURKE" ,
               "WASHINGTON" ,
               "TYRRELL" ,
               "MCDOWELL" ,
               "RANDOLPH" ,
               "CHATHAM" ,
               "WILSON" ,
               "ROWAN" ,
               "PITT" ,
               "CATAWBA" ,
               "BUNCOMBE" ,
               "JOHNSTON" ,
               "HAYWOOD" ,
               "DARE" ,
               "BEAUFORT" ,
               "SWAIN" ,
               "GREENE" ,
               "LEE" ,
               "RUTHERFORD" ,
               "WAYNE" ,
               "HARNETT" ,
               "CLEVELAND" ,
               "LINCOLN" ,
               "JACKSON" ,
               "MOORE" ,
               "MECKLENBURG" ,
               "CABARRUS" ,
               "MONTGOMERY" ,
               "STANLY" ,
               "HENDERSON" ,
               "GRAHAM" ,
               "LENOIR" ,
               "TRANSYLVANIA" ,
               "GASTON" ,
               "POLK" ,
               "MACON" ,
               "SAMPSON" ,
               "PAMLICO" ,
               "CHEROKEE" ,
               "CUMBERLAND" ,
               "JONES" ,
               "UNION" ,
               "ANSON" ,
               "HOKE" ,
               "HYDE" ,
               "DUPLIN" ,
               "RICHMOND" ,
               "CLAY" ,
               "CRAVEN" ,
               "SCOTLAND" ,
               "ONSLOW" ,
               "ROBESON" ,
               "CARTERET" ,
               "BLADEN" ,
               "PENDER" ,
               "COLUMBUS" ,
               "NEWHANOVER" ,
               "BRUNSWICK")   
  county_lookup = vector(mode="list", length=100)
  names(county_lookup) = numbers
  county_lookup = counties
  
  load("DATA.RData", envir=.GlobalEnv)
  demo(nc, ask = FALSE, echo = FALSE)
  nc = st_transform(nc,2264)
  
  values = reactiveValues()
  
  #spBayes model
  ###############################################
  y.t = data.frame(sapply(DATA[,6:12], as.numeric)) ; y.t.all = y.t
  N.t = ncol(y.t) ##number of months
  n = nrow(y.t) ##number of observation per months
  
  ##add some missing observations to illistrate prediction
  county = reactive({as.numeric(input$county)})
  miss = reactive({as.numeric(input$year) + 2}) #account for 9 years in data, we start at 2004 or 3
  y.t.holdout <- reactive({y.t[county, miss]})
  reactive({y.t[as.numeric(county), miss] <- NA})
  ###############################################
  
  coords = as.matrix(DATA[,c("X", "Y")])
  max.d = max(iDist(coords))
  
  ##set starting and priors
  p <- 3 #number of regression parameters in each month
  starting <- list("beta"=rep(0,N.t*p), "phi"=rep(3/(0.5*max.d), N.t),
                   "sigma.sq"=rep(2,N.t), "tau.sq"=rep(1, N.t),
                   "sigma.eta"=diag(rep(0.01, p)))
  
  #starting <- list("beta"=rep(c(.5,0,0),N.t), "phi"=rep(3/(0.5*max.d), N.t),
  #                 "sigma.sq"=rep(.25,N.t), "tau.sq"=rep(.01, N.t),
  #                 "sigma.eta"=diag(rep(0.01, p)))
  
  tuning <- list("phi"=rep(5, N.t))
  
  priors <- reactive({list("beta.0.Norm"=list(rep(as.numeric(input$beta_mean_prior),p), diag(as.numeric(input$beta_var_prior),p)),
                           "phi.Unif"=list(rep(0, N.t), rep(as.numeric(input$phi_prior), N.t)),
                           "sigma.sq.IG"=list(rep(as.numeric(input$var_a_prior),N.t), rep(as.numeric(input$var_b_prior),N.t)),
                           "tau.sq.IG"=list(rep(as.numeric(input$tau_a_prior),N.t), rep(as.numeric(input$tau_b_prior),N.t)),
                           "sigma.eta.IW"=list(as.numeric(input$error_df_prior), diag(as.numeric(input$error_scale_prior),p))
                           )
                      })
  
  #priors <- reactive({list("beta.0.Norm"=list(rep(input$beta_mean_prior,p), diag(c(.1,1,1),p)),
  #              "phi.Unif"=list(rep(3/(0.9*max.d), N.t), rep(3/(0.05*max.d), N.t)),
  #               "sigma.sq.IG"=list(rep(5,N.t), rep(1,N.t)),
  #               "tau.sq.IG"=list(rep(10,N.t), rep(1,N.t)),
  #               "sigma.eta.IW"=list(2, diag(0.001,p)))})
  
  ##make symbolic model formula statement for each month
  mods <- lapply(paste(colnames(y.t),'elev',sep='~'), as.formula)
  mods = 
    
    lapply(
      c(
        "Y_3 ~ DATA$Y_2 + DATA$Y_1",
        "Y_4 ~ DATA$Y_3 + DATA$Y_2",
        "Y_5 ~ DATA$Y_4 + DATA$Y_3",
        "Y_6 ~ DATA$Y_5 + DATA$Y_4",
        "Y_7 ~ DATA$Y_6 + DATA$Y_5",
        "Y_8 ~ DATA$Y_7 + DATA$Y_6",
        "Y_9 ~ DATA$Y_8 + DATA$Y_7"),as.formula)
  
  
  n.samples <- reactive({as.numeric(input$n.samples)})
  
  m.1 <- reactive({ spDynLM(mods, data=y.t, coords=coords,
                            starting=starting, tuning=tuning, priors=priors(), get.fitted =TRUE,
                            cov.model="exponential", n.samples=n.samples(), n.report=25)
  })
  
  burn.in <- reactive({as.numeric(input$burn.in)})
  quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}
  
  beta <- reactive({apply(m.1()$p.beta.samples[burn.in():n.samples(),], 2, quant)})
  theta <- reactive({apply(m.1()$p.theta.samples[burn.in():n.samples(),], 2, quant)})
  sigma.sq <- reactive({theta[,grep("sigma.sq", colnames(theta))]})
  tau.sq <- reactive({theta[,grep("tau.sq", colnames(theta))]})
  phi <- reactive({theta[,grep("phi", colnames(theta))]})
  
  y.hat <- reactive({apply(m.1()$p.y.samples[,burn.in():n.samples()], 1, quant)})
  y.hat.med <- reactive({matrix(y.hat()[1,], ncol=N.t)})
  y.hat.up <- reactive({matrix(y.hat()[3,], ncol=N.t)})
  y.hat.low <- reactive({matrix(y.hat()[2,], ncol=N.t)})
  
  
  y.obs <- reactive({as.vector(as.matrix(y.t[-county(), -miss()]))})
  y.obs.hat.med <- reactive({as.vector(y.hat.med()[-county(), -miss()])})
  y.obs.hat.up <- reactive({as.vector(y.hat.up()[-county(), -miss()])})
  y.obs.hat.low <- reactive({as.vector(y.hat.low()[-county(), -miss()])})
  
  y.ho <- reactive({as.matrix(y.t.holdout)})
  y.ho.hat.med <- reactive({as.vector(y.hat.med()[county(), miss()])})
  y.ho.hat.up <- reactive({as.vector(y.hat.up()[county(), miss()])})
  y.ho.hat.low <- reactive({as.vector(y.hat.low()[county(), miss()])})
  
  values$nc_t = reactive({cbind(nc, "Y.T" = y.t.all[,as.numeric(input$year)])})
  values$nc_p = reactive({cbind(nc, "Y.HAT" = y.hat.med()[,as.numeric(input$year)])})
  values$nc_h = reactive({y.ho.hat.med()[1]})
  ###########################################################################
  ###########################################################################
  
  output$actualPlot <- renderPlot({
    
    #Plot actual vote diffrences
    plot(values$nc_t()["Y.T"], main = paste0("Actual % Differences in Rep-Dem Voting",
                                           " ",
                                           (2002+ as.numeric(input$year)*2)))
  })
  
  output$predPlot <- renderPlot({
    
    #Plot predicted
    plot(values$nc_p()["Y.HAT"], main = paste0("Predicted % Differences in Rep-Dem Voting",
                                             " ",
                                             (2002+ as.numeric(input$year)*2)))
  })
  
  output$hoPlot <- renderPlot({
    
    barplot( c(y.t.all[as.numeric(input$county),as.numeric(input$year)],
               values$nc_h()), 
             names.arg = c("Actual % Diff", "Pred % Diff"),
             main = paste0("Difference in Actual and Predicted Votes for ",
                    county_lookup[as.numeric(input$county)], " Select County"),
             col = c("purple", "orange")
             )
    
    #Plot holdout county map
    #plot(values$nc_h()["Y.HO.HAT"], main = paste0("Predicted % Differences in Rep-Dem Voting",
    #                                            " ",
    #                                            (2002+ as.numeric(input$year)*2)))
  })

  # Send a pre-rendered image, and don't delete the image after sending it
  output$preImage <- renderImage({
    
    # Return a list containing the filename and alt text
    list(src = "math_images.png")
    
  }, deleteFile = FALSE)
}
# Run the application 
shinyApp(ui = ui, server = server)
