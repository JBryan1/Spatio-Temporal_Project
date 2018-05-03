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
library(sf)
library(spBayes)

load("DATA.RData")
demo(nc, ask = FALSE, echo = FALSE)
nc = st_transform(nc,2264)

# Define UI for application that draws a map of NC with differences in 
# beta coefficients between Democrats and Republicans at different times
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
                       c("LINCOLN"=65,
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
           numericInput("n.samples", "MCMC Samples:", 1000, min = 500, max = 10000),
           
           # Input: Number of burn-in
           numericInput("burnin", "Burn-in:", 200, min = 100, max = 400),
           
           # Input: Beta normal prior mean
           numericInput("beta_mean_prior", "Beta Prior Mean:", 0, min = -10, max = 10),
           
           # Input: Beta normal prior variance
           numericInput("beta_var_prior", "Beta Prior Variance:", 10, min = -5000, max = 5000),
           
           # Input: Phi uniform prior
           numericInput("phi_prior", "Phi Prior:", 0, min = -100, max = 100),
           
           # Input: Variance inv-gamma alpha prior
           numericInput("var_a_prior", "Variance Alpha Prior:", 5, min = -100, max = 100),
           
           # Input: Variance inv-gamma beta prior
           numericInput("var_b_prior", "Variance Beta Prior:", 1, min = -100, max = 100),
           
           # Input: Tau inv-gamma alpha prior
           numericInput("Tau_a_prior", "Tau-Sq Alpha Prior:", 10, min = -100, max = 100),
           
           # Input: Tau-sq inv-gamma beta prior
           numericInput("Tau_b_prior", "Tau-Sq Beta Prior:", 1, min = -100, max = 100),
          
           # Input: Error variance inv-Wishart df prior
           numericInput("Error_df_prior", "Error df Prior:", 1, min = -100, max = 100),
           
           # Input: Error variance inv-Wishart scale prior
           numericInput("Error_scale_prior", "Error Scale Prior:", .001, min = -10, max = 10)
           
         ),
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("mapPlot")
            )
         )
       ),
      
    tabPanel("Inference")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #spBayes model
  ###############################################
  y.t = DATA[,6:12] ; y.t.all = y.t
  N.t = ncol(y.t) ##number of months
  n = nrow(y.t) ##number of observation per months
  
  ##add some missing observations to illistrate prediction
  miss = reactive({input$year + 2}) #account for 9 years in data, we start at 2004 or 3
  y.t.holdout <- reactive({y.t[input$county, miss]})
  y.t[holdout.station.id, miss] <- NA
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
  
  priors <- reactive({list("beta.0.Norm"=list(rep(input$beta_mean_prior,p), diag(input$beta_var_prior,p)),
                 "phi.Unif"=list(rep(input$phi_prior, N.t), rep(input$phi_prior), N.t),
                 "sigma.sq.IG"=list(rep(input$var_a_prior,N.t), rep(input$var_b_prior,N.t)),
                 "tau.sq.IG"=list(rep(input$tau_a_prior,N.t), rep(input$tau_a_prior,N.t)),
                 "sigma.eta.IW"=list(input$error_df_prior, diag(input$error_scale_prior,p))) })
  
  #priors <- list("beta.0.Norm"=list(c(.5,0,0), diag(c(.1,1,1),p)),
  #               "phi.Unif"=list(rep(3/(0.9*max.d), N.t), rep(3/(0.05*max.d), N.t)),
  #               "sigma.sq.IG"=list(rep(5,N.t), rep(1,N.t)),
  #               "tau.sq.IG"=list(rep(10,N.t), rep(1,N.t)),
  #               "sigma.eta.IW"=list(2, diag(0.001,p)))
  
  
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
  
  
  n.samples <- reactive({input$n.samples})
  m.1 <- reactive({ spDynLM(mods, data=y.t, coords=coords,
                 starting=starting, tuning=tuning, priors=priors, get.fitted =TRUE,
                 cov.model="exponential", n.samples=n.samples, n.report=25)
                  })
  
  
  burn.in <- reactive({input$burnin})
  quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}
  
  beta <- apply(m.1$p.beta.samples[burn.in:n.samples,], 2, quant)
  theta <- apply(m.1$p.theta.samples[burn.in:n.samples,], 2, quant)
  sigma.sq <- theta[,grep("sigma.sq", colnames(theta))]
  tau.sq <- theta[,grep("tau.sq", colnames(theta))]
  phi <- theta[,grep("phi", colnames(theta))]
  
  
  
  y.hat <- apply(m.1$p.y.samples[,burn.in:n.samples], 1, quant)
  y.hat.med <- matrix(y.hat[1,], ncol=N.t)
  y.hat.up <- matrix(y.hat[3,], ncol=N.t)
  y.hat.low <- matrix(y.hat[2,], ncol=N.t)
  
  y.obs <- as.vector(as.matrix(y.t[-holdout.station.id, -miss]))
  y.obs.hat.med <- as.vector(y.hat.med[-holdout.station.id, -miss])
  y.obs.hat.up <- as.vector(y.hat.up[-holdout.station.id, -miss])
  y.obs.hat.low <- as.vector(y.hat.low[-holdout.station.id, -miss])
  
  y.ho <- as.matrix(y.t.holdout)
  y.ho.hat.med <- as.vector(y.hat.med[holdout.station.id, miss])
  y.ho.hat.up <- as.vector(y.hat.up[holdout.station.id, miss])
  y.ho.hat.low <- as.vector(y.hat.low[holdout.station.id, miss])
  
  ###########################################################################
  ###########################################################################
  output$mapPlot <- renderPlot({
    
    #Plot actual vote diffrences
    
    #Plot predicted
    #nc[paste0("Y.HAT",year_query)] = y.hat.med[,year_query]
    #plot(nc[paste0("Y.HAT",year_query)], main = paste0("Predicted % Differences in Rep-Dem Voting",
    #                                                (year_query)))
    
    #Plot holdout county map or metric
    plot(hist(y.hat.med[,year_query]))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

