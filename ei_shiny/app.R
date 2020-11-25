library(shiny)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("E/I Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "rh",
                  label = "Relative Humidity",
                  min = 0,
                  max = 1,
                  value = 0.65),
      
      sliderInput(inputId = "dI",
                  label = "Precip d18O",
                  min = -30,
                  max = 10,
                  value = -18),
      
      sliderInput(inputId = "temp",
                  label = "Temperature",
                  min = -30,
                  max = 30,
                  value = 5)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "E_I")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$E_I <- renderPlot({
    
    #convert T input in C to K
    K <- input$temp + 273.15
    # Horita and Wes, 1994 eqs
    aO18 <- exp((-7.685 + 6.7123 * (10^3/K) - (1.6664*(10^6/K^2)) + (0.35041*(10^9/K^3))) / 1000)
    eStar <- (aO18 - 1) * 1000
    dA <- (input$dI - 0.5*eStar)/(1+10^-3*0.5*eStar)
    eK <- (14.2*(1-input$rh))
    enrich_slope <- (input$rh-10^-3*(eK+eStar/aO18))/(1-input$rh+10^-3*eK)
    lim_enrich <- (input$rh * dA + eK + eStar/aO18)/(input$rh - 0.001*(eK + eStar/aO18))
    aStar <- 1 + eStar
    e <- eStar + eK
    
    ei_list <- seq(from = 0, to = 1, by = 0.01)
    
    st_state_out <- function(ei_list = ei_list) {
      out <- (ei_list*enrich_slope*lim_enrich+input$dI)/(1+ei_list*enrich_slope)
    }
    
    dL_out <- sapply(ei_list, st_state_out)
    ei_df <- data.frame("dL" = dL_out, "e.i" = ei_list)
    
    
    ggplot(ei_df, aes(dL, e.i)) +
      geom_point() +
      geom_line() +
      labs(x = "lake water d18O", y = "E/I") +
      theme_bw()
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
