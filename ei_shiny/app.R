library(shiny)
library(tidyverse)

 ui <- fluidPage(
  
  # App title ----
  titlePanel("E/I Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
        h3("Modern"),
        sliderInput(inputId = "rh_mod",
                  label = "Relative Humidity (%)",
                  min = 0,
                  max = 100,
                  value = 65),
      
        sliderInput(inputId = "dI_mod",
                  label = "Precip d18O (per mil)",
                  min = -30,
                  max = 10,
                  value = -21),
      
        sliderInput(inputId = "temp_mod",
                  label = "Temperature (C)",
                  min = -30,
                  max = 30,
                  value = 5),
      
        numericInput("num", 
                   h5("Calcite d18O"),
                   value = -17),
      
        h3("Paleo"),
        
        sliderInput(inputId = "rh_pal",
                    label = "Relative Humidity (%)",
                    min = 0,
                    max = 100,
                    value = 65),
        
        sliderInput(inputId = "dI_pal",
                    label = "Precip d18O (per mil)",
                    min = -30,
                    max = 10,
                    value = -21),
        
        sliderInput(inputId = "temp_pal",
                    label = "Temperature (C)",
                    min = -30,
                    max = 30,
                    value = 5),
        
        numericInput("num2", 
                   h5("Calcite d18O"),
                   value = -19)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "E_I")
      
    )
  )
)

# Define server logic----
server <- function(input, output) {
  
  # This expression that generates a model of E/I wrapped in a call

  output$E_I <- renderPlot({
    
    # convert T input in C to K
    rh2 <- input$rh_mod / 100
    rh_p <- input$rh_pal / 100
    K <- input$temp_mod + 273.15
    K_pal <- input$temp_pal + 273.15
    # Horita and Wes, 1994 eqs
    aO18 <- exp((-7.685 + 6.7123 * (10^3/K) - (1.6664*(10^6/K^2)) + (0.35041*(10^9/K^3))) / 1000)
    aO18_pal <- exp((-7.685 + 6.7123 * (10^3/K_pal) - (1.6664*(10^6/K_pal^2)) + (0.35041*(10^9/K_pal^3))) / 1000)
    eStar <- (aO18 - 1) * 1000
    eStar_pal <- (aO18_pal - 1) * 1000
    dA <- (input$dI_mod - 0.5*eStar)/(1+10^-3*0.5*eStar)
    dA_pal <- (input$dI_pal - 0.5*eStar_pal)/(1+10^-3*0.5*eStar_pal)
    eK <- (14.2*(1-rh2))
    eK_pal <- (14.2*(1-rh_p))
    enrich_slope <- (rh2-10^-3*(eK+eStar/aO18))/(1-rh2+10^-3*eK)
    enrich_slope_pal <- (rh_p-10^-3*(eK_pal+eStar_pal/aO18_pal))/(1-rh_p+10^-3*eK_pal)
    lim_enrich <- (rh2 * dA + eK + eStar/aO18)/(rh2 - 0.001*(eK + eStar/aO18))
    lim_enrich_pal <- (rh_p * dA_pal + eK_pal + eStar_pal/aO18_pal)/(rh_p - 0.001*(eK_pal + eStar_pal/aO18_pal))
    aStar <- 1 + eStar
    e <- eStar + eK
    
    ei_list <- seq(from = 0, to = 1, by = 0.01)
    
    mod_EI_mod <- (input$dI_mod - input$num) / (enrich_slope * (input$num - lim_enrich))
    mod_EI_paleo <- (input$dI_pal - input$num2) / (enrich_slope_pal * (input$num2 - lim_enrich_pal))
    
    st_state_out_mod <- function(ei_list = ei_list) {
      out <- (ei_list*enrich_slope*lim_enrich+input$dI_mod)/(1+ei_list*enrich_slope)
    }
    st_state_out_pal <- function(ei_list = ei_list) {
      out <- (ei_list*enrich_slope_pal*lim_enrich_pal+input$dI_pal)/(1+ei_list*enrich_slope_pal)
    }
    
    dL_out_mod <- sapply(ei_list, st_state_out_mod)
    ei_df_mod <- data.frame("dL" = dL_out_mod, "e.i" = ei_list)
    dL_out_pal <- sapply(ei_list, st_state_out_pal)
    ei_df_pal <- data.frame("dL_pal" = dL_out_pal, "e.i_pal" = ei_list)

    
    ggplot(ei_df_mod, aes(dL, e.i)) +
      geom_line(col = 4, size = 1) +
      geom_line(data = ei_df_pal, aes(dL_pal, e.i_pal), col = 2) +
      geom_vline(xintercept = input$num) +
      geom_vline(xintercept = input$num2, linetype = 2) +
      geom_point(aes(x = input$num, y = mod_EI_mod), col = 4, size = 4) + 
      geom_point(aes(x = input$num2, y = mod_EI_paleo), col = 2, size = 4) +
      annotate(geom = "text", label = 'modern calcite d18O', x = (input$num+0.3), y = 0.5, angle = -90) +
      annotate(geom = "text", label = 'paleo calcite d18O', x = (input$num2+0.3), y = 0.5, angle = -90) +
      labs(x = "lake water d18O", y = "E/I") +
      scale_y_continuous(limits = c(0,1), breaks = c(seq(0,1,0.1))) +
      scale_x_continuous(breaks = c(seq(min(c(ei_df_mod$dL, ei_df_pal$dL_pal),max(c(ei_df_mod$dL, ei_df_pal$dL_pal))),1))) +
      theme_bw()
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
