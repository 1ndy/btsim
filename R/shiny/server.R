library(shiny)

source(here::here('R/sim.R'))

# Define server logic ----
server <- function(input, output) {
  sim <- eventReactive(input$simulate, {
    simulate(input$n_generations,
         input$n_insects,
         input$resistant_allele_freq,
         input$refuge_crop_percentage,
         input$bushels_per_acre,
         input$acreage,
         input$plant_density,
         input$h_val,
         input$s_val,
         input$selection_mode
    )
  })

  output$census_v_generations <- renderPlot({
    df=sim()
    plot(df$generation, df$n_pests,
         main="Generations vs Census Size",
         xlab="Generations",
         ylab="Census Size",
         lwd=2.0,
         type="l")
  })

  output$raf_v_generations <- renderPlot({
    df=sim()
    plot(df$generation, df$res_allele_freq,
         main="Generations vs Resistant Allele Frequency",
         xlab="Generations",
         ylab="Resistant Allele Frequency",
         lwd=2.0,
         type="l")
  })

  output$bpa_v_generations <- renderPlot({
    df=sim()
    plot(df$generation, df$bushels_per_acre,
         main="Generations vs Bushels per Acre",
         xlab="Generations",
         ylab="Bushels per Acre",
         lwd=2.0,
         type="l")
  })

  output$pest_density_v_generations <- renderPlot({
    df=sim()
    plot(df$generation, df$pest_density,
         main="Generations vs Pest Density",
         xlab="Generations",
         ylab="Pest Density",
         lwd=2.0,
         type="l")
  })

  output$sim_data <- renderDataTable(sim())

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("bt_simulation-",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".csv")
    },
    content = function(file) {
      df = sim()
      write.csv(df, file)
    }

  )
}
