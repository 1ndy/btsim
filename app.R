source(here::here('R/shiny/ui.R'))
source(here::here('R/shiny/server.R'))

# Run the app ----
shinyApp(ui = ui, server = server)

