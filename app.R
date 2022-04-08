source("ui.R", local=TRUE)
source("server.R", local=TRUE)

# Run the app ----
shinyApp(ui = ui, server = server)
