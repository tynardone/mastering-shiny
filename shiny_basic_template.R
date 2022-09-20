library(shiny)
library(vroom)
library(tidyverse)

ui <- fluidPage(
  "Hello, world!"
)


server <- function(input, output, session) {
  
}

# 127.0.0.1 is a standard address, that means "this computer".
# The next four digits are a randomly assigned port number.
shinyApp(ui, server)