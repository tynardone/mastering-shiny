library(shiny)
library(vroom)
library(tidyverse)


prod_codes <- setNames(products$prod_code, products$title)
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")
injuries <- vroom::vroom("neiss/injuries.tsv.gz")

count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n )) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", 
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
    )
  ),
    column(3, selectInput("y", "Y axis", c("rate", "count"))),
    column(3, numericInput("nrow", label = "Number of rows", value = 5, min=1, max=10))
  ),
fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  table_rows = reactive(input$nrow - 1)
  
  output$diag <- renderTable(count_top(selected(), diag, n = table_rows()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n = table_rows()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = table_rows()), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>% 
        ggplot(aes(age, n, color = sex)) +
        geom_line() + 
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")}
  }, res = 96)
  
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())

}

shinyApp(ui, server)