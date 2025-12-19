library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("Gen Z Workplace Study: Mental Health Priority"),
  
  mainPanel(
    plotOutput("priorityPlot"),
    tableOutput("summaryTable")
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Reactive expression to read and process data
  processed_data <- reactive({
    # Read TSV and convert to tibble
    df <- read_delim("gen_z_workplace_study.tsv", delim = "\t") %>%
      as_tibble()
    
    # Calculate means by Generation
    df_summary <- df %>%
      group_by(Generation) %>%
      summarize(mean_priority = mean(Mental_Health_Priority, na.rm = TRUE))
    
    return(df_summary)
  })
  
  # Render the Bar Plot
  output("priorityPlot") <- renderPlot({
    ggplot(processed_data(), aes(x = Generation, y = mean_priority, fill = Generation)) +
      geom_col(show.legend = FALSE) +
      theme_minimal() +
      labs(
        title = "Mean Mental Health Priority by Generation",
        y = "Average Priority Score (1-5)",
        x = "Generation"
      ) +
      scale_fill_brewer(palette = "Set2")
  })
  
  # Render a small summary table for verification
  output("summaryTable") <- renderTable({
    processed_data()
  })
}

# Run the Application 
shinyApp(ui = ui, server = server)
