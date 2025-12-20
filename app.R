library(shiny)
library(tidyverse)

# --- Data Preparation (Global) ---
# We define this outside the server function so it loads once when the app starts
url <- "https://raw.githubusercontent.com/cca-cce/osm-edu/refs/heads/main/gen_z_workplace_study.tsv"

# --- User Interface ---
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Gen Z Workplace Study: Mental Health Analysis"),
  
  mainPanel(
    width = 12,
    plotOutput("priorityPlot", height = "500px"),
    hr(),
    helpText("Data source: https://raw.githubusercontent.com/cca-cce/osm-edu/refs/heads/main/gen_z_workplace_study.tsv")
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive expression to fetch and clean data
  dataset <- reactive({
    read_delim(url, delim = "\t", show_col_types = FALSE) %>%
      as_tibble()
  })
  
  # Render the plot
  output$priorityPlot <- renderPlot({
    dataset() %>%
      group_by(Generation) %>%
      summarize(mean_priority = mean(Mental_Health_Priority, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = Generation, y = mean_priority, fill = Generation)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(mean_priority, 2)), vjust = -0.5, fontface = "bold", size = 5) +
      scale_y_continuous(limits = c(0, 5)) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Mean Mental Health Priority by Generation",
        subtitle = "A comparison of values between Gen Z and Older Generations",
        x = "Generation",
        y = "Mean Priority Score (1-5)"
      ) +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"))
  })
}

# Run the Application 
shinyApp(ui = ui, server = server)

