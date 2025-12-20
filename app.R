library(shiny)
library(tidyverse)
library(bslib)

# --- GLOBAL SCOPE ---
# This runs once when the app starts. It's shared by all users.
url <- "https://raw.githubusercontent.com/cca-cce/osm-edu/refs/heads/main/gen_z_workplace_study.tsv"

# Fetch data once at startup
# We use try() to prevent the app from crashing if GitHub is briefly down
data_raw <- try(read_delim(url, delim = "\t", show_col_types = FALSE))

# --- USER INTERFACE ---
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Gen Z Workplace Study: Mental Health Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This study compares how different generations prioritize 
                mental health in the workplace on a scale of 1-5."),
      hr(),
      helpText("Data is fetched live from GitHub.")
    ),
    
    mainPanel(
      # We wrap the plot in a 'withSpinner' logic if you had the shinycssloaders package, 
      # but for now, a standard plot output is fine.
      plotOutput("priorityPlot", height = "500px")
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  output$priorityPlot <- renderPlot({
    # Validate that data was actually loaded
    validate(
      need(is.data.frame(data_raw), "Loading data from GitHub... please refresh if plot doesn't appear.")
    )
    
    data_raw %>%
      group_by(Generation) %>%
      summarize(mean_priority = mean(Mental_Health_Priority, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = Generation, y = mean_priority, fill = Generation)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(mean_priority, 2)), vjust = -0.5, fontface = "bold", size = 5) +
      scale_y_continuous(limits = c(0, 5)) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      labs(
        title = "Mean Mental Health Priority by Generation",
        subtitle = "Higher scores indicate higher priority",
        x = "Generation Group",
        y = "Average Score"
      ) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"),
            panel.grid.major.x = element_blank())
  })
}

# Run the Application 
shinyApp(ui = ui, server = server)
