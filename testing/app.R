library(shiny)
library(dplyr)

# Define UI
ui <- fluidPage(
  selectInput("column", "Select column", choices = c("calories", "fat", "sugar")),
  tableOutput("result")
)

# Define server logic
server <- function(input, output) {
  # Your dataframe
  df <- data.frame(
    food_item = c("Chicken", "Beef", "Pork", "Salmon", "Tofu", "Rice", "Beans", "Broccoli", "Carrots", "Spinach"),
    calories = c(100, 200, 150, 180, 50, 120, 80, 30, 20, 10),
    fat = c(5, 10, 8, 12, 3, 1, 2, 0, 0, 1),
    sugar = c(0, 0, 0, 0, 0, 0, 0, 1, 2, 0)
  )
  
  # Function to get top n rows based on a column
  get_top_n <- function(df, col_name, n) {
    df %>% 
      arrange(desc({{col_name}})) %>% 
      slice_head(n = n)
  }
  
  # Render output
  output$result <- renderTable({
    # Get selected column from input
    column_selected <- input$column
    
    # Get top 5 values for selected column
    top_5_values <- get_top_n(df, !!sym(column_selected), 5)
    
    # Calculate the sum of remaining values for selected column
    column_remainder <- sum(df[[column_selected]]) - sum(top_5_values[[column_selected]])
    
    # Add a new row 'other' with the sum of remaining values
    other_row <- c("Other", column_remainder, NA, NA)
    df_with_other <- rbind(top_5_values, other_row)
    
    # Return the resulting dataframe
    df_with_other
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

