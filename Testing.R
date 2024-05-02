## Example shiny app with bucket list

library(shiny)
library(sortable)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
  ),
  fluidRow(
    column(
      tags$b("Exercise"),
      width = 12,
      bucket_list(
        header = "Drag the items in any desired bucket",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "Drag from here",
          labels = list(
            "one",
            "two",
            "three",
            htmltools::tags$div(
              htmltools::em("Complex"), " html tag without a name"
            ),
            "five" = htmltools::tags$div(
              htmltools::em("Complex"), " html tag with name: 'five'"
            )
          ),
          input_id = "rank_list_1"
        ),
        add_rank_list(
          text = "to here",
          labels = NULL,
          input_id = "rank_list_2"
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$b("Result"),
      column(
        width = 12,
        
        tags$p("input$rank_list_1"),
        verbatimTextOutput("results_1"),
        
        tags$p("input$rank_list_2"),
        verbatimTextOutput("results_2"),
        
        tags$p("input$bucket_list_group"),
        verbatimTextOutput("results_3")
      )
    )
  )
)

server <- function(input, output, session) {
  output$results_1 <-
    renderPrint(
      input$rank_list_1 # This matches the input_id of the first rank list
    )
  output$results_2 <-
    renderPrint(
      input$rank_list_2 # This matches the input_id of the second rank list
    )
  output$results_3 <-
    renderPrint(
      input$bucket_list_group # Matches the group_name of the bucket list
    )
  
}

recipe_names <- c("Banana Pancakes -- 52855.6", "BBQ Pork Sloppy Joes -- 52995.7", "Beef Brisket Pot Roast -- 52812.8", "15-minute chicken & halloumi burgers -- 53085.9")

# Extract numbers after the dot (.) in each element of the vector
numbers_after_dot <- gsub(".*\\.(\\d+)$", "\\1", recipe_names)
numbers_after_dot
# Convert the extracted numbers to numeric format
numbers_after_dot_numeric <- as.numeric(numbers_after_dot)

# Print the result
print(numbers_after_dot_numeric)
shinyApp(ui, server)
library(tidyverse)
df <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Value = c("A", "B", "C", "D", "E")
)

# Vector of values to filter
filter_values <- c("B", "D")

# Filtering the dataframe
filtered_df <- df %>%
  filter(Value %in% filter_values)

print(filtered_df)



df <- data.frame(
    name = rep(1, 5),
    calories = rep(2, 5),
    serving_size_g = rep(3, 5),
    fat_total_g = rep(3, 5),
    fat_saturated_g = rep(4, 5),
    protein_g = rep(5, 5),
    sodium_mg = rep(6, 5),
    potassium_mg = rep(7, 5),
    cholesterol_mg = rep(8, 5),
    carbohydrates_total_g = rep(9, 5),
    fiber_g = rep(20, 5),
    sugar_g = rep(11, 5),
    RID = rep(12, 5),
    ID = c("1","6","2","2","3")
  )

df[df[, 14] %in% gsub(".*\\.(\\d+)$", "\\1",recipe_names), ]

df[df[, 14] %in% c("1", "3"), ]

data.frame(
  name = NULL,
  calories = NULL,
  serving_size_g = NULL,
  fat_total_g = NULL,
  fat_saturated_g = NULL,
  protein_g = NULL,
  sodium_mg = NULL,
  potassium_mg = NULL,
  cholesterol_mg = NULL,
  carbohydrates_total_g = NULL,
  fiber_g = NULL,
  sugar_g = NULL,
  RID = NULL,
  ID = NULL
)

df2 <-data.frame()
df2$col1 <- NULL
column_names <- c("col1", "col2", "col3")

# Create an empty dataframe with columns but no rows
empty_df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(empty_df) <- column_names

