
library(shiny)
library(sortable)
library(shinyWidgets)
navbarPage(
  "Meal Planning",   
  tabPanel("Recipe Selection", 
           fluidRow(
             tabsetPanel(
               tabPanel('Recipies',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("recipieSearchCat",
                                        "Search Recipies by:",
                                        c("Cuisine", "Ingredients", "Random")),
                            conditionalPanel(
                              condition = "input.recipieSearchCat == 'Cuisine'",
                              selectInput("cuisineSelector", "Cusine:", choices = NULL)),
                            conditionalPanel(
                              condition = "input.recipieSearchCat == 'Ingredients'",
                              selectInput("IngredientsSelector", "Ingredients:", choices = NULL)),
                            conditionalPanel(
                              condition = "input.recipieSearchCat == 'Random'",
                              actionButton("randomSelector", "Random Recipies")
                            )
                          ),
                          mainPanel(
                            selectInput("recipieSelector", "Select Recipies", choices = NULL, multiple = TRUE),
                            actionButton("addRecipies", "Add")
                          )
                        )
             ),
             tabPanel("Add Recipie",
                      textInput("rName", "Recipie Name:"),
                      textInput("rIngredient", "Ingredients List:"),
                      textInput("rSteps", "Steps:"),
                      actionButton("rSubmit", 'Submit')
                      ),
             tabPanel("Add Resturant Meal",
                      textInput("restMealname", "Meal Name:"),
                      textInput("restMealDescription", "Describe Meal:"),
                      actionButton("rmSubmit", 'Submit')
             )
             
             ),
           ),
           fluidRow(
             mainPanel(
               uiOutput("bucket")
             )
           ),
           fluidRow(
             mainPanel(
                actionButton('clearInput', 'Clear Recipies')
               )
           )
           ),
  tabPanel("Daily Nutritional Count", 
           sidebarPanel(
             selectInput('weekDaySelect',
                         "Select Day of the Week",
                         choices = c("All" = 'all', "Sunday" = 'sun', "Monday" = 'mon',
                                     "Tuesday" = 'tue', "Wednesday" = 'wed',
                                     "Thursday" = 'thur', "Friday" = 'fri',
                                     "Saturday" = 'sat'))
             # actionButton("submitWeekDay",
                          # "Submit")
           ),
           mainPanel(
             verbatimTextOutput("text"),
             
             verbatimTextOutput("reactive_output"),
             tableOutput("MealNutTable")
           )),
  tabPanel("Weekly Macro Tracker", verbatimTextOutput("reactive_output")),
  tabPanel("Future Work", tableOutput("my_table"))
  
)