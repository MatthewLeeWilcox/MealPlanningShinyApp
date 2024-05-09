
library(shiny)
library(sortable)
library(shinyWidgets)
library(DT)
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
                            numericInput("servingSize", "Select Serving", 1, min = 1, max = 20),
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
           mainPanel(
             selectInput('weekDaySelect', 
                         "Select Day of the Week",
                         choices = c("Sunday" = 'sun', "Monday" = 'mon',
                                     "Tuesday" = 'tue', "Wednesday" = 'wed',
                                     "Thursday" = 'thur', "Friday" = 'fri',
                                     "Saturday" = 'sat')),
             DTOutput("nutTable")
           )),
  tabPanel("Weekly Macro Tracker",
           sidebarLayout(
             sidebarPanel(width = 2,
               selectInput("macroSelector", "Select Macronutrient",choices = c('Calories', "Protein")),
               checkboxGroupInput("graphType", "Select Graph Type:",choices = c("Bar", "Line")),
               conditionalPanel(
                 condition = "input.macroSelector == 'Calories'",
                 checkboxInput("bmrSelector","Add Basal Metabolic Rate", value = FALSE)
                 ),
               conditionalPanel(
                 condition = "input.bmrSelector == true",
                 titlePanel("BMR Calculator"),
                 selectInput("gender", "Gender:", choices = c("Male", "Female")),
                 sliderInput("age",
                             "Age:",
                             value = 25,
                             min = 1,
                             max = 99),
                 selectInput("heightFt", "Height Ft.:", choices = c("3ft", "4ft", "5ft", "6ft", "7ft", "8ft")),
                 selectInput("heightIn", "Height In.:", choices = c("1in", "2in",
                                                                    "3in", "4in",
                                                                    "5in", "6in",
                                                                    "7in", "8in",
                                                                    "9in", "10in",
                                                                    "11in")),
                 selectInput("activityLevel",
                             "Activity Level",
                             choices = c("Sedentary (little or no exercies)" = "sedentary",
                                         "Lightly Active (light exercise/sports 1-3 days per week" = "light", 
                                         "Moderately Active (moderative exercise/sports 3-5 days per week" = "moderate",
                                         "Very Active (hard exercise/sport 6-7 days per week" = "very",
                                         "Extra Active (very hard exerciese/sports plus a physical job or training twice a day)" = "extra")
                 ))
             ),
             mainPanel()
           )
           ),
  tabPanel("Future Work", tableOutput("my_table"),
           tableOutput(("my_table_1")))
  
)



























