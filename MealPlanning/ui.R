
library(shiny)
library(sortable)
library(shinyWidgets)
library(DT)
library(plotly)
library(tools)
library(shinythemes)

navbarPage(
  "Meal Planning",   
  theme = shinytheme("sandstone"),
  tabPanel("Recipe Selection", 
           fluidRow(
             tabsetPanel(
               tabPanel('Recipies',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("recipieSearchCat",
                                        "Search Recipies by:",
                                        c("Cuisine", "Ingredients")),
                            conditionalPanel(
                              condition = "input.recipieSearchCat == 'Cuisine'",
                              selectInput("cuisineSelector", "Cusine:", choices = NULL)),
                            conditionalPanel(
                              condition = "input.recipieSearchCat == 'Ingredients'",
                              selectInput("IngredientsSelector", "Ingredients:", choices = NULL))
                            # conditionalPanel(
                            #   condition = "input.recipieSearchCat == 'Random'",
                            #   actionButton("randomSelector", "Random Recipies")
                            # )
                          ),
                          mainPanel(
                            selectInput("recipieSelector", "Select Recipies", choices = NULL, multiple = TRUE),
                            numericInput("servingSize", "Select Serving", 1, min = 1, max = 20),
                            actionButton("addRecipies", "Add")
                          )
                        )
             ),
             tabPanel("Add Recipie",fluidPage(
               imageOutput("ComingSoon1")
                      # textInput("rName", "Recipie Name:"),
                      # textInput("rIngredient", "Ingredients List:"),
                      # textInput("rSteps", "Steps:"),
                      # numericInput("rservingSize", "Select Serving", 1, min = 1, max = 20),
                      # actionButton("rSubmit", 'Submit')
                      )),
             tabPanel("Add Resturant Meal",fluidPage(
               imageOutput("ComingSoon2")
                      # textInput("restMealname", "Meal Name:"),
                      # textInput("restMealDescription", "Describe Meal:"),
                      # actionButton("rmSubmit", 'Submit'))
              
             )
             
             )
           )),
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
               selectInput("macroSelector", "Select Macronutrient",
                           multiple = TRUE,
                           choices =c("Calories", "Fat (g)", "Satuated Fat (g)",
                                      "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                                      "Carbohydrates (g)", "Fiber (g)", "Sugar (g)"),
                           selected = "Calories"),
               checkboxInput("barGraph", "Bar Graph", TRUE),
               checkboxInput("lineGraph", "Line Graph", FALSE),
               conditionalPanel(
                 condition = "input.macroSelector.includes('Calories')",
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
                 sliderInput("weight",
                             "weight:",
                             value = 165,
                             min = 1,
                             max = 500),
                 sliderInput("heightFt", "Height Ft.:", value = 5, min =3, max = 8),
                 sliderInput("heightIn", "Height In.:", value = 0, min = 0, max = 11),
                 selectInput("activityLevel",
                             "Activity Level",
                             choices = c("Sedentary (little or no exercies)" = "sedentary",
                                         "Lightly Active (light exercise/sports 1-3 days per week" = "light", 
                                         "Moderately Active (moderative exercise/sports 3-5 days per week" = "moderate",
                                         "Very Active (hard exercise/sport 6-7 days per week" = "very",
                                         "Extra Active (very hard exerciese/sports plus a physical job or training twice a day)" = "extra")
                 ),
                 uiOutput("BMRexplanation"))
             ),
             mainPanel(titlePanel("Weekly Macro Nutrient Tracker"),
                       plotlyOutput("MacroDOWPlot", height = "1200px"))
           )
           ),
  tabPanel("Recipes",
           sidebarLayout(
             sidebarPanel(width = 2,
                          selectInput("rstepDOW", "Select day of the week:",
                                      choices = c("Sunday" = 'Sun', "Monday" = 'Mon',
                                                  "Tuesday" = 'Tue', "Wednesday" = 'Wed',
                                                  "Thursday" = 'Thur', "Friday" = 'Fri',
                                                  "Saturday" = 'Sat')),
                          uiOutput("recipeSelectUI")
                ),
             mainPanel(
               fluidRow(
                 column(width = 6,
                   uiOutput("Recipieout"),
                   uiOutput("RecipeStep")
                   
                 ),
                 column(width = 6,
                        selectInput("macroSelector2", "Select Macronutrient:",choices =c("Calories", "Fat (g)", "Satuated Fat (g)",
                                                                                         "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                                                                                         "Carbohydrates (g)", "Fiber (g)", "Sugar (g)"),
                                    selected = "Calories"),
                   plotlyOutput("piePlot"),
                   DTOutput("macroDT")
                 )
               )
           )))
  # tabPanel("Test", tableOutput("my_tab")
  #          )
)