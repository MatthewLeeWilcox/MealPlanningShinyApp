
library(shiny)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(tools)


function(input, output, session) {
  
  # Recipie Search lists for Selectors
  cuisineList <- fromJSON("https://www.themealdb.com/api/json/v1/1/list.php?a=list")$meals[[1]]
  ingredientList <-fromJSON("https://www.themealdb.com/api/json/v1/1/list.php?i=list")$meals[[2]]
  
  # Recipie Search Selectors
  updateSelectInput(session,
                    "cuisineSelector",
                    choices = cuisineList
  )
  updateSelectInput(session,
                    "IngredientsSelector",
                    choices = ingredientList
  )
  
  recipies_list <- reactive({
    if(input$recipieSearchCat == 'Cuisine'){
      api_pull_data <- fromJSON(paste0("https://www.themealdb.com/api/json/v1/1/filter.php?a=", input$cuisineSelector))
      setNames(api_pull_data$meals[[3]], api_pull_data$meals[[1]])
      
    } else {
      api_pull_data <- fromJSON(paste0("https://www.themealdb.com/api/json/v1/1/filter.php?i=", input$IngredientSelector))
      setNames(api_pull_data$meals[[3]],  api_pull_data$meals[[1]])
    }
  })

  observeEvent(c(input$cuisineSelector, input$IngredientsSelector),{
    updateSelectInput(session,
                      "recipieSelector",
                      choices = recipies_list()
    )
  })
  
#################################################################
# Function to Process the meal api pull
  removeEnd1 <- function(input_str){
    input_str <- input_str
    cleaned_string <- gsub(" , 1", "", input_str)
    lastChar <- substring(cleaned_string, nchar(cleaned_string)-1, nchar(cleaned_string))
    if (substring(cleaned_string, nchar(cleaned_string), nchar(cleaned_string)) == "1") {
      # Remove the last character
      cleaned_string <- substr(cleaned_string, 1, nchar(cleaned_string) - 1)
    } else {
      cleaned_string <- cleaned_string
    }
    cleaned_string
    
  }
  
  removeEndCommas <- function(input_str){
    input_str <- input_str
    cleaned_string <- gsub(" ,", "", input_str)
    lastChar <- substring(cleaned_string, nchar(cleaned_string)-1, nchar(cleaned_string))
    if (substring(cleaned_string, nchar(cleaned_string), nchar(cleaned_string)) == "1") {
      # Remove the last character
      cleaned_string <- substr(cleaned_string, 1, nchar(cleaned_string) - 1)
    } else {
      cleaned_string <- cleaned_string
    }
    cleaned_string
    
  }
  
  removeEndCommas2 <- function(input_str){
    input_str <- input_str
    cleaned_string <- gsub("  ,", "", input_str)
    lastChar <- substring(cleaned_string, nchar(cleaned_string)-1, nchar(cleaned_string))
    if (substring(cleaned_string, nchar(cleaned_string), nchar(cleaned_string)) == "1") {
      # Remove the last character
      cleaned_string <- substr(cleaned_string, 1, nchar(cleaned_string) - 1)
    } else {
      cleaned_string <- cleaned_string
    }
    cleaned_string
    
  }
  
  removeEndones <- function(input_str){
    cleaned_string <- input_str
    if (substring(cleaned_string, nchar(cleaned_string), nchar(cleaned_string)) == "1") {
      # Remove the last character
      cleaned_string <- substr(cleaned_string, 1, nchar(cleaned_string) - 1)
    } else {
      cleaned_string <- cleaned_string
    }
    cleaned_string
    
  }
  
  removeLastComma <- function (input_str){
    cleaned_string <- input_str
    if (substring(cleaned_string, nchar(cleaned_string), nchar(cleaned_string)) == ",") {
      # Remove the last character
      cleaned_string <- substr(cleaned_string, 1, nchar(cleaned_string) - 1)
    } else {
      cleaned_string <- cleaned_string
    }
    cleaned_string
    
  }
  
  
  process_meal <- function(url){
    x <-fromJSON(url)
    
    x2 <- x$meals
    
    df_mask_measure <- grep("^strMeasure", names(x2), value = TRUE)
    df_mask_ingredient <- grep("^strIngredient", names(x2), value = TRUE)
    
    measurements <- tolower(t(x2[df_mask_measure]))
    ingredients <- t(x2[df_mask_ingredient])
    measurements[measurements == " "] <- 1
    
    
    y <-paste(measurements,ingredients)
    filtered <- subset(y, y != " ")
    filtered <- subset(filtered, y != "")
    filtered <- filtered[!sapply(filtered,is.na)]
    resultIngredients <- paste(filtered, collapse = ", ")
    ingredientsCleaned <- gsub("NA", "", resultIngredients)
    ingredientsCleaned <- gsub("dash", "1g", ingredientsCleaned)
    ingredientsCleaned <- gsub("pinch", "1g", ingredientsCleaned)
    ingredientsCleaned <- gsub("sprinkle", "1g", ingredientsCleaned)
    ingredientsCleaned <- gsub("1/2 tsp", "1g", ingredientsCleaned)
    ingredientsCleaned <- gsub("1/4 tsp", "1g", ingredientsCleaned)
    ingredientsCleaned <- gsub("1/8 tsp", "1g", ingredientsCleaned)
    
    result_df <- data.frame(
      id = x2$idMeal,
      name = x2$strMeal,
      ingredients = removeLastComma(removeEndCommas2(trimws(removeEndones(trimws(removeEnd1(ingredientsCleaned)))))), #removeLastComma(trimws(removeEndCommas(ingredientsCleaned))),
      steps = x2$strInstructions
    )
    result_df
  }
  
  
##################################################################

recipe_df <- reactiveVal({
  data.frame(
    id = NULL,
    id2 =  NULL,
    name =  NULL,
    servings =  NULL,
    ingredients = NULL,
    steps =  NULL
  )
})

bucket_recipe_list <- reactiveVal(
  c()
)
sunday_list <- reactiveVal(
  c()
)
monday_list <- reactiveVal(
  c()
)
tuesday_list <- reactiveVal(
  c()
)
wednesday_list <- reactiveVal(
  c()
)
thursday_list <- reactiveVal(
  c()
)
friday_list <- reactiveVal(
  c()
)
saturday_list <- reactiveVal(
  c()
)
################################################################################
# Meal Macro Conts
################################################################################

getNutritionFacts <- function(recipe_item_code, input_api_key, input_query){
  api_url <- "https://api.calorieninjas.com/v1/nutrition?query="
  print("###############################")
  print(input_query)
  print("###############################")
  
  response <- GET(paste0(api_url, URLencode(input_query)), add_headers('X-Api-Key' = input_api_key))
  temp.df <- fromJSON(content(response, "text"))$items
  temp.df$RID <- as.character(recipe_item_code)
  temp.df  
}


macrosDf <- reactiveVal({
  data.frame(
    name = character(0),
    calories = numeric(0),
    serving_size_g = numeric(0),
    fat_total_g = numeric(0),
    fat_saturated_g = numeric(0),
    protein_g = numeric(0),
    sodium_mg = numeric(0),
    potassium_mg = numeric(0),
    cholesterol_mg = numeric(0),
    carbohydrates_total_g = numeric(0),
    fiber_g = numeric(0),
    sugar_g = numeric(0),
    RID = character(0),
    ID = character(0)
  )
})
Sum.macrosDf <- reactiveVal({
  data.frame(
    name = character(0),
    sum_calories = numeric(0),
    sum_fat = numeric(0),
    sum_sat = numeric(0),
    sum_proteing = numeric(0),
    sum_sodium = numeric(0),
    sum_potassium =numeric(0) ,
    sum_carbohydrates = numeric(0),
    sum_fiber = numeric(0),
    sum_sugar = numeric(0),
    ID = character(0)
  )
})
################################################################################

api_key <- "QAM21lUsGekFQHsi6lktYg==fwz7Orddmt1IZLYu"


# Add Recipe data
global_macro_input_counter <- reactiveVal({0})

observeEvent(c(input$addRecipies),{
  recipe_names <- c(input$rank_list_1)
  for (selectedrecipie in input$recipieSelector){
    url <- paste0("https://www.themealdb.com/api/json/v1/1/lookup.php?i=",selectedrecipie)
    meal_df <- process_meal(url)
    meal_df['id2'] <- global_macro_input_counter()
    meal_df['sevings'] <- input$servingSize
    recipe_df(rbind(recipe_df(),meal_df))
    print(meal_df[2])
    print(meal_df[1])
    recipe_names <- c(recipe_names, paste0(meal_df[2],"   - ", meal_df[1],".",as.character(global_macro_input_counter())))
    
    ingredientdf <- getNutritionFacts(meal_df[1], "QAM21lUsGekFQHsi6lktYg==fwz7Orddmt1IZLYu", meal_df[3])
    ingredientdf['ID'] <- global_macro_input_counter()
    ingredientdf[,c("calories",
                    "fat_total_g",
                    "fat_saturated_g",
                    "protein_g",
                    "sodium_mg",
                    "potassium_mg",
                    "carbohydrates_total_g",
                    "fiber_g", 
                    "sugar_g")] <- ingredientdf[,c("calories",
                                                   "fat_total_g",
                                                   "fat_saturated_g",
                                                   "protein_g",
                                                   "sodium_mg",
                                                   "potassium_mg",
                                                   "carbohydrates_total_g",
                                                   "fiber_g", 
                                                   "sugar_g")]/input$servingSize
    macrosDf(rbind(macrosDf(), ingredientdf))
    ingredientdf <- ingredientdf %>%
      group_by(ID) %>%
      summarise(sum_calories = sum(calories),
                sum_fat = sum(fat_total_g),
                sum_sat = sum(fat_saturated_g),
                sum_proteing = sum(protein_g),
                sum_sodium = sum(sodium_mg),
                sum_potassium = sum(potassium_mg),
                sum_carbohydrates = sum(carbohydrates_total_g),
                sum_fiber = sum(fiber_g),
                sum_sugar = sum(sugar_g))
    ingredientdf['name']<- meal_df[2]
    Sum.macrosDf(rbind(Sum.macrosDf(), ingredientdf))
    global_macro_input_counter(global_macro_input_counter()+1)
  }
  
  
  sunday_list(input$rank_list_2)
  monday_list(input$rank_list_3)
  tuesday_list(input$rank_list_4)
  wednesday_list(input$rank_list_5)
  thursday_list(input$rank_list_6)
  friday_list(input$rank_list_7)
  saturday_list(input$rank_list_8)
  bucket_recipe_list(recipe_names)
  
  
    updateSelectInput(session,
                      "recipieSelector",
                      choices = recipies_list(),
                      selected = NULL)
  })

################################################################################
# Add Our Own Recipe
################################################################################
output$ComingSoon1 <- renderImage({
  # Return a list containing the filename
  list(src = "images/27282.jpg",  # Path to your image file
       contentType = 'image/jpeg',
       width = 400,
       height = 300,
       alt = "Alternate text for the image")
}, deleteFile = FALSE)

output$ComingSoon2 <- renderImage({
  # Return a list containing the filename
  list(src = "images/27282.jpg",  # Path to your image file
       contentType = 'image/jpeg',
       width = 400,
       height = 300,
       alt = "Alternate text for the image")
}, deleteFile = FALSE)
# global_recipie_coutner <-reactiveVal(1)
# 
# observeEvent(c(input$rSubmit),{
#   recipe_names <- c(input$rank_list_1)
#   
#   custom_id <-paste0(global_recipie_coutner(), ".",global_macro_input_counter())
#   custom_recipe <- data.frame(
#     id = global_recipie_coutner(),
#     name = input$rName,
#     ingredients = input$rIngredient,
#     steps = input$rSteps,
#     id2 = global_macro_input_counter(),
#     sevings = input$rservingSize
# 
#   )
#   recipe_df(rbind(recipe_df(),custom_recipe))
#   recipe_names <- c(recipe_names, paste0(input$rname,"   - ", as.character(global_recipie_coutner()),".",as.character(global_macro_input_counter())))
# 
#   ingredientdf <- getNutritionFacts(global_recipie_coutner(), "QAM21lUsGekFQHsi6lktYg==fwz7Orddmt1IZLYu", global_macro_input_counter)
#   ingredientdf['ID'] <- global_macro_input_counter()
#   ingredientdf[,c("calories",
#                   "fat_total_g",
#                   "fat_saturated_g",
#                   "protein_g",
#                   "sodium_mg",
#                   "potassium_mg",
#                   "carbohydrates_total_g",
#                   "fiber_g",
#                   "sugar_g")] <- ingredientdf[,c("calories",
#                                                  "fat_total_g",
#                                                  "fat_saturated_g",
#                                                  "protein_g",
#                                                  "sodium_mg",
#                                                  "potassium_mg",
#                                                  "carbohydrates_total_g",
#                                                  "fiber_g",
#                                                  "sugar_g")]/input$servingSize
#   macrosDf(rbind(macrosDf(), ingredientdf))
#   ingredientdf <- ingredientdf %>%
#     group_by(ID) %>%
#     summarise(sum_calories = sum(calories),
#               sum_fat = sum(fat_total_g),
#               sum_sat = sum(fat_saturated_g),
#               sum_proteing = sum(protein_g),
#               sum_sodium = sum(sodium_mg),
#               sum_potassium = sum(potassium_mg),
#               sum_carbohydrates = sum(carbohydrates_total_g),
#               sum_fiber = sum(fiber_g),
#               sum_sugar = sum(sugar_g))
#   ingredientdf['name']<- meal_df[2]
#   Sum.macrosDf(rbind(Sum.macrosDf(), ingredientdf))
#   global_macro_input_counter(global_macro_input_counter()+1)
#   global_recipie_coutner(global_recipie_coutner()+1)
# #
# 
# 
#   # Clear Fields
#   updateTextInput(session,
#                   'rName',
#                   value = '')
#   updateTextInput(session,
#                   'rIngredient',
#                   value = '')
#   updateTextInput(session,
#                   'rSteps',
#                   value = '')
# })

################################################################################
# Add Resturant meal //// COMING SOON!!!!1
################################################################################
# global_rest_coutner <-reactiveVal(1)
# observeEvent(c(input$rSubmit),{
#   custom_id <-paste0("RT", global_rest_coutner())
#   custom_recipe <- data.frame(
#     id = custom_id,
#     name = input$rName,
#     ingredients = input$rIngredient,
#     steps = input$rSteps
#   )
#   global_recipie_coutner(global_recipie_coutner()+1)
#   recipe_df(rbind(recipe_df(),custom_recipe))
# 
#   
#   
#   # Clear Fields
#   updateTextInput(session,
#                   'rName',
#                   value = '')
#   updateTextInput(session,
#                   'rIngredient',
#                   value = '')
#   updateTextInput(session,
#                   'rSteps',
#                   value = '')
# })

################################################################################
# 2nd Fluid Row Bucket Output
################################################################################

output$bucket <- renderUI({
  bucket_list(
    header = "Drag Recipies to Day of the Week",
    group_name = "bucket_list_group",
    orientation = "horizontal",
    add_rank_list(
      text = "Recipies",
      labels = bucket_recipe_list() ,
      input_id = "rank_list_1"
    ),
    add_rank_list(
      text = "Sunday",
      labels = sunday_list(),
      input_id = "rank_list_2"
    ),
    add_rank_list(
      text = "Monday",
      labels = monday_list(),
      input_id = "rank_list_3"
    ),
    add_rank_list(
      text = "Tuesday",
      labels = tuesday_list(),
      input_id = "rank_list_4"
    ),
    add_rank_list(
      text = "Wednesday",
      labels = wednesday_list(),
      input_id = "rank_list_5"
    ),
    add_rank_list(
      text = "Thursday",
      labels = thursday_list(),
      input_id = "rank_list_6"
    ),
    add_rank_list(
      text = "Friday",
      labels = friday_list(),
      input_id = "rank_list_7"
    ),
    add_rank_list(
      text = "Saturday",
      labels = saturday_list(),
      input_id = "rank_list_8"
    ),
    add_rank_list(
      text = "Remove Recipe",
      labels = NULL,
      input_id = "rank_list_9",
      options = sortable_options(
        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
      )
    )
  )
})  

output$nutTable <- renderDT({
    df <- Sum.macrosDf()
    df$ID <- as.character(df$ID)
    print(str(df))
    print(str(input$rank_list_2))
    if(input$weekDaySelect == 'sun'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_2))
    } else if(input$weekDaySelect == 'mon'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_3))
    }else if(input$weekDaySelect == 'tue'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_4))
    }else if(input$weekDaySelect == 'wed'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_5))
    }else if(input$weekDaySelect == 'thur'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_6))
    }else if(input$weekDaySelect == 'fri'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_7))
    }else if(input$weekDaySelect == 'sat'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_8))
    } else if(input$weekDaySelect == 'all'){
      df <- df %>%
        filter(ID %in% sub(".*\\.(\\d+)$", "\\1",c(input$rank_list_2,
                                                   input$rank_list_3,
                                                   input$rank_list_4,
                                                   input$rank_list_5,
                                                   input$rank_list_6,
                                                   input$rank_list_7,
                                                   input$rank_list_8
        )))
    }
    df <- df %>% 
      select("name", "sum_calories","sum_fat", "sum_sat", "sum_proteing", 
             "sum_sodium", "sum_potassium", "sum_carbohydrates", "sum_fiber",
             "sum_sugar")
    colnames(df) <- c("Name", "Calories", "Fat (g)", "Satuated Fat (g)",
                      "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                      "Carbohydrates (g)", "Fiber (g)", "Sugar (g)")
    datatable(df)
    })



output$MacroDOWTable <- renderTable({
  df <- Sum.macrosDf() %>%
    mutate(weekDay = case_when(
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_2) ~ "Sun",
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_3) ~ "Mon",
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_4) ~ "Tue",
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_5) ~ "Wed",
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_6) ~ "Thur",
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_7) ~ "Fri",
      ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_8) ~ "Sat"
    ))  %>%
    group_by(weekDay) %>%
    summarise(sum_calories = sum(sum_calories),
              sum_fat = sum(sum_fat),
              sum_sat = sum(sum_sat),
              sum_proteing = sum(sum_proteing),
              sum_sodium = sum(sum_sodium),
              sum_potassium = sum(sum_potassium),
              sum_carbohydrates = sum(sum_carbohydrates),
              sum_fiber = sum(sum_fiber),
              sum_sugar = sum(sum_sugar))
  colnames(df) <- c("Day of the Week", "Calories", "Fat (g)", "Satuated Fat (g)",
                   "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                   "Carbohydrates (g)", "Fiber (g)", "Sugar (g)")
  df
  
  
})

bmrReact <- reactiveVal(0)

observeEvent(c(input$bmrSelector,
               input$gender,
               input$weight,
               input$heightFt,
               input$heightIn,
               input$age,
               input$activityLevel),{

                 if(input$gender == "Male"){
                   bmr <- 66.47 +  (6.24 * as.numeric(input$weight)) + (12.7 *(as.numeric(input$heightFt)*12 +as.numeric(input$heightIn))) - (6.755*as.numeric(input$age))
                   # bmr <- 6
                   print(bmr)
                   
                   } else {
                   bmr <- 655.1 +  (4.35 * as.numeric(input$weight)) + (4.7 *(as.numeric(input$heightFt)*12 +as.numeric(input$heightIn))) - (4.7*as.numeric(input$age))
                 }

                 if(input$activityLevel == 'sedentary' ){
                   bmr <- bmr *1.2
                 }
                 if(input$activityLevel == 'light' ){
                   bmr <- bmr *1.375
                 }
                 if(input$activityLevel == 'moderate' ){
                   bmr <- bmr *1.55
                 }
                 if(input$activityLevel == 'very' ){
                   bmr <- bmr *1.725
                 }
                 if(input$activityLevel == 'extra' ){
                   bmr <- bmr *1.9
                 }

                 bmrReact(bmr)

                 print(bmr)

               })

output$BMRexplanation <- renderUI({
  HTML("<p>BMR, or Basal metabolic rate, is the rate of energy you expend at rest. It can also be manipulated with the amount of physical activity to approximate the overall caloric expenditure of someone. To maintain your weight, you should aim to match your caloric intake with your BMR. if you are trying to lose weight, you should eat less than your BMR, and to gain weight eat more than. However, it has also been shown that a BMR calculator such as this one frequently can differ from one’s true BMR by 10%.</p>")
})

output$MacroDOWPlot <- renderPlotly({
    df <- Sum.macrosDf() %>%
      mutate(weekDay = case_when(
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_2) ~ "Sun",
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_3) ~ "Mon",
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_4) ~ "Tue",
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_5) ~ "Wed",
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_6) ~ "Thur",
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_7) ~ "Fri",
        ID %in% sub(".*\\.(\\d+)$", "\\1",input$rank_list_8) ~ "Sat"
      ))
    # print(df)
    zeroDF <- data.frame(
      name = rep(c(" "),7),
      sum_calories  = rep(0, 7),
      sum_fat = rep(0, 7),
      sum_sat = rep(0, 7),
      sum_proteing   = rep(0, 7),
      sum_sodium = rep(0, 7),
      sum_potassium = rep(0, 7),
      sum_carbohydrates = rep(0, 7),
      sum_fiber = rep(0, 7),
      sum_sugar = rep(0, 7),
      ID = rep(0, 7),  
      weekDay = rep(c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"), each = 1)
      
    )
    
    df <- rbind(zeroDF, df)
    # print(df)
    df <- df %>%  
    group_by(weekDay) %>%
    summarise(sum_calories = sum(sum_calories),
              sum_fat = sum(sum_fat),
              sum_sat = sum(sum_sat),
              sum_proteing = sum(sum_proteing),
              sum_sodium = sum(sum_sodium),
              sum_potassium = sum(sum_potassium),
              sum_carbohydrates = sum(sum_carbohydrates),
              sum_fiber = sum(sum_fiber),
              sum_sugar = sum(sum_sugar))
    print(df)
  colnames(df) <- c("weekDay", "Calories", "Fat (g)", "Satuated Fat (g)",
                    "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                    "Carbohydrates (g)", "Fiber (g)", "Sugar (g)")
  print(df)
  df$weekDay <- factor(df$weekDay, levels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"))
  df <- df[order(df$weekDay), ]
  
 # Calories Plot
   caloriesPlot <- plot_ly() 
  
  
  if ("Calories" %in% input$macroSelector && input$barGraph == TRUE){
    caloriesPlot <- caloriesPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~Calories,
                                         type = 'bar',
                                         name = "Calories")
  }
  if ("Calories" %in% input$macroSelector && input$lineGraph == TRUE){
    caloriesPlot <- caloriesPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~Calories,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = "Calories")
  }
  if (input$bmrSelector == TRUE){
    caloriesPlot <- caloriesPlot %>%
      add_trace(data = df,
                x = df$weekDay,
                y = bmrReact(),
                type = "scatter",
                mode = "lines",
                name = "Your BMR")
  }

  
  
  # Fat Plot
    fatPlot <- plot_ly()
    
  
  
  if ("Fat (g)" %in% input$macroSelector && input$barGraph == TRUE) {
    fatPlot <- fatPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Fat (g)`,
                                         type = 'bar',
                                         name = "Fat (g)" )
  }
  if ("Fat (g)" %in% input$macroSelector && input$lineGraph == TRUE) {
    fatPlot <- fatPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Fat (g)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = "Fat (g)" )
  }
  
  # Sat Fat
    satfatPlot <- plot_ly()
  
  if ("Satuated Fat (g)" %in% input$macroSelector && input$barGraph == TRUE) {
    satfatPlot <- satfatPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Satuated Fat (g)`,
                                         type = 'bar',
                                         name = 'Saturated Fat (g)')
  }
  if ("Satuated Fat (g)" %in% input$macroSelector && input$lineGraph == TRUE) {
    satfatPlot <- satfatPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Satuated Fat (g)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = 'Saturated Fat (g)')
  }
  
  # Protien Graphs
    proteinPlot <- plot_ly()
  
  if ("Protein (g)" %in% input$macroSelector && input$barGraph == TRUE) {
    proteinPlot <- proteinPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Protein (g)`,
                                         type = 'bar',
                                         name = "Protein (g)")
  }
  if ("Protein (g)" %in% input$macroSelector && input$lineGraph == TRUE) {
    proteinPlot <- proteinPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Protein (g)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = "Protein (g)")
  }
  
  # Sodium Graphs
    sodiumPlot <- plot_ly()
  
  if ("Sodium (mg)" %in% input$macroSelector && input$barGraph == TRUE) {
    sodiumPlot <- sodiumPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Sodium (mg)`,
                                         type = 'bar',
                                         name = "Sodium (mg)")
    
  }
  if ("Sodium (mg)" %in% input$macroSelector && input$lineGraph == TRUE) {
    sodiumPlot <- sodiumPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Sodium (mg)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = "Sodium (mg)")
    
  }
  
  # Potassium Graph
    potassiumPlot <- plot_ly()
    
  if ("Potassium (mg)" %in% input$macroSelector && input$barGraph == TRUE) {
    potassiumPlot <- potassiumPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Potassium (mg)`,
                                         type = 'bar',
                                         name = "Potassium (mg)")
    
  }
  if ("Potassium (mg)" %in% input$macroSelector && input$lineGraph == TRUE) {
    potassiumPlot <- potassiumPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Potassium (mg)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = "Potassium (mg)")
    
  }
  
  # Carbs Plot
    carbPlot <- plot_ly()
  
  if ("Carbohydrates (g)" %in% input$macroSelector && input$barGraph == TRUE) {
    carbPlot <- carbPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Carbohydrates (g)`,
                                         type = 'bar',
                                         name = "Carbohydrates (g)")
    
  }
  
  if ("Carbohydrates (g)" %in% input$macroSelector && input$lineGraph == TRUE) {
    carbPlot <- carbPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Carbohydrates (g)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = "Carbohydrates (g)")
    
  }
  
  # Fiber
    fiberPlot <- plot_ly()

  
  if ("Fiber (g)" %in% input$macroSelector && input$barGraph == TRUE) {
    fiberPlot <- fiberPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Fiber (g)`,
                                         type = 'bar',
                                         name = 'Fiber (g)')
    
  }
  if ("Fiber (g)" %in% input$macroSelector && input$lineGraph == TRUE) {
    fiberPlot <- fiberPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Fiber (g)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = 'Fiber (g)')
    
  }
  
  # Sugar Plot
   sugarPlot <- plot_ly() 
  
  if ("Sugar (g)" %in% input$macroSelector && input$barGraph == TRUE) {
    sugarPlot <- sugarPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Sugar (g)`,
                                         type = 'bar',
                                         name = 'Sugar (g)')
    
  }
  if ("Sugar (g)"%in% input$macroSelector && input$lineGraph == TRUE) {
    sugarPlot <- sugarPlot %>% add_trace(data = df,
                                         x = ~weekDay,
                                         y = ~`Sugar (g)`,
                                         type = 'scatter',
                                         mode = 'lines+markers',
                                         name = 'Sugar (g)')
  }
  
  
  plotList <- list(caloriesPlot, fatPlot, satfatPlot, proteinPlot, sodiumPlot, 
                   potassiumPlot, carbPlot, fiberPlot, sugarPlot)
  
  selectedIndex <- c()
  
  if ("Calories" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 1)
  }
  if ("Fat (g)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 2)
  }
  if ("Satuated Fat (g)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 3)
  }
  if ("Protein (g)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 4)
  }
  if ("Sodium (mg)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 5)
  }
  if ("Potassium (mg)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 6)
  }
  if ("Carbohydrates (g)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 7)
  }
  if ("Fiber (g)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 8)
  }
  if ("Sugar (g)" %in% input$macroSelector){
    selectedIndex <- c(selectedIndex, 9)
  }
  print(selectedIndex)
  filteredPlotList <- plotList[selectedIndex]
  
  nrowVal <- 1
  
  
  if (length(selectedIndex)>2){
    nrowVal <- 3
  }
  if (length(selectedIndex) == 0){
    plot_ly()
  } else{
  subplot(filteredPlotList, nrows = nrowVal, titleY = TRUE)
  }
})
# 
# 
# # observeEvent(session, c(input$rstepDOW), {
# #   print(1)
#   # df <- recipe_df()%>%
#   #   mutate(id2 = as.character(id2))
#   # print(2)
#   # if(nrow(df()) == 0){
#   #   inputselectList <- c()
#   # }else {
#   # if (input$rstepDOW == "Sun"&& length(input$rarnk_list_2) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_2))
#   # } else if  (input$rstepDOW == "Mon" && length(input$rarnk_list_3) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_3))
#   # }else if  (input$rstepDOW == "Tue" && length(input$rarnk_list_4) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_4))
#   # }else if  (input$rstepDOW == "Wed" && length(input$rarnk_list_5) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_5))
#   # }else if  (input$rstepDOW == "Thur" && length(input$rarnk_list_6) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_6))
#   # }else if  (input$rstepDOW == "Fri" && length(input$rarnk_list_7) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_7))
#   # }else if  (input$rstepDOW == "Sat" && length(input$rarnk_list_8) > 0){
#   #   df <- df() %>%
#   #     filter(id2 %in% as.character(input$rank_list_8))
#   # } else {
#   #   c()
#   # }
# #     updateSelectInput(session, "recipeSelect", choices = df()[names])
# #
# #
# #
# #   print(df())
# #
# #   }
# #
# #   })
# #
# # recipeSelectChoices <- eventReactive(input$rstepDOW,{
# #
# #   df <- recipe_df()
# #   print(df)
# #   print(nrow(df))
# #   if(nrow(df) != 0){
# #     df <- df%>%
# #       mutate(id2 = as.character(id2))
# #     if (input$rstepDOW == "Sun"&& length(input$rarnk_list_2) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_2))
# #     } else if  (input$rstepDOW == "Mon" && length(input$rank_list_3) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_3))
# #     }else if  (input$rstepDOW == "Tue" && length(input$rank_list_4) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_4))
# #     }else if  (input$rstepDOW == "Wed" && length(input$rank_list_5) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_5))
# #     }else if  (input$rstepDOW == "Thur" && length(input$rank_list_6) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_6))
# #     }else if  (input$rstepDOW == "Fri" && length(input$rank_list_7) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_7))
# #     }else if  (input$rstepDOW == "Sat" && length(input$rank_list_8) > 0){
# #       df <- df %>%
# #         filter(id2 %in% as.character(input$rank_list_8))
# #     } else {
# #       df <- data.frame(
# #         id = numeric(0),
# #         id2 = numeric(0),
# #         name = character(0),
# #         servings = numeric(0),
# #         ingredients = character(0),
# #         steps = character(0)
# #       )
# #     }
# #   }
#   # print("end")
#   # print(df$name)
#   # print(input$rank_list_3)
# # })
# 
# #
# # observeEvent(c(input$rstepDOW), {
# #   updateSelectInput(session, "recipeSelect", choices = recipeSelectChoices()$name)
# # })

output$recipeSelectUI <- renderUI({
  df <- recipe_df()

  if(nrow(df) != 0){
    df <- df%>%
      mutate(id2 = as.character(id2))
    if (input$rstepDOW == "Sun"&& length(input$rank_list_2) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_2)))
    } else if  (input$rstepDOW == "Mon" && length(input$rank_list_3) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_3)))
    }else if  (input$rstepDOW == "Tue" && length(input$rank_list_4) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_4)))
    }else if  (input$rstepDOW == "Wed" && length(input$rank_list_5) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_5)))
    }else if  (input$rstepDOW == "Thur" && length(input$rank_list_6) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_6)))
    }else if  (input$rstepDOW == "Fri" && length(input$rank_list_7) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_7)))
    }else if  (input$rstepDOW == "Sat" && length(input$rank_list_8) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_8)))
    } else {
      df <- data.frame(
        id = character(0),
        id2 = character(0),
        name = character(0),
        servings = character(0),
        ingredients = character(0),
        steps = character(0)
      )
    }
  }
  
  choiceList <- df$name
  selectInput("recipeSelect", "Select Recipe:", choices = choiceList)
  
})


output$Recipieout <- renderUI({
  df <- recipe_df()
  print(df)
  print(paste("Nrow",nrow(df)))
  if(nrow(df) != 0){
    df <- df%>%
      mutate(id2 = as.character(id2))
    if (input$rstepDOW == "Sun"&& length(input$rank_list_2) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_2)))
    } else if  (input$rstepDOW == "Mon" && length(input$rank_list_3) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_3)))
    }else if  (input$rstepDOW == "Tue" && length(input$rank_list_4) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_4)))
    }else if  (input$rstepDOW == "Wed" && length(input$rank_list_5) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_5)))
    }else if  (input$rstepDOW == "Thur" && length(input$rank_list_6) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_6)))
    }else if  (input$rstepDOW == "Fri" && length(input$rank_list_7) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_7)))
    }else if  (input$rstepDOW == "Sat" && length(input$rank_list_8) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_8)))
    } else {
      df <- data.frame(
        id = numeric(0),
        id2 = numeric(0),
        name = character(0),
        sevings = numeric(0),
        ingredients = character(0),
        steps = character(0)
      )
    }
  df <- df %>% filter(name == input$recipeSelect)
  ingredients <- strsplit(df$ingredients, ", ")[[1]]
  ingredients <- paste("<ul>", paste("<li>", ingredients, "</li>", sep = ""), "</ul>", collapse = "\n")
  HTML(paste0("<b>", toTitleCase(df$name),"</b>", "<br><br>",
              "<b>Servings: </b>", df$sevings, "<br><br>",
              "<b> Ingredients:</b>", ingredients, "<br>"
  ))
  } else{
    HTML("  ")
  }
  
 
    
  
})


output$RecipeStep <- renderUI({
  df <- recipe_df()
  # print(df)
  # print(nrow(df))
  if(nrow(df) != 0){
    df <- df%>%
      mutate(id2 = as.character(id2))
    if (input$rstepDOW == "Sun"&& length(input$rank_list_2) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_2)))
    } else if  (input$rstepDOW == "Mon" && length(input$rank_list_3) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_3)))
    }else if  (input$rstepDOW == "Tue" && length(input$rank_list_4) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_4)))
    }else if  (input$rstepDOW == "Wed" && length(input$rank_list_5) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_5)))
    }else if  (input$rstepDOW == "Thur" && length(input$rank_list_6) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_6)))
    }else if  (input$rstepDOW == "Fri" && length(input$rank_list_7) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_7)))
    }else if  (input$rstepDOW == "Sat" && length(input$rank_list_8) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_8)))
    } else {
      df <- data.frame(
        id = numeric(0),
        id2 = numeric(0),
        name = character(0),
        sevings = numeric(0),
        ingredients = character(0),
        steps = character(0)
      )
    }
  }
  if (nrow(df)!= 0){
  df <- df %>% filter(name == input$recipeSelect)
  HTML(paste0("Steps: <br> ",gsub("\r", "<br>", df$steps)))
  
  } else {
    HTML(" ")
  }
})




output$piePlot <- renderPlotly({
  df <- recipe_df()
  # print(df)
  # print(nrow(df))
  if(nrow(df) != 0){
    df <- df%>%
      mutate(id2 = as.character(id2))
    if (input$rstepDOW == "Sun"&& length(input$rank_list_2) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_2)))
    } else if  (input$rstepDOW == "Mon" && length(input$rank_list_3) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_3)))
    }else if  (input$rstepDOW == "Tue" && length(input$rank_list_4) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_4)))
    }else if  (input$rstepDOW == "Wed" && length(input$rank_list_5) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_5)))
    }else if  (input$rstepDOW == "Thur" && length(input$rank_list_6) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_6)))
    }else if  (input$rstepDOW == "Fri" && length(input$rank_list_7) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_7)))
    }else if  (input$rstepDOW == "Sat" && length(input$rank_list_8) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_8)))
    } else {
      df <- data.frame(
        id = numeric(0),
        id2 = numeric(0),
        name = character(0),
        sevings = numeric(0),
        ingredients = character(0),
        steps = character(0)
      )
    }
  }
  if (nrow(df)!= 0){
    df <- df %>% filter(name == input$recipeSelect)
    
    id_num <- df$id
    print(id_num)
    nutDF <- macrosDf() %>% filter(RID == id_num)
    nutDF$name <- toTitleCase(nutDF$name)
    colnames(nutDF) <- c("name", "Calories", 'Ignore', "Fat (g)", "Satuated Fat (g)",
                         "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                         "Carbohydrates (g)", "Fiber (g)", "Sugar (g)", "RID", "ID")
    nutDF <- nutDF %>% select("name", input$macroSelector2)
    
    if(nrow(nutDF > 5)){
      sortNut <- nutDF[order(-nutDF[,2]), ]
      top5 <- head(sortNut,5)
      remaining <- sortNut[-(1:5), ]
      
      nutDF <- rbind(top5, c("Other", sum(remaining[, 2])))
      plot_ly(nutDF, labels = ~name, values =~get(input$macroSelector2), type = 'pie')
      
  } 
  }

})


output$macroDT <- renderDT({
  df <- recipe_df()
  # print(df)
  # print(nrow(df))
  if(nrow(df) != 0){
    df <- df%>%
      mutate(id2 = as.character(id2))
    if (input$rstepDOW == "Sun"&& length(input$rank_list_2) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_2)))
    } else if  (input$rstepDOW == "Mon" && length(input$rank_list_3) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_3)))
    }else if  (input$rstepDOW == "Tue" && length(input$rank_list_4) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_4)))
    }else if  (input$rstepDOW == "Wed" && length(input$rank_list_5) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_5)))
    }else if  (input$rstepDOW == "Thur" && length(input$rank_list_6) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_6)))
    }else if  (input$rstepDOW == "Fri" && length(input$rank_list_7) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_7)))
    }else if  (input$rstepDOW == "Sat" && length(input$rank_list_8) > 0){
      df <- df %>%
        filter(id2 %in% as.character(sub(".*\\.(\\d+)$", "\\1",input$rank_list_8)))
    } else {
      df <- data.frame(
        id = numeric(0),
        id2 = numeric(0),
        name = character(0),
        sevings = numeric(0),
        ingredients = character(0),
        steps = character(0)
      )
    }
  }
  if (nrow(df)!= 0){
    df <- df %>% filter(name == input$recipeSelect)
    id_num <- df$id
    print(id_num)
    nutDF <- macrosDf() %>% filter(RID == id_num)
    nutDF$name <- toTitleCase(nutDF$name)
    colnames(nutDF) <- c("name", "Calories", 'Ignore', "Fat (g)", "Satuated Fat (g)",
                         "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                         "Carbohydrates (g)", "Fiber (g)", "Sugar (g)", "RID", "ID")
    datatable(nutDF %>% select("name", "Calories","Fat (g)", "Satuated Fat (g)",
                               "Protein (g)", "Sodium (mg)", "Potassium (mg)",
                               "Carbohydrates (g)", "Fiber (g)", "Sugar (g)"))
  } 

})


output$my_tab <- renderTable({
  recipe_df()
})





#### Reset Shiny App
observeEvent(c(input$clearInput), {
  recipe_df(data.frame(
    id = NULL,
    name = NULL,
    ingredients = NULL,
    steps = NULL
  ))
  macrosDf(
    data.frame(
      name = character(0),
      calories = numeric(0),
      serving_size_g = numeric(0),
      fat_total_g = numeric(0),
      fat_saturated_g = numeric(0),
      protein_g = numeric(0),
      sodium_mg = numeric(0),
      potassium_mg = numeric(0),
      cholesterol_mg = numeric(0),
      carbohydrates_total_g = numeric(0),
      fiber_g = numeric(0),
      sugar_g = numeric(0),
      RID = character(0),
      ID = character(0)
    )
  )
  Sum.macrosDf(data.frame(
      name = character(0),
      sum_calories = numeric(0),
      sum_fat = numeric(0),
      sum_sat = numeric(0),
      sum_proteing = numeric(0),
      sum_sodium = numeric(0),
      sum_potassium =numeric(0) ,
      sum_carbohydrates = numeric(0),
      sum_fiber = numeric(0),
      sum_sugar = numeric(0),
      ID = character(0)
    )
  )
})


}