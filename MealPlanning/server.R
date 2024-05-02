
library(shiny)
library(httr)
library(jsonlite)
library(shinyWidgets)

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
    ingredientsCleaned <- gsub("dash", "1", ingredientsCleaned)
    ingredientsCleaned <- gsub("pinch", "1", ingredientsCleaned)
    ingredientsCleaned <- gsub("sprinkle", "1", ingredientsCleaned)
    
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
    name = NULL,
    ingredients = NULL,
    steps = NULL
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
output$reactive_output <- renderText({
  # Print reactive value
  print(toString(bucket_recipe_list()))
  print(toString(sunday_list()))
})

################################################################################
# Meal Macro Conts
################################################################################

getNutritionFacts <- function(recipe_item_code, input_api_key, input_query){
  api_url <- "https://api.calorieninjas.com/v1/nutrition?query="
  response <- GET(paste0(api_url, URLencode(input_query)), add_headers('X-Api-Key' = input_api_key))
  temp.df <- fromJSON(content(response, "text"))$items
  temp.df$RID <- as.character(recipe_item_code)
  temp.df  
}


macrosDf <- reactiveVal({
  cnl <- c("name", "calories", "serving_size_g", "fat_total_g", 
           "fat_saturated_g", "protein_g", "sodium_mg", "potassium_mg", 
           "cholesterol_mg", "carbohydrates_total_g", "fiber_g", 
           "sugar_g", "RID", "ID")
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
################################################################################

api_key <- "QAM21lUsGekFQHsi6lktYg==fwz7Orddmt1IZLYu"

recipeIdentifier <- reactiveVal({0})

# Add Recipe data
observeEvent(c(input$addRecipies),{
  recipe_names <- c(input$rank_list_1)
  for (selectedrecipie in input$recipieSelector){
    url <- paste0("https://www.themealdb.com/api/json/v1/1/lookup.php?i=",selectedrecipie)
    meal_df <- process_meal(url)
    recipe_df(rbind(recipe_df(),meal_df))
    id <- recipeIdentifier()
    recipe_names <- c(recipe_names, paste0(meal_df[2],"  --  ", meal_df[1],".",id) )
    
    ingredientdf <- getNutritionFacts(meal_df[1], "QAM21lUsGekFQHsi6lktYg==fwz7Orddmt1IZLYu", meal_df[3])
    ingredientdf$ID <- id
    macrosDf(rbind(macrosDf(), ingredientdf))
    recipeIdentifier(id+1)
    # update_rank_list
    # old_recipie_rank <- input$rank_list_1
    # new_labels <- 
    # update_rank_list("rank_list_1", labels = new_labels)
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
global_recipie_coutner <-reactiveVal(1)
observeEvent(c(input$rSubmit),{
  custom_id <-paste0("CR", global_recipie_coutner())
  custom_recipe <- data.frame(
    id = custom_id,
    name = input$rName,
    ingredients = input$rIngredient,
    steps = input$rSteps
  )
  global_recipie_coutner(global_recipie_coutner()+1)
  recipe_df(rbind(recipe_df(),custom_recipe))
  
  

  
  # Clear Fields
  updateTextInput(session,
                  'rName',
                  value = '')
  updateTextInput(session,
                  'rIngredient',
                  value = '')
  updateTextInput(session,
                  'rSteps',
                  value = '')
})

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
      # labels = NULL,
      labels = sunday_list(),
      input_id = "rank_list_2"
    ),
    add_rank_list(
      text = "Monday",
      # labels = NULL,
      labels = monday_list(),
      input_id = "rank_list_3"
    ),
    add_rank_list(
      text = "Tuesday",
      # labels = NULL,
      labels = tuesday_list(),
      input_id = "rank_list_4"
    ),
    add_rank_list(
      text = "Wednesday",
      # labels = NULL,
      labels = wednesday_list(),
      input_id = "rank_list_5"
    ),
    add_rank_list(
      text = "Thursday",
      # labels = NULL,
      labels = thursday_list(),
      input_id = "rank_list_6"
    ),
    add_rank_list(
      text = "Friday",
      # labels = NULL,
      labels = friday_list(),
      input_id = "rank_list_7"
    ),
    add_rank_list(
      text = "Saturday",
      # labels = NULL,
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

NutDaydf <- reactiveVal({
  data.frame(
  test = c("1", "2"),
  t2 = c("3", "4")
  )
})

sunNut <- reactiveVal({
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
})
cnl <- c("name", "calories", "serving_size_g", "fat_total_g", 
                     "fat_saturated_g", "protein_g", "sodium_mg", "potassium_mg", 
                     "cholesterol_mg", "carbohydrates_total_g", "fiber_g", 
                     "sugar_g", "RID", "ID")
monNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
tueNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
wedNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
thurNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
friNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
satNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})
sunNut <- reactiveVal({
  empty_df <- data.frame(matrix(ncol = length(cnl), nrow = 0))
  colnames(empty_df) <- cnl
  empty_df
})

observeEvent(c("submitWeekDay","input$rank_list_2",
               "input$rank_list_3","input$rank_list_4",
               "input$rank_list_5","input$rank_list_6","input$rank_list_7",
               "input$rank_list_8","input$rank_list_9"),{
        df <- macrosDf()
  sunNut(
    df[df[, 14] %in% gsub(".*\\.(\\d+)$", "\\1",input$rank_list_2), ]
    )
  # monNut(
  #   macrosDf() %>% filter(ID %in% input$rank_list_3)
  # )
  # tueNut(
  #   macrosDf() %>% filter(ID %in% input$rank_list_4)
  # )
  # wedNut(
  #   macrosDf() %>% filter(ID %in% input$rank_list_5)
  # )
  # thurNut(
  #   macrosDf() %>% filter(ID %in% input$rank_list_6)
  # )
  # friNut(
  #   macrosDf() %>% filter(ID %in% input$rank_list_7)
  # )
  # satNut(
  #   macrosDf() %>% filter(ID %in% input$rank_list_8)
  # )
})


output$MealNutTable <- renderTable({
  if(input$weekDaySelect == "sun"){
    sunNut()
  }else if (input$weekDaySelect == "mon"){
    monNut()}
  else if (input$weekDaySelect == "tue"){
    tueNut()
  }else if (input$weekDaySelect == "wed"){
    wedNut()
  }else if (input$weekDaySelect == "thur"){
    thurNut()
  }else if (input$weekDaySelect == "fri"){
    friNut()
  }else if (input$weekDaySelect == "sat"){
    satNut()
  }else{
    macrosDf()
  }

})







### Test View the Recipe DF
output$my_table1 <- renderTable({
  NutDaydf()
})
output$my_table <- renderTable({
  macrosDf()
})
###


#### Reset Shiny App
observeEvent(c(input$clearInput), {
  recipe_df(data.frame(
    id = NULL,
    name = NULL,
    ingredients = NULL,
    steps = NULL
  ))
})


}