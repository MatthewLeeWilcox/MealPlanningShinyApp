library(httr)
library(jsonlite)

api_key <- "QAM21lUsGekFQHsi6lktYg==fwz7Orddmt1IZLYu"
api_url <- 'https://api.calorieninjas.com/v1/nutrition?query='
query <- '3lb carrots and a chicken sandwich'
response <- GET(paste0(api_url, URLencode(query)), add_headers('X-Api-Key' = api_key))

if (status_code(response) == 200) {
  print(content(response, "text"))
} else {
  cat("Error:", status_code(response), content(response, "text"))
}


test.df <- fromJSON(content(response, "text"))

test2 <- test.df$items
test2

original_string <- "1 Chicken Breast, 1/4 cup Pickle Juice, 1 Egg, 1/4 cup Milk, 1/2 cup Flour, 1 tbs Icing Sugar, 1/2 tsp Paprika, 1/2 tsp Salt, 1/4 tsp Black Pepper, 1/4 tsp Garlic Powder, 1/4 tsp Celery Salt, 1/2 tsp Cayenne Pepper, 1 cup Olive Oil, 1 Sesame Seed Burger Buns, , , , , ,"

# Remove trailing commas using regex
cleaned_string <- sub(",\\s*$", "", original_string)

# Print cleaned string
cleaned_string



removeEndCommas <- function(input_str){
  input_str <- input_str
  cleaned_string <- gsub(" ,", "", input_str)
  lastChar <- substring(cleaned_string, nchar(cleaned_string)-1, nchar(cleaned_string))
  if (substring(cleaned_string, nchar(cleaned_string), nchar(cleaned_string)) == ",") {
    # Remove the last character
    cleaned_string <- substr(cleaned_string, 1, nchar(cleaned_string) - 1)
  } else {
    cleaned_string <- cleaned_string
  }
  cleaned_string
  
}

removeEndCommas(original_string)




getNutritionFacts <- function(input_url, input_api_key, input_query){
  response <- GET(paste0(input_url, URLencode(input_query)), add_headers('X-Api-Key' = input_api_key))
  temp.df <- fromJSON(content(response, "text"))
  temp.df$items
  
}


x <- getNutritionFacts(api_url, api_key, '3lb carrots and a chicken sandwich')
recipe_item_code <- "1234"

url <- paste0("https://api.calorieninjas.com/v1/nutrition?query=", recipe_item_code)
url


clean_data <- data.frame(
  Name = c("John", "Alice", "Bob", "Emily", "David"),
  Age = c(25, 30, 35, NA, 40),
  Gender = c("M", "F", "M", "F", "M"),
  Score = c(80, 85, 90, 75, 95)
)

# Add a new column with constant value
clean_data$NewColumn <- "ConstantValue"

# Print the updated data frame
print(clean_data)



getNutritionFacts <- function(recipe_item_code, input_api_key, input_query){
  api_url <- "https://api.calorieninjas.com/v1/nutrition?query="
  response <- GET(paste0(api_url, URLencode(input_query)), add_headers('X-Api-Key' = input_api_key))
  temp.df <- fromJSON(content(response, "text"))$items
  # temp.df$RID <- as.character(recipe_item_code)
  temp.df  
}

z <- getNutritionFacts("12", api_key, query)
View(z)
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
  print(class(measurements))
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


url <- 'https://www.themealdb.com/api/json/v1/1/lookup.php?i=52995'
url2 <- 'https://www.themealdb.com/api/json/v1/1/lookup.php?i=53085'
url3 <- 'https://www.themealdb.com/api/json/v1/1/lookup.php?i=52812'


process_meal(url)
process_meal(url2)
process_meal(url3)



df <-  data.frame(
  name = NA,
  calories = NA
)


df <- data.frame(
  group_var = c("A", "A", "B", "B"),
  var1 = c(1, 2, 3, 4),
  var2 = c(5, 6, 7, 8)
)

# Group by 'group_var' and get the total sum for every column
total_sums <- df %>%
  group_by(group_var) %>%
  summarise(across(everything(), sum))

print(total_sums)

# Create an empty data frame with column names
empty_df <- data.frame(
  var1 = numeric(0),  # Example column with numeric data type and 0 rows
  var2 = character(0), # Example column with character data type and 0 rows
  var3 = logical(0)    # Example column with logical data type and 0 rows
)

# Check the structure of the empty data frame
str(empty_df)

