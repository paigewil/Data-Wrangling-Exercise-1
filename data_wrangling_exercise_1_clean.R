#Downloading packages need
library(dplyr)
library(tidyr)
library(tools)

#Step 0: Load the data in RStudio
df <- read.csv("E:/Learning/Springboard Intro to Data Science/exercises/refine_original.csv", stringsAsFactors = FALSE)

# Step 1: Clean up brand names
df$company <- sapply(df$company, tolower)
df$company <- sub(pattern = "phllips|phillips|phillps|phlips|fillips", replacement = "philips", x = df$company)
df$company <- sub(pattern = "akz0|ak zo", replacement = "akzo", x = df$company)
df$company <- sub(pattern = "unilver", replacement = "unilever", x = df$company)

#Step 2: Separate product code and number
df <- df %>% separate(Product.code...number, c("product_code", "product_number"), "-")

#Step 3: Add product categories
prod_cat_fun <- function(seq) {
  new_seq <- vector()
  for (i in 1: length(seq)) {
    if (seq[i] == "p") {
      new_seq[i] <- "Smartphone"
    }
    if (seq[i] == "v") {
      new_seq[i] <- "TV"
    }
    if (seq[i] == "x") {
      new_seq[i] <- "Laptop"
    } 
    if (seq[i] == "q") {
      new_seq[i] <- "Tablet"
    }
  }
  return (new_seq)
}

df$product_category <- prod_cat_fun(df$product_code)

#Step 4: Add full address for geocoding
df$full_address <- paste(df$address, toTitleCase(df$city), toTitleCase(df$country), sep = ", ")

#Step 5: Create dummy variables for company and product category
df$company_philips <- as.numeric(grepl(pattern = "philips", x = df$company))
df$company_akzo <- as.numeric(grepl(pattern = "akzo", x = df$company))
df$company_va_houten <- as.numeric(grepl(pattern = "van houten", x = df$company))
df$company_unilever <- as.numeric(grepl(pattern = "unilever", x = df$company))


df$product_smartphone <- as.numeric(grepl(pattern = "Smartphone", x = df$product_category))
df$product_tv <- as.numeric(grepl(pattern = "TV", x = df$product_category))
df$product_laptop <- as.numeric(grepl(pattern = "Laptop", x = df$product_category))
df$product_tablet <- as.numeric(grepl(pattern = "Tablet", x = df$product_category))


write.csv(file = "E:/Learning/Springboard Intro to Data Science/exercises/refine_clean.csv", x=df)
