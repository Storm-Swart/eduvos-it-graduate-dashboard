library(readr)
library(tidyverse)

# Load dataset
graduate_survey <- read_csv("C:/Users/Storm/Desktop/University Tings/3rd Year/ITRDA3-11/ITRDA3-11 - Assignment - Online - EDUV4817334/graduate_survey.csv")

View(graduate_survey)

# First, let's examine the dataset structure
glimpse(graduate_survey)
summary(graduate_survey)

# 1. Select only the relevant columns mentioned in the scenario
clean_data <- graduate_survey %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, 
         Databases, Platform, WebFramework, Industry, 
         AISearch, AITool, Employment)

# 2. Check for missing values
colSums(is.na(clean_data))

# 3. Handle missing values - creating "Unknown" category for missing categorical data
clean_data <- clean_data %>%
  mutate(across(everything(), ~ifelse(is.na(.), "Unknown", as.character(.))))

# 4. Standardize Campus values (combining Durban and Umhlanga)
# First, check unique values
unique(clean_data$Campus)

# Standardize campus names
clean_data <- clean_data %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban", "Umhlanga") ~ "Durban",
    # Add more standardizations as needed based on what you see in unique values
    TRUE ~ Campus
  ))

# 5. Count responses by campus to identify top 3-5 campuses
campus_counts <- clean_data %>%
  count(Campus, sort = TRUE) %>%
  print()

# Get names of top 5 campuses
top_campuses <- campus_counts %>%
  slice_head(n = 5) %>%
  pull(Campus)

# Subset data to include only top campuses
filtered_data <- clean_data %>%
  filter(Campus %in% top_campuses)

# 6. For multi-value columns 
# Check if ProgLang has multiple values per cell (comma-separated)
if(any(grepl(",", clean_data$ProgLang))) {
  # Example processing for programming languages
  prog_lang_data <- clean_data %>%
    select(Campus, StudyField, ProgLang) %>%
    separate_rows(ProgLang, sep = ",\\s*") %>%
    mutate(ProgLang = trimws(ProgLang))
  
  # Preview the processed data
  head(prog_lang_data)
}

# Save the cleaned dataset
write_csv(filtered_data, "C:/Users/Storm/Desktop/University Tings/3rd Year/ITRDA3-11/ITRDA3-11 - Assignment - Online - EDUV4817334/cleaned_graduate_survey.csv")
