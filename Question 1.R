library(readr)
library(tidyverse)

# Load dataset
graduate_survey <- read_csv("graduate_survey.csv")

View(graduate_survey)

# Dataset structure
glimpse(graduate_survey)
summary(graduate_survey)

# Relevant columns
clean_data <- graduate_survey %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, 
         Databases, Platform, WebFramework, Industry, 
         AISearch, AITool, Employment)

# Check for missing values
colSums(is.na(clean_data))

# Handle missing values 
clean_data <- clean_data %>%
  mutate(across(everything(), ~ifelse(is.na(.), "Unknown", as.character(.))))

# Standardize Campus values 
unique(clean_data$Campus)

# Standardize campus names
clean_data <- clean_data %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban", "Umhlanga") ~ "Durban",
    TRUE ~ Campus
  ))

# Count responses by campus
campus_counts <- clean_data %>%
  count(Campus, sort = TRUE) %>%
  print()

# Top 5 campus names
top_campuses <- campus_counts %>%
  slice_head(n = 5) %>%
  pull(Campus)

# Include only top campuses
filtered_data <- clean_data %>%
  filter(Campus %in% top_campuses)

# Multi-value columns 
if(any(grepl(",", clean_data$ProgLang))) {
  prog_lang_data <- clean_data %>%
    select(Campus, StudyField, ProgLang) %>%
    separate_rows(ProgLang, sep = ",\\s*") %>%
    mutate(ProgLang = trimws(ProgLang))
  
  head(prog_lang_data)
}

# Save cleaned dataset
write_csv(filtered_data, "cleaned_graduate_survey.csv")