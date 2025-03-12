library(tidyverse)
library(ggplot2)
library(scales)

# Load cleaned dataset
cleaned_data <- read_csv("cleaned_graduate_survey.csv")

# Function to process multi-value columns
process_multivalue_column <- function(data, column, top_n = 10) {
  data %>%
    select(all_of(column)) %>%
    separate_rows(!!sym(column), sep = ";") %>%
    mutate(across(everything(), trimws)) %>%
    filter(!is.na(!!sym(column)), !!sym(column) != "") %>% # Remove blanks
    count(!!sym(column), sort = TRUE) %>%
    top_n(top_n, n) %>%
    rename(Item = !!sym(column))
}

# Function to create better styled plots
create_bar_chart <- function(data, title, subtitle = NULL, color_palette = "viridis") {
  ggplot(data, aes(x = reorder(Item, n), y = n, fill = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_viridis_c() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL, 
      y = "Number of Graduates",
      caption = "Source: Graduate Survey Data"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 10),
      panel.grid.major.y = element_blank()
    )
}

#------------------- Top Tools Used by Graduates --------------------#

# Store tool plots
tool_plots <- list()

# Programming Languages
top_prog_lang <- process_multivalue_column(cleaned_data, "ProgLang")
tool_plots$prog_lang <- create_bar_chart(
  top_prog_lang, 
  "Top Programming Languages Used by Graduates",
  "Question I: What are the top tools used by graduates?"
)

# Databases
top_databases <- process_multivalue_column(cleaned_data, "Databases")
tool_plots$databases <- create_bar_chart(
  top_databases, 
  "Top Databases Used by Graduates"
)

# Cloud Platforms
top_platforms <- process_multivalue_column(cleaned_data, "Platform")
tool_plots$platforms <- create_bar_chart(
  top_platforms, 
  "Top Cloud Platforms Used by Graduates"
)

# Web Frameworks
top_web_frameworks <- process_multivalue_column(cleaned_data, "WebFramework")
tool_plots$web_frameworks <- create_bar_chart(
  top_web_frameworks, 
  "Top Web Frameworks Used by Graduates"
)

# AI Search Tools
top_ai_search <- process_multivalue_column(cleaned_data, "AISearch")
tool_plots$ai_search <- create_bar_chart(
  top_ai_search, 
  "Top AI Search Tools Used by Graduates"
)

# AI Developer Tools
top_ai_tools <- process_multivalue_column(cleaned_data, "AITool")
tool_plots$ai_tools <- create_bar_chart(
  top_ai_tools, 
  "Top AI Developer Tools Used by Graduates"
)

# Display tool plots in sequence
for (plot in tool_plots) {
  print(plot)
}

#------------------- Most Popular Industries --------------------#

# Visualization of industries by study field
industry_by_field <- cleaned_data %>%
  select(StudyField, Industry) %>%
  separate_rows(Industry, sep = ";") %>%
  mutate(Industry = trimws(Industry)) %>%
  filter(!is.na(Industry), Industry != "") %>%
  count(StudyField, Industry, sort = TRUE) %>%
  group_by(StudyField) %>%
  top_n(5, n) %>%
  ungroup()

# Plot top industries by study field
ggplot(industry_by_field, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity") +
  facet_wrap(~StudyField, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top Industries by Study Field",
    subtitle = "Question II: What are the most popular industries graduates go into?",
    x = NULL,
    y = "Number of Graduates",
    caption = "Source: Graduate Survey Data"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "lightblue", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )


#------------------- Top Job Roles --------------------#

# Visualization of top roles by study field
roles_by_field <- cleaned_data %>%
  count(StudyField, Role, sort = TRUE) %>%
  group_by(StudyField) %>%
  top_n(5, n) %>%
  ungroup()

# Plot top roles by study field
ggplot(roles_by_field, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity") +
  facet_wrap(~StudyField, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top Job Roles by Study Field",
    subtitle = "Question III: What are the top job roles graduates go into?",
    x = NULL,
    y = "Number of Graduates",
    caption = "Source: Graduate Survey Data"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "lightgreen", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

#------------------- Employment Rate --------------------#

# Calculate employment rate 
employment_rate <- cleaned_data %>%
  count(StudyField, Employment) %>%
  group_by(StudyField) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total * 100
  ) %>%
  ungroup()

unique_employment <- unique(cleaned_data$Employment)
length(unique_employment)  # See how many categories we have

# Employment status variable
cleaned_data <- cleaned_data %>%
  mutate(
    EmploymentSimplified = case_when(
      # Consider anyone with "Employed" in their status as employed
      grepl("Employed", Employment) ~ "Employed",
      # Consider "Independent contractor, freelancer, or self-employed" as employed
      grepl("Independent contractor, freelancer, or self-employed", Employment) ~ "Employed",
      # Consider anyone looking for work or explicitly not employed as unemployed
      grepl("Not employed", Employment) | grepl("looking for work", Employment) ~ "Unemployed",
      # Consider students and retired as their own categories
      grepl("Student", Employment) & !grepl("Employed", Employment) ~ "Student",
      grepl("Retired", Employment) & !grepl("Employed", Employment) ~ "Retired",
      TRUE ~ "Other"  # Catch-all for any other statuses
    )
  )

# Check categories
table(cleaned_data$EmploymentSimplified)

# Calculate employment rate with simplified categories
employment_rate_simple <- cleaned_data %>%
  count(StudyField, EmploymentSimplified) %>%
  group_by(StudyField) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total * 100
  ) %>%
  ungroup()

# Binary employment rate 
binary_employment <- cleaned_data %>%
  mutate(
    EmploymentBinary = if_else(
      EmploymentSimplified == "Employed", 
      "Employed", 
      "Not Employed"
    )
  ) %>%
  count(StudyField, EmploymentBinary) %>%
  group_by(StudyField) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total * 100
  ) %>%
  ungroup()

# Plot simplified employment rates
ggplot(employment_rate_simple, aes(x = StudyField, y = Percentage, fill = EmploymentSimplified)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percentage), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3) +
  scale_fill_brewer(palette = "Set2") + 
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Employment Status by Study Field",
    subtitle = "Question IV: What is the employment rate of graduates from each study?",
    x = "Study Field",
    y = "Percentage",
    fill = "Employment Status",
    caption = "Source: Graduate Survey Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot binary employment rates
ggplot(binary_employment, aes(x = StudyField, y = Percentage, fill = EmploymentBinary)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percentage), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Employed" = "#4DAF4A", "Not Employed" = "#E41A1C")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Employment Rate by Study Field",
    subtitle = "Question IV: What is the employment rate of graduates from each study?",
    x = "Study Field",
    y = "Percentage",
    fill = "Employment Status",
    caption = "Source: Graduate Survey Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Summary table for binary employment rates
binary_summary <- binary_employment %>%
  filter(EmploymentBinary == "Employed") %>%
  select(StudyField, Percentage) %>%
  arrange(desc(Percentage))

# Print summary
cat("\n### Employment Rate by Study Field ###\n")
print(binary_summary)


#------------------- Summary Tables --------------------#

# Top tools summary
top_tools_summary <- list(
  "Programming Languages" = top_prog_lang %>% head(5) %>% select(Item, n),
  "Databases" = top_databases %>% head(5) %>% select(Item, n),
  "Cloud Platforms" = top_platforms %>% head(5) %>% select(Item, n),
  "Web Frameworks" = top_web_frameworks %>% head(5) %>% select(Item, n),
  "AI Search Tools" = top_ai_search %>% head(5) %>% select(Item, n),
  "AI Developer Tools" = top_ai_tools %>% head(5) %>% select(Item, n)
)

# Print summary table
for (name in names(top_tools_summary)) {
  cat("\n### Top 5", name, "###\n")
  print(top_tools_summary[[name]])
}

# Most popular industries summary
cat("\n### Most Popular Industries by Study Field ###\n")
industry_summary <- industry_by_field %>%
  group_by(StudyField) %>%
  slice_max(order_by = n, n = 3) %>%
  select(StudyField, Industry, n)
print(industry_summary)

# Top job roles summary
cat("\n### Top Job Roles by Study Field ###\n")
roles_summary <- roles_by_field %>%
  group_by(StudyField) %>%
  slice_max(order_by = n, n = 3) %>%
  select(StudyField, Role, n)
print(roles_summary)

# Employment rate summary
cat("\n### Employment Rate by Study Field ###\n")
employment_summary <- employment_rate %>%
  filter(Employment == "Employed") %>%
  select(StudyField, Percentage) %>%
  arrange(desc(Percentage))
print(employment_summary)