# Eduvos Graduate Survey Dashboard
# Created: March 10, 2025
# Author: Storm Swart

# Required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(scales)
library(plotly)
library(shinyWidgets)
library(rsconnect)


#------------------- Data Processing Functions --------------------#


# Multi-value columns function
process_multivalue_column <- function(data, column, top_n = 10) {
  data %>%
    select(all_of(column)) %>%
    separate_rows(!!sym(column), sep = ";") %>%
    mutate(across(everything(), trimws)) %>%
    filter(!is.na(!!sym(column)), !!sym(column) != "") %>% 
    count(!!sym(column), sort = TRUE) %>%
    top_n(top_n, n) %>%
    rename(Item = !!sym(column))
}

# Function to create interactive bar charts
create_bar_chart <- function(data, title, subtitle = NULL, color_palette = "viridis") {
  p <- ggplot(data, aes(x = reorder(Item, n), y = n, fill = n)) +
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
  
  return(ggplotly(p))
}

# Filter data by multiple criteria function
filter_data <- function(data, campus_filter, field_filter, role_filter) {
  filtered <- data
  
  # Apply campus filter if not "All"
  if (!("All" %in% campus_filter)) {
    filtered <- filtered %>% filter(Campus %in% campus_filter)
  }
  
  # Apply study field filter if not "All"
  if (!("All" %in% field_filter)) {
    filtered <- filtered %>% filter(StudyField %in% field_filter)
  }
  
  # Apply role filter if not "All"
  if (!("All" %in% role_filter)) {
    filtered <- filtered %>% filter(Role %in% role_filter)
  }
  
  return(filtered)
}


#------------------- UI Definition -------------------#


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Eduvos IT Graduate Survey",
    titleWidth = 300
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Tech Tools", tabName = "tech_tools", icon = icon("laptop-code")),
      menuItem("Employment", tabName = "employment", icon = icon("briefcase")),
      menuItem("Job Roles & Industries", tabName = "roles", icon = icon("users")),
      menuItem("Raw Data", tabName = "raw_data", icon = icon("table")),
      menuItem("User Guide", tabName = "guide", icon = icon("question-circle"))
    ),
    br(),
    
    # Global Filters
    selectizeInput(
      "campus_filter", 
      "Campus:",
      choices = c("All"),
      multiple = TRUE,
      selected = "All",
      options = list(plugins = list('remove_button'))
    ),
    selectizeInput(
      "field_filter", 
      "Study Field:",
      choices = c("All"),
      multiple = TRUE,
      selected = "All",
      options = list(plugins = list('remove_button'))
    ),
    selectizeInput(
      "role_filter", 
      "Role:",
      choices = c("All"),
      multiple = TRUE,
      selected = "All",
      options = list(plugins = list('remove_button'))
    ),
    actionButton("reset_filters", "Reset Filters", icon = icon("refresh")),
    br(), br(),
    
    # Footer with credits
    div(
      style = "padding: 15px;",
      "Dashboard created by:",
      br(),
      "Student ID: EDUV4817334",
      br(),
      "Date: March 2025"
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom CSS styling */
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        .value-box {
          border-radius: 5px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        .small-box {
          border-radius: 5px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        .info-box {
          border-radius: 5px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        .bg-blue {
          background-color: #3c8dbc !important;
        }
        .bg-green {
          background-color: #00a65a !important;
        }
        .bg-yellow {
          background-color: #f39c12 !important;
        }
        .bg-red {
          background-color: #dd4b39 !important;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "About This Dashboard",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("This dashboard analyzes survey data from Eduvos IT graduates to help understand their career pathways and technology preferences."),
                  p("Use the filters on the left sidebar to explore data by campus, study field, and job role. Navigate between tabs to view different aspects of the data."),
                  p("The dashboard provides insights into:"),
                  tags$ul(
                    tags$li("Programming languages, databases, frameworks, and tools used by graduates"),
                    tags$li("Employment rates across different study fields"),
                    tags$li("Popular job roles and industries"),
                    tags$li("Technology trends and preferences")
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("total_graduates_box", width = 3),
                valueBoxOutput("employed_rate_box", width = 3),
                valueBoxOutput("top_study_field_box", width = 3),
                valueBoxOutput("top_role_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Survey Response by Campus",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotlyOutput("campus_plot", height = "300px")
                ),
                box(
                  title = "Employment Status",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotlyOutput("employment_overview_plot", height = "300px")
                )
              )
      ),
      
      # Tech Tools Tab
      tabItem(tabName = "tech_tools",
              fluidRow(
                box(
                  title = "Technology Tools Filter",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("tech_type", "Select Technology Category:",
                              choices = c("Programming Languages", "Databases", "Cloud Platforms", 
                                          "Web Frameworks", "AI Search Tools", "AI Developer Tools"),
                              selected = "Programming Languages")
                )
              ),
              fluidRow(
                tabBox(
                  title = "Technology Analysis",
                  width = 12,
                  tabPanel("Tool Usage", 
                           plotlyOutput("tech_tool_plot", height = "500px")),
                  tabPanel("Tool Distribution by Study Field", 
                           plotlyOutput("tech_by_field_plot", height = "500px"))
                )
              )
      ),
      
      # Employment Tab
      tabItem(tabName = "employment",
              fluidRow(
                tabBox(
                  title = "Employment Status",
                  width = 12,
                  tabPanel("Overall Employment Rate", 
                           plotlyOutput("employment_rate_plot", height = "400px")),
                  tabPanel("Employment by Study Field", 
                           plotlyOutput("employment_by_field_plot", height = "400px")),
                )
              ),
              fluidRow(
                box(
                  title = "Employment Statistics",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("employment_stats_table")
                )
              )
      ),
      
      # Job Roles and Industries Tab
      tabItem(tabName = "roles",
              fluidRow(
                tabBox(
                  title = "Job Roles Analysis",
                  width = 12,
                  tabPanel("Top Job Roles", 
                           plotlyOutput("job_roles_plot", height = "400px")),
                  tabPanel("Roles by Study Field", 
                           plotlyOutput("roles_by_field_plot", height = "400px"))
                )
              ),
              fluidRow(
                tabBox(
                  title = "Industries Analysis",
                  width = 12,
                  tabPanel("Top Industries", 
                           plotlyOutput("industries_plot", height = "400px")),
                  tabPanel("Industries by Study Field", 
                           plotlyOutput("industries_by_field_plot", height = "400px"))
                )
              )
      ),
      
      # Raw Data Tab
      tabItem(tabName = "raw_data",
              fluidRow(
                box(
                  title = "Raw Survey Data",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("raw_data_table")
                )
              )
      ),
      
      # User Guide Tab
      tabItem(tabName = "guide",
              fluidRow(
                box(
                  title = "Dashboard User Guide",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  h4("Getting Started"),
                  p("This dashboard visualizes the results of a survey conducted among Eduvos IT graduates. The dashboard allows you to explore trends related to technology usage, employment outcomes, and career paths."),
                  
                  h4("Navigation"),
                  tags$ul(
                    tags$li(strong("Overview:"), " Summary statistics and key metrics from the survey"),
                    tags$li(strong("Tech Tools:"), " Analysis of programming languages, databases, web frameworks, and other technologies used by graduates"),
                    tags$li(strong("Employment:"), " Employment rates and trends across different study fields"),
                    tags$li(strong("Job Roles & Industries:"), " Analysis of job roles and industries graduates work in"),
                    tags$li(strong("Raw Data:"), " The processed survey dataset for direct examination")
                  ),
                  
                  h4("Using Filters"),
                  p("The left sidebar contains filters that can be applied across the entire dashboard:"),
                  tags$ul(
                    tags$li(strong("Campus:"), " Filter by specific Eduvos campus locations"),
                    tags$li(strong("Study Field:"), " Filter by area of study"),
                    tags$li(strong("Role:"), " Filter by current job role")
                  ),
                  p("You can select multiple filter values, or select 'All' to remove that filter. Use the 'Reset Filters' button to clear all selections."),
                  
                  h4("Interactive Features"),
                  p("Most visualizations in this dashboard are interactive. You can:"),
                  tags$ul(
                    tags$li("Hover over charts to see detailed information"),
                    tags$li("Click on elements in charts to filter the data"),
                    tags$li("Zoom in/out of charts using the toolbar"),
                    tags$li("Download visualizations as images using the toolbar")
                  ),
                  
                  h4("Data Processing Notes"),
                  p("The survey data has been processed to:"),
                  tags$ul(
                    tags$li("Standardize campus names"),
                    tags$li("Handle missing values"),
                    tags$li("Split multi-value responses (e.g., multiple programming languages per graduate)"),
                    tags$li("Calculate employment rates")
                  )
                )
              )
      )
    )
  )
)


#------------------- Server Logic -------------------#


server <- function(input, output, session) {
  
  # Load cleaned data when app starts
  data <- reactive({
    read_csv("cleaned_graduate_survey.csv")
  })
  
  # Initialize filter choices based on loaded data
  observe({
    campus_choices <- c("All", unique(data()$Campus))
    field_choices <- c("All", unique(data()$StudyField))
    role_choices <- c("All", unique(data()$Role))
    
    # Update select inputs
    updateSelectizeInput(session, "campus_filter", choices = campus_choices, selected = "All")
    updateSelectizeInput(session, "field_filter", choices = field_choices, selected = "All")
    updateSelectizeInput(session, "role_filter", choices = role_choices, selected = "All")
  })
  
  # Reset filters action
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "campus_filter", selected = "All")
    updateSelectizeInput(session, "field_filter", selected = "All")
    updateSelectizeInput(session, "role_filter", selected = "All")
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    filter_data(data(), input$campus_filter, input$field_filter, input$role_filter)
  })
  
  #------------------- Overview Tab Outputs -------------------#
  
  # Value boxes
  output$total_graduates_box <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Graduates",
      icon = icon("user-graduate"),
      color = "blue"
    )
  })
  
  output$employed_rate_box <- renderValueBox({
    employed_count <- filtered_data() %>%
      filter(grepl("Employed", Employment)) %>%
      nrow()
    
    employment_rate <- round(employed_count / nrow(filtered_data()) * 100, 1)
    
    valueBox(
      paste0(employment_rate, "%"),
      "Employment Rate",
      icon = icon("briefcase"),
      color = "green"
    )
  })
  
  output$top_study_field_box <- renderValueBox({
    top_field <- filtered_data() %>%
      count(StudyField, sort = TRUE) %>%
      slice(1)
    
    valueBox(
      top_field$StudyField[1],
      "Top Study Field",
      icon = icon("graduation-cap"),
      color = "yellow"
    )
  })
  
  output$top_role_box <- renderValueBox({
    top_role <- filtered_data() %>%
      count(Role, sort = TRUE) %>%
      slice(1)
    
    valueBox(
      top_role$Role[1],
      "Top Job Role",
      icon = icon("users"),
      color = "red"
    )
  })
  
  # Campus distribution plot
  output$campus_plot <- renderPlotly({
    campus_data <- filtered_data() %>%
      count(Campus, sort = TRUE)
    
    p <- ggplot(campus_data, aes(x = reorder(Campus, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis_c() +
      labs(x = NULL, y = "Number of Graduates") +
      theme_minimal()
    
    ggplotly(p) %>% 
      layout(margin = list(l = 100, r = 50, b = 50, t = 50))
  })
  
  # Employment overview plot
  output$employment_overview_plot <- renderPlotly({
    employment_data <- filtered_data() %>%
      mutate(
        EmploymentStatus = case_when(
          grepl("Employed", Employment, ignore.case = TRUE) ~ "Employed",
          grepl("Independent contractor|Self-employed|Freelance", Employment, ignore.case = TRUE) ~ "Self-employed",
          grepl("Not employed|Unemployed", Employment, ignore.case = TRUE) ~ "Unemployed",
          grepl("Student", Employment, ignore.case = TRUE) ~ "Student",
          is.na(Employment) | Employment == "" ~ "Unknown",
          TRUE ~ "Other"
        )
      ) %>%
      count(EmploymentStatus) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Pie chart
    plot_ly(employment_data, labels = ~EmploymentStatus, values = ~percentage, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = c("Employed" = "#4DAF4A", 
                                     "Self-employed" = "#377EB8", 
                                     "Unemployed" = "#E41A1C",
                                     "Student" = "#984EA3",
                                     "Unknown" = "#FF7F00",
                                     "Other" = "#FFFF33"))) %>%
      layout(title = "Employment Status",
             showlegend = TRUE)
  })
  
  #------------------- Tech Tools Tab Outputs -------------------#
  
  # Input selection to column names
  tech_column <- reactive({
    switch(input$tech_type,
           "Programming Languages" = "ProgLang",
           "Databases" = "Databases",
           "Cloud Platforms" = "Platform",
           "Web Frameworks" = "WebFramework",
           "AI Search Tools" = "AISearch",
           "AI Developer Tools" = "AITool")
  })
  
  # Tech tool usage plot
  output$tech_tool_plot <- renderPlotly({
    req(tech_column())
    
    tool_data <- process_multivalue_column(filtered_data(), tech_column(), top_n = 10)
    
    create_bar_chart(
      tool_data,
      paste0("Top ", input$tech_type, " Used by Graduates"),
      "Based on survey responses"
    )
  })
  
  # Tech tools by study field plot
  output$tech_by_field_plot <- renderPlotly({
    req(tech_column())
    
    # Top tools by study field
    tool_by_field <- filtered_data() %>%
      select(StudyField, all_of(tech_column())) %>%
      filter(!is.na(!!sym(tech_column())), !!sym(tech_column()) != "") %>%
      separate_rows(!!sym(tech_column()), sep = ";") %>%
      mutate(across(all_of(tech_column()), trimws)) %>%
      count(StudyField, !!sym(tech_column()), sort = TRUE) %>%
      group_by(StudyField) %>%
      top_n(5, n) %>%
      ungroup()
    
    names(tool_by_field)[2] <- "Tool"
    
    # Create plot
    p <- ggplot(tool_by_field, aes(x = reorder(Tool, n), y = n, fill = StudyField)) +
      geom_bar(stat = "identity") +
      facet_wrap(~StudyField, scales = "free_y") +
      coord_flip() +
      labs(
        title = paste0("Top ", input$tech_type, " by Study Field"),
        x = NULL,
        y = "Number of Graduates"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold")
      )
    
    ggplotly(p)
  })
  
  #------------------- Employment Tab Outputs -------------------#
  
  # Employment rate plot
  output$employment_rate_plot <- renderPlotly({
    employment_data <- filtered_data() %>%
      mutate(
        EmploymentStatus = case_when(
          grepl("Employed", Employment) ~ "Employed",
          grepl("Independent contractor", Employment) ~ "Self-employed",
          grepl("Not employed", Employment) ~ "Unemployed",
          grepl("Student", Employment) ~ "Student",
          TRUE ~ "Other"
        )
      ) %>%
      count(EmploymentStatus) %>%
      mutate(percentage = n / sum(n) * 100)
    
    # Bar chart
    p <- ggplot(employment_data, aes(x = reorder(EmploymentStatus, percentage), 
                                     y = percentage, fill = EmploymentStatus)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                hjust = -0.1) +
      scale_fill_brewer(palette = "Set2") +
      coord_flip() +
      labs(
        title = "Overall Employment Status",
        x = NULL,
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Employment by study field plot
  output$employment_by_field_plot <- renderPlotly({
    employment_by_field <- filtered_data() %>%
      mutate(
        EmploymentBinary = ifelse(
          grepl("Employed", Employment) | 
            grepl("Independent contractor", Employment), 
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
    
    # plot
    p <- ggplot(employment_by_field, aes(x = StudyField, y = Percentage, fill = EmploymentBinary)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(Percentage), "%")), 
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold") +
      scale_fill_manual(values = c("Employed" = "#4DAF4A", "Not Employed" = "#E41A1C")) +
      labs(
        title = "Employment Rate by Study Field",
        x = "Study Field",
        y = "Percentage (%)",
        fill = "Employment Status"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Employment statistics table
  output$employment_stats_table <- renderDT({
    # Create employment statistics table
    employment_stats <- filtered_data() %>%
      mutate(
        EmploymentBinary = ifelse(
          grepl("Employed", Employment) | 
            grepl("Independent contractor", Employment), 
          "Employed", 
          "Not Employed"
        )
      ) %>%
      group_by(StudyField) %>%
      summarize(
        TotalGraduates = n(),
        Employed = sum(EmploymentBinary == "Employed"),
        NotEmployed = sum(EmploymentBinary == "Not Employed"),
        EmploymentRate = round(sum(EmploymentBinary == "Employed") / n() * 100, 1)
      ) %>%
      arrange(desc(EmploymentRate))
    
    # Return formatted table
    datatable(
      employment_stats,
      caption = "Employment Statistics by Study Field",
      options = list(
        pageLength = 5,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE
      )
    )
  })
  
  #------------------- Job Roles and Industries Tab Outputs -------------------#
  
  # Top job roles plot
  output$job_roles_plot <- renderPlotly({
    roles_data <- filtered_data() %>%
      count(Role, sort = TRUE) %>%
      top_n(10, n)
    
    p <- ggplot(roles_data, aes(x = reorder(Role, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis_c() +
      labs(
        title = "Top 10 Job Roles",
        x = NULL,
        y = "Number of Graduates"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Roles by study field plot
  output$roles_by_field_plot <- renderPlotly({
    roles_by_field <- filtered_data() %>%
      count(StudyField, Role, sort = TRUE) %>%
      group_by(StudyField) %>%
      top_n(5, n) %>%
      ungroup()
    
    p <- ggplot(roles_by_field, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
      geom_bar(stat = "identity") +
      facet_wrap(~StudyField, scales = "free_y") +
      coord_flip() +
      labs(
        title = "Top Job Roles by Study Field",
        x = NULL,
        y = "Number of Graduates"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.background = element_rect(fill = "lightgreen"),
        strip.text = element_text(face = "bold")
      )
    
    ggplotly(p)
  })
  
  # Top industries plot
  output$industries_plot <- renderPlotly({
    industry_data <- filtered_data() %>%
      select(Industry) %>%
      separate_rows(Industry, sep = ";") %>%
      mutate(Industry = trimws(Industry)) %>%
      filter(!is.na(Industry), Industry != "") %>%
      count(Industry, sort = TRUE) %>%
      top_n(10, n)
    
    p <- ggplot(industry_data, aes(x = reorder(Industry, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_viridis_c() +
      labs(
        title = "Top 10 Industries",
        x = NULL,
        y = "Number of Graduates"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Industries by study field plot
  output$industries_by_field_plot <- renderPlotly({
    industry_by_field <- filtered_data() %>%
      select(StudyField, Industry) %>%
      separate_rows(Industry, sep = ";") %>%
      mutate(Industry = trimws(Industry)) %>%
      filter(!is.na(Industry), Industry != "") %>%
      count(StudyField, Industry, sort = TRUE) %>%
      group_by(StudyField) %>%
      top_n(5, n) %>%
      ungroup()
    
    p <- ggplot(industry_by_field, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
      geom_bar(stat = "identity") +
      facet_wrap(~StudyField, scales = "free_y") +
      coord_flip() +
      labs(
        title = "Top Industries by Study Field",
        x = NULL,
        y = "Number of Graduates"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold")
      )
    
    ggplotly(p)
  })
  
  #------------------- Raw Data Tab Outputs -------------------#
  
  # Raw data table
  output$raw_data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
}

#------------------- Run the Application -------------------#

shinyApp(ui = ui, server = server)
