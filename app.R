



library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)

# Reading a CSV file containing Eduvos IT graduates survey data  
# This dataset will be used for filtering and generating visualizations in the dashboard

data <- read.csv("graduate_survey1.csv")

# UI Layout for the Eduvos IT Graduates Dashboard 

ui <- fluidPage(
  titlePanel("Eduvos IT Graduates Dashboard"),
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),  
    "",
    
    # Overview Tab 
    tabPanel(tagList(icon("dashboard"), "Overview"), fluidPage(
      fluidRow(
        column(3, wellPanel(
          h4("Filter Options"),
          style = "padding: 10px; margin-bottom: 0px;",
          
          # Dropdown filters for user selection
          selectInput("campus", "Select Campus:", 
                      choices = unique(data$Campus), multiple = TRUE),
          selectInput("field", "Select Field of Study:",
                      choices = unique(data$StudyField), multiple = TRUE),
          selectInput("job", "Select Job Role:",
                      choices = unique(data$Role), multiple = TRUE),
          selectInput("industry", "Select Industry:", 
                      choices = unique(data$Industry), multiple = TRUE),
          selectInput("degree", "Select Degree Level:", 
                      choices = unique(data$EduLevel), multiple = TRUE),
          
          # Action button to generate filtered data
          actionButton("generate", "Generate Data", icon = icon("play"))
        )),
        column(8, 
               
               # Displaying key metrics in value boxes
               valueBoxOutput("total_graduates", width = 3),
               valueBoxOutput("most_used_language", width = 3),
               valueBoxOutput("popular_database", width = 3),
               valueBoxOutput("employment_status", width = 3),
               
               # Summary section containing a text output and tabbed tables
               fluidRow(
                 box(width = 12, title = "Summary", status = "primary", solidHeader = TRUE,
                     textOutput("summary_text"),
                     
                     # Tabbed panel to display different categorized tables 
                     tabsetPanel(
                       tabPanel(tagList(icon("laptop-code"),"Languages"), 
                                DTOutput("language_table")),
                       
                       tabPanel(tagList(icon("database"), "Databases"), 
                                DTOutput("database_table")),
                       
                       tabPanel(tagList(icon("laptop"), "Web Frameworks"),
                                DTOutput("web_table")),
                       
                       tabPanel(tagList(icon("cloud"), "Cloud Platforms"),
                                DTOutput("cloud_table")),
                       
                       tabPanel(tagList(icon("robot"), "AI Tools"), 
                                DTOutput("ai_table")),
                       
                       tabPanel(tagList(icon("user-tie"), "Employment"),
                                DTOutput("employment_table"))
                       
                     )
                 )
               )
        )
      )
    )),
    
    # This code defines tabbed panels for different dashboard visualizations.  
    # Each tab contains a Plotly output for a specific data category.  
    
     
    tabPanel(tagList(icon("chart-bar"), "Programming Languages"),
             fluidPage(plotlyOutput("lang_plot", height = "700px"))),
    
    tabPanel(tagList(icon("chart-pie"), "Databases"), 
             fluidPage(plotlyOutput("db_plot", height = "600px"))),
    
    tabPanel(tagList(icon("chart-line"), "Web Frameworks"),
             fluidPage(plotlyOutput("web_plot", height = "600px"))),
    
    tabPanel(tagList(icon("chart-line"), "Cloud Platforms"),
             fluidPage(plotlyOutput("cloud_plot", height = "600px"))),
    
    tabPanel(tagList(icon("chart-bar"), "Job Roles"), 
             fluidPage(plotlyOutput("job_plot", height ="600px"))),
    
    tabPanel(tagList(icon("chart-area"), "AI Tools"), 
             fluidPage(plotlyOutput("ai_tools_plot"))),
    
    tabPanel(tagList(icon("chart-bar"), "Employment"),
             fluidPage(plotlyOutput("employment_plot")))
    
  )
)


# Server Logic
# This code defines a reactive function to filter data based on user input selections.  
# It dynamically updates the dataset based on selected Campus, Study Field, Job Role, and Degree Level.  

server <- function(input, output) {
  
  # Reactive function to filter data based on user input selections 
  filtered_data <- reactive({
    data %>%
      filter(
        (Campus %in% input$campus | length(input$campus) == 0) &
          (StudyField %in% input$field | length(input$field) == 0) &
          (Role %in% input$job | length(input$job) == 0) &
          (EduLevel %in% input$degree | length(input$degree) == 0)
      )
  })
  
  # Summary Text Output
  
  # This code generates a dynamic summary text based on user-selected filters.  
  # It displays the total number of graduates along with their campus, field of study, job roles, industry, and degree level.
  # Displays an overview of the filtered data
  
  output$summary_text <- renderText({
    req(input$generate)
    df <- filtered_data()
    total <- nrow(df)
    
    # Formatting selected filter values for display 
    campus_selected <- ifelse(length(input$campus) > 0, paste(input$campus, collapse = ", "), "All Campuses")
    field_selected <- ifelse(length(input$field) > 0, paste(input$field, collapse = ", "), "All Fields")
    job_selected <- ifelse(length(input$job) > 0, paste(input$job, collapse = ", "), "All Job Roles")
    industry_selected <- ifelse(length(input$industry) > 0, paste(input$industry, collapse = ", "), "All Industries")
    degree_selected <- ifelse(length(input$degree) > 0, paste(input$degree, collapse = ", "), "All Degrees")
    
    # Generating the summary text 
    HTML(paste0(
      "There are ", total, " graduates in ", campus_selected, ". ",
      "They studied ", field_selected, " and are working as ", job_selected, " in ", industry_selected, ". ",
      "Their education level is ", degree_selected, ". ",
      "CLICK ON THE TABS IN THE PAGE HEADER TO VIEW THE GRAPHS"
    ))
  })
  
  # This code generates summary tables for various categories,  
  # displaying key insights about database usage, programming languages, web frameworks, cloud platforms, AI tools, and employment distribution.  
  # Database Table Output - Displays a summary of database usage 
  
  output$database_table <- renderDT({
    req(input$generate)
    df <- filtered_data()
    db_counts <- table(unlist(strsplit(paste(df$Databases, collapse = ";"), ";")))
    db_df <- data.frame(Database = names(db_counts), Count = as.numeric(db_counts))
    datatable(db_df, options = list(pageLength = 5))
  })
  
  # Programming Languages Table - Displays a summary of programming language usage 
  output$language_table <- renderDT({
    req(input$generate)
    df <- filtered_data()
    lang_counts <- table(unlist(strsplit(paste(df$ProgLang, collapse = ";"), ";")))
    lang_df <- data.frame(Language = names(lang_counts), Count = as.numeric(lang_counts))
    datatable(lang_df, options = list(pageLength = 5))
  })
  
  # Web Frameworks Table - Displays a summary of web framework usage
  output$web_table <- renderDT({
    req(input$generate)
    df <- filtered_data()
    web_counts <- table(unlist(strsplit(paste(df$WebFramework, collapse = ";"), ";")))
    web_df <- data.frame(WebFrameworks = names(web_counts), Count = as.numeric(web_counts))
    datatable(web_df, options = list(pageLength = 5))
  })
  
  # Cloud Platforms Table - Displays a summary of cloud platform usage
  output$cloud_table <- renderDT({
    req(input$generate)
    df <- filtered_data()
    cloud_counts <- table(unlist(strsplit(paste(df$Platform, collapse = ";"), ";")))
    cloud_df <- data.frame(CloudPlatform = names(cloud_counts), Count = as.numeric(cloud_counts))
    datatable(cloud_df, options = list(pageLength = 5))
  })
  
  # AI Tools Table - Displays a summary of AI tools used by graduates 
  output$ai_table <- renderDT({
    req(input$generate)
    df <- filtered_data()
    ai_counts <- table(unlist(strsplit(paste(df$AITool, collapse = ";"), ";")))
    ai_df <- data.frame(AITool = names(ai_counts), Count = as.numeric(ai_counts))
    datatable(ai_df, options = list(pageLength = 5))
  })
  
  # Employment Table - Displays employment distribution across industries 
  output$employment_table <- renderDT({
    req(input$generate)
    df <- filtered_data()
    
    employment_summary <- df %>%
      group_by(Industry, Employment) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    datatable(employment_summary, options = list(pageLength = 5))
  
  })

  #Displays the total number of graduates based on the selected filter
  # The value box upgrades dynamically when the filters change 
  
  output$total_graduates <- renderValueBox({
    req(input$generate)
    valueBox(nrow(filtered_data()), "Total Graduates", icon = icon("users"), color = "blue")
  })
  
  #This code slits multiple values in one column
  #It counts occurances of each category and plots data selected by the user
  #This plot Visualizes the most used programming languages 
  
  # Programming language Plot
  output$lang_plot <- renderPlotly({
    req(input$generate)
    lang_counts <- table(unlist(strsplit(paste(filtered_data()$ProgLang, collapse = ";"), ";")))
    lang_df <- data.frame(Language = names(lang_counts), Count = as.numeric(lang_counts))
    
    Lang_graph <- ggplot(lang_df, aes(x = reorder(Language, Count), y = Count, fill = Language)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Most Used Programming Languages", x = "Language", y = "Count")+
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 10))
    
    ggplotly(Lang_graph)
  })
  
  # This code processes and visualizes the most used databases.  
  # It splits multiple database entries stored in one column, counts occurrences,  
  # and generates a bar chart displaying database usage.
  
  # Database output Plot 
  output$db_plot <- renderPlotly({
    req(input$generate)
    db_counts <- table(unlist(strsplit(paste(filtered_data()$Databases, collapse = ";"), ";")))
    db_df <- data.frame(Database = names(db_counts), Count = as.numeric(db_counts))
    
    Database_graph <- ggplot(db_df, aes(x = reorder(Database, -Count), y = Count, fill = Database)) +
      geom_bar(stat = "identity") + 
      coord_flip() + 
      theme_minimal() +
      labs(title = "Most Used Databases", x = "Database", y = "Count")+
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    
    ggplotly(Database_graph)
    
  })
  
  # This code processes and visualizes the most used cloud platforms.  
  # It splits multiple platform entries stored in one column, counts occurrences,  
  # and generates a bar chart displaying cloud platform usage.  
  
  # Cloud Platforms Plot
  output$cloud_plot <- renderPlotly({
    req(input$generate)
    cloud_counts <- table(unlist(strsplit(paste(filtered_data()$Platform, collapse = ";"), ";")))
    cloud_df <- data.frame(CloudPlatform = names(cloud_counts), Count = as.numeric(cloud_counts))
    
    Cloud_graph <- ggplot(cloud_df, aes(x = reorder(CloudPlatform, -Count), y = Count, fill = CloudPlatform)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_minimal() +
      labs(title = "Most Used Cloud Platforms", x = "Cloud Platform", y = "Count")+
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    
    ggplotly(Cloud_graph)
  })
  
  # This code processes and visualizes employment distribution across industries.  
  # It creates a grouped bar chart showing employment status within each industry.  
  
  
  #Employment data Plot 
  output$employment_plot <- renderPlotly({
    req(input$generate)
    emp_data <- filtered_data()
    
    emp_graph <- ggplot(emp_data, aes(x = Industry, fill = Employment)) +  
      geom_bar(position = "dodge") +
      theme_minimal() +
      scale_fill_manual(values = c( "#1F77B4", "#3293ce", "#00c0ef", "#7F7F7F"))+
      labs(title = "Employment Distribution by Industry", x = "Industry", y = "Count") +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))  
    
    ggplotly(emp_graph)
    
  })
  
  # This code processes and visualizes job roles by industry.  
  # It creates a stacked bar chart displaying the distribution of job roles across industries.  
  
  #Job Roles Graph 
  output$job_plot <- renderPlotly({
    req(input$generate)
    job_data <- filtered_data()
    
    job_graph <- ggplot(job_data, aes(x = Industry, fill = Role)) +  
      geom_bar(position = "stack") +
      theme_minimal() +
      labs(title = "Job Roles by Industry", x = "Industry", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            legend.position = "bottom",  
            legend.text = element_text(size = 9))   
    
    ggplotly(job_graph)
  })
  
  # This code processes and visualizes employment details for full-time employed graduates.  
  # It creates a grouped bar chart showing employment distribution by campus and study field.  
  
  #Employment details Plot 
  output$employment_details_plot <- renderPlotly({
    req(input$generate)
    emp_data <- filtered_data() %>% filter(Employment == "Employed Full-Time")
    
    Emp_details_graph <- ggplot(emp_data, aes(x = Campus, fill = StudyField)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Employment Distribution by Campus and Study Field", x = "Campus", y = "Count")+
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    
    ggplotly(Emp_details_graph)
  })
  
  # This code processes and visualizes the most used web frameworks.  
  # It splits multiple framework entries stored in one column, counts occurrences,  
  # and generates a bar chart displaying web framework usage.  
  
  # Web Frameworks Plot
  output$web_plot <- renderPlotly({
    req(input$generate)
    web_counts <- table(unlist(strsplit(paste(filtered_data()$WebFramework, collapse = ";"), ";")))
    web_df <- data.frame(Framework = names(web_counts), Count = as.numeric(web_counts))
    
    Web_graph <- ggplot(web_df, aes(x = reorder(Framework, -Count), y = Count, fill = Framework)) +
      geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() +
      labs(title = "Most Used Web Frameworks", x = "Framework", y = "Count")+
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    ggplotly(Web_graph)
  })
  
  # This code processes and visualizes the most used AI tools.  
  # It splits multiple AI tool entries stored in one column, counts occurrences,  
  # and generates a bar chart displaying AI tool usage.
  
  # AI Tools Plot
  output$ai_tools_plot <- renderPlotly({
    req(input$generate)
    ai_counts <- table(unlist(strsplit(paste(filtered_data()$AITool, collapse = ";"), ";")))
    ai_df <- data.frame(AITool = names(ai_counts), Count = as.numeric(ai_counts))
    
    Ai_graph <- ggplot(ai_df, aes(x = reorder(AITool, -Count), y = Count, fill = AITool)) +
      geom_bar(stat = "identity") + 
      theme_minimal() +
      scale_fill_manual(values = c( "#1F77B4", "#aebbff", "#92b2ff", "#7F7F7F","blue","#3a6dcf","#5ab6f5","#6cb2b8","#56b1d4","#56b","#bbbbbb"))+
      labs(title = "Most Used AI Tools", x = "AI Tool", y = "Count")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) 
    
    ggplotly(Ai_graph)
  })
}

# This code launches the Shiny application.  
# It initializes the UI and server components, allowing user interaction with the dashboard.  

shinyApp(ui = ui, server = server)
