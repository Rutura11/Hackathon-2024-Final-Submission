
##  Data Analysis Libraries
library(shiny)
library(shinydashboard)
library(highcharter)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalt)
library(plotly)
library(rsconnect)


# datasets used
file_path <- "C:/Users/highe/Desktop/Youth Challenges Dash"

youth_employment2 <- read_excel("youth employment2.xlsx")
Poverty_status <- read_excel("Poverty status.xlsx")
Sector_distribution <- read_excel("Sector distribution.xlsx")
Industry_distribution1 <- read_excel("Industry distribution1.xlsx")
Regional_Employment <- read_excel("Regional Employment.xlsx")
Impact_levels <- read_excel("Impact levels.xlsx")
Employment_Barriers <- read_excel("Employment Barriers.xlsx")
Underemployment_rates <- read_excel("Underemployment rates.xlsx")
Financially_rates <- read_excel("Financially rates.xlsx")
employment_status_by_region <- read_excel("employment status by region.xlsx")
Regional_education_and_employment_rates <- read_excel("Regional education and employment rates.xlsx")
Targets_job <- read_excel("Targets job.xlsx")
Vocational_training_role <- read_excel("Vocational training role.xlsx")
Enrollment_over_time <- read_excel("Enrollment over time.xlsx")
Programs_progress <- read_excel("Programs progress.xlsx")
Empowerment_Programs1 <- read_excel("Empowerment Programs1.xlsx")
Outcomes_and_Strategies <- read_excel("Outcomes and Strategies.xlsx")
employment_poverty_status <- read_excel("employment poverty status.xlsx")
Education_and_Skills <- read_excel("Education and Skills.xlsx")
Youth_Health <- read_excel("Youth Health.xlsx")
Finance_and_Business <- read_excel("Finance and Business.xlsx")
Employment_and_Regional_Distribution <- data.frame( 
  Region = c("Kigali", "Northern", "Southern", "Eastern", "Western"), 
  Employment_Rate = c(65, 50, 55, 60, 58), 
  Unemployment_Rate = c(20, 25, 22, 18, 20), 
  Underemployment_Rate = c(15, 25, 23, 22, 22) 
)
employment_status <- read_excel("employment_status.xlsx")
access_finance <- read_excel("access_finance.xlsx")
skills_development <- read_excel("skills_development.xlsx")

## Main Calculation
# cumulative progress towards the 1.5 million jobs target
Targets_job <- Targets_job %>%
  mutate(Total_Jobs_Created = Agriculture + Services + Industry + Construction,
         Cumulative_Jobs = cumsum(Total_Jobs_Created))
print(Targets_job)  # Print to check values
Empowerment_Programs1 <- Empowerment_Programs1 %>%
  mutate(across(starts_with("Funding"), ~ as.numeric(gsub(",", "", .))))


## Dashboard user interface

ui <- dashboardPage(title = "demo app", skin = "purple",
                    
                    # dashboard Header                                       
                    dashboardHeader(
                      title = "Youth Challenges in Rwanda", 
                      titleWidth = 400,
                      
                      ## A dropdown menu for data sources
                      #NISR data source
                      dropdownMenu(
                        type = "tasks", 
                        badgeStatus = NULL,
                        icon = icon("database", title = "NISR"),
                        headerText = "NISR EICV5 youth report",
                        tags$li(a(href = "https://statistics.gov.rw/publication/eicv5thematic-reportyouth",
                                  target = "_blank",
                                  "Click here to view data sources",
                                  class = "dropdown-item"),
                                class = "dropdown-item")),
                      
                      
                      #NYC Data source
                      dropdownMenu(
                        type = "tasks", 
                        badgeStatus = NULL,
                        icon = icon("poll", title = "NYC Report(2021-2025)"),
                        headerText = "NYC Report(2021-2025)",
                        tags$li(a(href = "https://www.nyc.gov.rw/publications?tx_filelist_filelist%5Baction%5D=list&tx_filelist_filelist%5Bcontroller%5D=File&tx_filelist_filelist%5Bpath%5D=%2Fuser_upload%2FNYC%2FPublications%2FStrategic_plan%2F&cHash=8739f80889f0bd92faf012611312138a",
                                  target = "_blank",
                                  "Click here to view data sources",
                                  class = "dropdown-item"),
                                class = "dropdown-item")),
                      
                      
                      #github Repository
                      dropdownMenu(
                        type = "tasks", 
                        badgeStatus = NULL,
                        icon = icon("github", title = "GitHub Repository"),
                        headerText = "GitHub Repository",
                        tags$li(a(href = "https://github.com/Rutura11/Hackathon-2024-Final-Submission",
                                  target = "_blank",
                                  "Click here to visit Repository",
                                  class = "dropdown-item"),
                                class = "dropdown-item"))),
                    
                    # dashboard Sidebar items
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "Home"),
                        
                        menuItem("Overview", tabName = "YEO", icon = icon("chart-bar"),
                                 menuSubItem("Introduction", tabName = "INT"),
                                 menuSubItem("Unemployment and Poverty", tabName = "EPL"),
                                 menuSubItem("Job Distribution", tabName = "JDS"),
                                 menuSubItem("Regional Employment", tabName = "RED")),
                        
                        menuItem("Challenges", tabName = "CFY", icon = icon("exclamation-triangle"),
                                 menuSubItem("Causes of Unemployment", tabName = "COU"),
                                 menuSubItem("At-Risk Groups", tabName = "ARG"),
                                 menuSubItem("Regional Variations", tabName = "RV")),
                        
                        menuItem("Solutions", tabName = "SO", icon = icon("lightbulb"),
                                 menuSubItem("Improving Employment", tabName = "IE"),
                                 menuSubItem("Vocational Training", tabName = "RVT"),
                                 menuSubItem("Empowerment Programs", tabName = "EP")),
                        
                        menuItem("Interactive Insights", tabName = "IS", icon = icon("chart-line"),
                                 menuSubItem("Explore the Data", tabName = "ETD"),
                                 menuSubItem("Recommended Actions", tabName = "RA")),
                        menuItem("Predictive Model", tabName = "PM", icon = icon("magic"),
                                 menuSubItem("Unemployment Prediction", tabName = "UP")
                        )
                      ), width = 220
                    ),
                    
                    # dashboardBody
                    
                    dashboardBody(
                      tags$style(HTML("
    .justified-text {
      text-align: justify;
    }
  ")),
                      tabItems(
                        tabItem(tabName = "Home",
                                fluidPage(box(
                                  tagList(title = "", icon = icon("home")),
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = 12,
                                  height = 600,
                                  img(src = "Welkie pic.jpg", height = "550px", width = "100%", alt = "Welcome to the dashboard")
                                ))),
                        
                        tabItem(tabName = "INT",
                                fluidPage(
                                  box(title = tagList(icon("book-open"), "Introduction To dashboard"), 
                                      width = 12, height = 600, solidHeader = TRUE, status = "info",
                                      h1("RWANDAN YOUTH CHALLENGES: DATA SCIENCE DASHBOARD"),
                                      p("This interactive dashboard provides a comprehensive overview of key issues affecting youth across Rwanda. 
                      It consolidates essential data on youth employment, poverty, education, health, and financial inclusion 
                      to help policymakers, researchers, and community members understand the current landscape and identify 
                      areas for targeted intervention.", class = "justified-text"),
                                      p("This dashboard integrates data from NISR and National Youth Center to deliver an engaging, 
                      user-friendly experience. Users can interactively explore critical metrics, trends, 
                      and visualizations to better understand the challenges facing Rwandan youth today. 
                      Key areas of focus include employment and skills development, educational attainment, digital literacy, health 
                      and wellbeing, and financial inclusion.", class = "justified-text"),
                                      p("Explore the data, uncover insights, and join us in fostering solutions that will support Rwanda’s young generation for a brighter future.", class = "justified-text")
                                  )
                                )
                        ),
                        tabItem(tabName = "EPL",
                                fluidPage(box(width = 12,
                                              tabBox(
                                                id = "tabset1",
                                                height = "300px",
                                                width = 20,
                                                tabPanel("Youth Economic Status",
                                                         fluidRow(
                                                           box(selectInput("status", "Select Youth Category:", choices = unique(youth_employment2$Indicator1)), width = 4),
                                                           valueBox("79.9%", "Usually", color = "blue", icon = shiny::icon("user-check")),
                                                           valueBox("20.1%", "Inactive", color =  "red", icon = shiny::icon("ban")),
                                                           box(highchartOutput("barChart"), width = 12))
                                                ),
                                                tabPanel("Poverty Status",
                                                         fluidRow(
                                                           box(highchartOutput("povertyChart"), width = 12, height = "600px")
                                                         )
                                                )
                                              ) )
                                )),
                        tabItem(tabName = "JDS",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20,
                                        tabPanel("Main usual jobs of youth",
                                                 fluidRow(
                                                   box(selectInput("employment_type", "Select Main Job Type:", 
                                                                   choices = colnames(Sector_distribution)[2:6]), width = 4),
                                                   infoBox("working youth", value = "2443", subtitle = "Toatal number of working youth EICV5", icon = shiny::icon("briefcase"), width = 7),
                                                   box(highchartOutput("sectorChart"), width = 12)
                                                 )
                                        ),
                                        tabPanel("Youth Job Distribution by Indusrty",
                                                 fluidRow(
                                                   box(selectInput("age_group", "Select Age Group:", 
                                                                   choices = unique(Industry_distribution1$Indicator4)), width = 4),
                                                   box(highchartOutput("industryChart"), width = 12,)
                                                 )
                                        )
                                      ))
                                )),
                        tabItem(tabName = "RED",
                                fluidPage(
                                  box(title = "Regional Employment", width = 12, 
                                      highchartOutput("tileChart", height = "500px"))
                                )),
                        tabItem(tabName = "COU",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20,
                                        tabPanel("Youth Employment Barriers by Region",
                                                 fluidRow(
                                                   box(width = 12,
                                                       highchartOutput("radarChart", height = "500px")))),
                                        
                                        tabPanel("Youth employment Barriers by Impact Levels",
                                                 fluidRow( box(width = 12, height = 600,
                                                               highchartOutput("lollipopChart", height = "500px")))
                                        )
                                      ))
                                )),
                        tabItem(tabName = "ARG",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20, 
                                        tabPanel("Financial exclusion rates",
                                                 fluidRow(
                                                   box(width = 12,
                                                       highchartOutput("financialExclusionPieChart", height = "500px")))),
                                        
                                        tabPanel("Underemployement rates",
                                                 fluidRow(
                                                   box(width = 12,
                                                       highchartOutput("underemploymentChart", height = "500px"))
                                                 )
                                        )
                                      )
                                  ))
                        ),
                        tabItem(tabName = "RV",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20,
                                        tabPanel("Regional Education and Employment Rates",
                                                 fluidRow(
                                                   box(width = 12, height = 600,
                                                       highchartOutput("educationEmploymentChart", height = "500px")))),
                                        
                                        tabPanel("Regional Employment Status",
                                                 fluidRow(
                                                   box(width = 12, height = 600,
                                                       highchartOutput("barChart1", height = "500px"))
                                                 )
                                        )
                                      )
                                  )
                                )),
                        tabItem(tabName = "IE",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20,
                                        tabPanel("Targets job",
                                                 fluidRow(
                                                   box(width = 3, selectInput("column_select", "Select Sector", choices = c("Agriculture", "Services", "Industry", "Construction"),
                                                                              selectize = TRUE, width = "150px"),  
                                                       
                                                       tags$style(HTML("
        .selectize-input { font-size: 14px; height: 30px; }  /* Adjust dropdown font and height */
      "))),
                                                   
                                                   infoBox("Decent Employment ", value = "1,500,000 jobs", subtitle = "Total Job Creation Target", icon = shiny::icon("users"), width = 4),
                                                   infoBox("Productive Jobs", value = "214,000 jobs/year", subtitle = "Annual Job Creation Target", icon = shiny::icon("calendar-alt"), width = 4),
                                                   box(highchartOutput("waterfallChart", height = "550px", width = "100%"), width = 12, height = 550))),
                                        
                                        tabPanel("Job creation goals",
                                                 fluidRow(
                                                   DT::dataTableOutput("jobCreationTable")
                                                 )
                                        )
                                      )
                                  )
                                )),
                        tabItem(tabName = "RVT",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20, 
                                        tabPanel("Role of Vocational Training",
                                                 fluidRow(
                                                   box(highchartOutput("vocationalTrainingChart"), width = 12)
                                                 )),
                                        tabPanel("Enrollment over time",
                                                 fluidRow(
                                                   box(width = 12, height = 600,
                                                       highchartOutput("enrollmentLineChart", height = "500px"))))
                                      )
                                  )
                                )),
                        tabItem(tabName = "EP",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20,
                                        tabPanel("Program progress",
                                                 fluidRow(
                                                   box(width = 12, height = 600,
                                                       plotOutput("bulletChart")))),
                                        tabPanel("Empowerment programs",
                                                 fluidRow(
                                                   box(width = 12, height = 600,
                                                       box(selectInput("year_select", "Select Funding Year:", 
                                                                       choices = c("Funding2021", "Funding2022", "Funding2023")), width = 4),
                                                       box(highchartOutput("empowermentPieChart"), width = 12, height = "500px")
                                                       
                                                   ))),
                                        tabPanel("Outcomes and Strategies",
                                                 fluidRow(
                                                   box(width = 12, height = 600,
                                                       
                                                       box(selectInput("priority_area", "Select Priority Area:", 
                                                                       choices = unique(Outcomes_and_Strategies$`Priority area`)), 
                                                           width = 6),
                                                       box(highchartOutput("outcomeStrategyChart"), width = 12, height = "600px")
                                                   )))
                                      ))
                                )),
                        tabItem(tabName = "ETD",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(  id = "tabset1",
                                               height = "220px",
                                               width = 20,
                                               tabPanel(" Youth Employment and Poverty Status",
                                                        fluidRow(
                                                          # Dropdown for selecting region
                                                          box(selectInput("region", "Select Region", choices = c("All", unique(employment_poverty_status$Region))), width = 3)
                                                        ),
                                                        fluidRow(
                                                          box(highchartOutput("employmentPovertyChart"), width = 12)
                                                        )
                                               ),
                                               tabPanel("Youth Education and Skills Development",
                                                        fluidRow(
                                                          # Dropdown for selecting education level
                                                          box(selectInput("education_level", "Select Education Level", 
                                                                          choices = c("All", unique(Education_and_Skills$`Education Level`))), width = 5),
                                                          # Dropdown for selecting gender
                                                          box(selectInput("gender", "Select Gender", 
                                                                          choices = c("All", "Male (%)", "Female (%)")), width = 3)
                                                        ),
                                                        fluidRow(
                                                          box(highchartOutput("educationSkillsChart"), width = 12)
                                                        )
                                                        
                                               ),
                                               tabPanel("Youth Health and Wellbeing",
                                                        fluidRow(
                                                          box(selectInput("health_metric", "Select Health Metric", 
                                                                          choices = c("All", unique(Youth_Health$`Health Metric`))), width = 4)
                                                        ),
                                                        fluidRow(
                                                          box(highchartOutput("healthChart"), width = 12)
                                                        )
                                               ),
                                               tabPanel("Financial Inclusion and Entrepreneurship",
                                                        fluidRow(
                                                          # Dropdown for selecting finance metric
                                                          box(selectInput("finance_metric", "Select Financial Metric", 
                                                                          choices = unique(Finance_and_Business$`Finance and Business Metric`)), width = 5)
                                                        ),
                                                        fluidRow(
                                                          box(highchartOutput("financeChart"), width = 12)
                                                        )
                                               ),
                                               tabPanel("Youth ICT and Digital Literacy by Region",
                                                        fluidRow(
                                                          box(width = 12,
                                                              highchartOutput("digitalLiteracyChart")
                                                          ))
                                               )
                                      ))
                                )),
                        tabItem(tabName = "RA",
                                fluidPage(
                                  h2("Recommended Actions"),
                                  
                                  # Employment and Skills Development Section
                                  box(title = tagList(icon("briefcase"), "Employment and Skills Development"), 
                                      width = 12, solidHeader = TRUE, status = "primary",
                                      p("Expand Vocational Training to rural areas."),
                                      p("Support youth entrepreneurship with financial and mentorship programs."),
                                      p("Strengthen digital literacy initiatives for the youth.")
                                  ),
                                  
                                  # Education and Digital Literacy Section
                                  box(title = tagList(icon("graduation-cap"), "Education and Digital Literacy"), 
                                      width = 12, solidHeader = TRUE, status = "success",
                                      p("Increase access to secondary and tertiary education."),
                                      p("Expand digital training and internet access, especially in rural areas.")
                                  ),
                                  
                                  # Health and Wellbeing Section
                                  box(title = tagList(icon("heartbeat"), "Health and Wellbeing"), 
                                      width = 12, solidHeader = TRUE, status = "warning",
                                      p("Improve access to reproductive health services."),
                                      p("Promote health insurance coverage among youth.")
                                  ),
                                  
                                  # Financial Inclusion Section
                                  box(title = tagList(icon("piggy-bank"), "Financial Inclusion"), 
                                      width = 12, solidHeader = TRUE, status = "info",
                                      p("Expand financial literacy programs in schools and communities."),
                                      p("Reduce financial exclusion by promoting accessible banking and savings options.")
                                  ),
                                  
                                )),
                        tabItem(tabName = "UP",
                                fluidPage(
                                  box(width = 12,
                                      tabBox(
                                        id = "tabset1",
                                        height = "300px",
                                        width = 20,
                                        tabPanel("Unemployment rate Prediction",
                                                 fluidRow(
                                                   box(
                                                     title = tagList(icon("chart-line"), "Predict Youth Unemployment"),
                                                     width = 12, solidHeader = TRUE, status = "danger",
                                                     fluidRow(
                                                       column(
                                                         width = 4,
                                                         selectInput(
                                                           "region", 
                                                           "Select Region:", 
                                                           choices = unique(Employment_and_Regional_Distribution$Region)
                                                         ),
                                                         actionButton("predict", "Predict Unemployment Rate", class = "btn-primary")
                                                       ),
                                                       column(
                                                         width = 8,
                                                         verbatimTextOutput("predictionResult"),
                                                         highchartOutput("plot", height = "500px")
                                                       )
                                                     )
                                                   )))
                                      ))
                                ))
                      )
                    ))

## Dashboard server
server <- function(input, output) {
  
  
  # Logic for youth employment barChart
  output$barChart <- renderHighchart({
    selected_status <- input$status
    filtered_data <- youth_employment2[youth_employment2$Indicator1 == selected_status, ]
    
    # Extract years and values for the selected indicator
    years <- names(filtered_data)[!names(filtered_data) %in% c("Indicator1")]
    values <- as.numeric(filtered_data[1, -1])  # Assuming the first row contains the values
    
    # Create a bar chart with larger tooltip label
    highchart() %>%
      hc_chart(type = 'bar') %>%
      hc_title(text = paste("Youth Category:", selected_status)) %>%
      hc_xAxis(categories = years) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_add_series(name = selected_status, data = values)
  })
  
  
  #Logic  for Poverty Status Across Age Groups stacked bar chart
  output$povertyChart <- renderHighchart({
    # Create the stacked bar chart
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Poverty Status Across Age Groups") %>%
      hc_xAxis(categories = Poverty_status$Indicator2, title = list(text = "Age Group")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_add_series(name = "Below Poverty Line", data = Poverty_status$`Below Poverty Line`, stack = "poverty", 
                    color = "#1f77b4", dataLabels = list(enabled = TRUE, format = '{point.y}%')) %>%
      hc_add_series(name = "Extrem Poverty Line", data = Poverty_status$`Extrem Poverty Line`, stack = "poverty", 
                    color = "#ff7f0e", dataLabels = list(enabled = TRUE, format = '{point.y}%')) %>%
      hc_add_series(name = "Above Poverty Line", data = Poverty_status$`Above Poverty Line`, stack = "poverty", 
                    color = "#2ca02c", dataLabels = list(enabled = TRUE, format = '{point.y}%')) %>%
      hc_plotOptions(series = list(stacking = "normal", 
                                   events = list(click = JS("function(event) { 
                                     var chart = this.chart;
                                     var total = 0;
                                     chart.series.forEach(function(series) {
                                       total += series.data[event.point.index].y;
                                     });
                                     alert('Total: ' + total + '%');
                                   }")))) %>%
      hc_tooltip(shared = TRUE, valueSuffix = "%")
  })
  
  
  # Logic for Job Distribution across Sectors
  output$sectorChart <- renderHighchart({
    selected_employment_type <- input$employment_type
    
    # Extract values for all age groups for the selected employment type
    age_groups <- Sector_distribution$Indicator3
    values <- Sector_distribution[[selected_employment_type]]
    
    # Create a line chart
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = paste("Main Job Type for", selected_employment_type)) %>%
      hc_xAxis(categories = age_groups, title = list(text = "Age Group")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_add_series(name = selected_employment_type, data = values, color = "#28a745", marker = list(enabled = TRUE))  # Set color to green
  })
  
  
  # Logic for Youth Job Distribution by Industry
  output$industryChart <- renderHighchart({
    selected_age_group <- input$age_group
    
    # Filter data based on selected age group
    filtered_data <- Industry_distribution1 %>% filter(Indicator4 == selected_age_group)
    
    # Prepare data for donut chart
    categories <- colnames(filtered_data)[2:ncol(filtered_data)]
    values <- as.numeric(filtered_data[1, 2:ncol(filtered_data)])
    
    # Create a donut chart
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = paste("job Industry by Age Group:", selected_age_group)) %>%
      hc_plotOptions(pie = list(
        innerSize = "60%",
        dataLabels = list(enabled = TRUE, format = '{point.name}: {point.y:.1f}%')
      )) %>%
      hc_add_series(
        name = "Industry Distribution",
        data = list_parse2(data.frame(name = categories, y = values))
      )
  })
  
  
  # Logic for Regional Employment Disparities
  output$tileChart <- renderHighchart({
    # Ensure numeric columns are selected and reshaped
    data <- Regional_Employment %>%
      select(Region, where(is.numeric)) %>%  # Select numeric columns along with the Region column
      pivot_longer(cols = -Region, names_to = "Indicator", values_to = "Value")  # Reshape data to long format
    
    # Create a tile chart
    highchart() %>%
      hc_chart(type = "heatmap") %>%
      hc_title(text = "Regional Employment Disparities") %>%
      hc_xAxis(categories = unique(data$Region), title = list(text = "Region")) %>%
      hc_yAxis(categories = unique(data$Indicator), title = list(text = "Indicator")) %>%
      hc_add_series(
        name = "Employment Data",
        data = list_parse2(data.frame(
          x = as.numeric(factor(data$Region)) - 1,
          y = as.numeric(factor(data$Indicator)) - 1,
          value = data$Value,
          Region = data$Region  # Add Region column for tooltip
        )),
        borderWidth = 1,
        dataLabels = list(
          enabled = TRUE,
          format = '{point.Region}: {point.value}'  # Display Region and Value in each tile
        )
      ) %>%
      hc_colorAxis(stops = color_stops(n = 5, colors = c("#d62728", "#ff7f0e", "#2ca02c", "#1f77b4", "#9467bd"))) %>%
      hc_tooltip(
        pointFormat = "{point.Region}<br>Value: {point.value}"
      )
    
  })
  
  
  # Logic for Impact level on Causes of Unemployment 
  output$lollipopChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column", spacingBottom = 80) %>%
      hc_title(text = "Relative Impact Levels") %>%
      hc_subtitle(text = "Legend: 1 = Low, 2 = Medium, 2.5 = Medium-High, 3 = High") %>%
      hc_xAxis(categories = Impact_levels$Barrier, title = list(text = "Barrier")) %>%
      hc_yAxis(title = list(text = "Impact Level"), min = 0, max = 3) %>%
      hc_add_series(data = Impact_levels$`Impact Level`, type = "column", name = "Impact Level", color = "blue") %>%
      hc_legend(layout = "horizontal", align = "center", verticalAlign = "top", y = 30, floating = FALSE, itemStyle = list(fontSize = "12px"), padding = 20) %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b><br/>Impact Level: {point.y}")
  })
  
  # Logic Youth Employment Barriers by Region
  output$radarChart <- renderHighchart({
    data <- Employment_Barriers %>%
      pivot_longer(cols = c("Urban (%)", "Rural (%)"), names_to = "Area", values_to = "Impact")
    
    # Create the radar chart with specified colors and without axis titles
    highchart() %>%
      hc_chart(polar = TRUE, type = "line") %>%
      hc_title(text = "Barriers by Regional", style = list(fontSize = "20px")) %>%
      hc_xAxis(categories = unique(data$Barrier), 
               title = NULL,  # Removed title for xAxis
               labels = list(style = list(fontSize = "14px"))) %>%
      hc_yAxis(min = 0, max = 100, 
               title = NULL,  # Removed title for yAxis
               labels = list(style = list(fontSize = "12px"))) %>%
      hc_add_series(name = "Urban (%)", data = data %>% filter(Area == "Urban (%)") %>% pull(Impact), 
                    pointPlacement = "on", color = "#FFD700",  # Yellow color
                    dataLabels = list(enabled = TRUE, format = '{point.y}%')) %>%
      hc_add_series(name = "Rural (%)", data = data %>% filter(Area == "Rural (%)") %>% pull(Impact), 
                    pointPlacement = "on", color = "#1E90FF",  # Blue color
                    dataLabels = list(enabled = TRUE, format = '{point.y}%')) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE))) %>%
      hc_legend(itemStyle = list(fontSize = "14px")) %>%
      hc_tooltip(pointFormat = "Barrier: {point.category}<br>Impact: {point.y}%")
  })
  
  
  
  # Logic Underemployment Rates Chart (Clustered Bar)
  output$underemploymentChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Underemployment Rate by Age Group") %>%
      hc_xAxis(categories = Underemployment_rates$`Age Group`, title = list(text = "Age Group")) %>%
      hc_yAxis(title = list(text = "Underemployment Rate (%)"), min = 0) %>%
      hc_add_series(name = "Underemployment Rate (%)", data = Underemployment_rates$`Underemployment Rate(%)`, color = "#458B00") %>%
      hc_plotOptions(column = list(stacking = "normal", dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b><br/>Underemployment Rate: {point.y}%")
  })
  
  
  # Logic for Financial Exclusion Pie Chart
  output$financialExclusionPieChart <- renderHighchart({
    # Convert the 'Financially rates' data into a suitable format for a pie chart
    data <- Financially_rates %>%
      select(`Age Group`, `Financial Exclusion Rate(%)`) %>%
      rename(name = `Age Group`, y = `Financial Exclusion Rate(%)`) %>%
      list_parse()
    
    # Create the pie chart
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Financial Exclusion Rate by Age Group") %>%
      hc_plotOptions(pie = list(
        innerSize = "50%",  # This makes it a donut chart
        dataLabels = list(enabled = TRUE, format = '{point.name}: {point.y:.1f}%')
      )) %>%
      hc_add_series(name = "Financial Exclusion Rate", data = data)
  })  
  
  
  # Logic Bar chart for Regional Employment Status
  output$barChart1 <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Employment Rate by Region") %>%
      hc_xAxis(categories = employment_status_by_region$Region, title = list(text = "Region")) %>%
      hc_yAxis(title = list(text = "Employment Rate (%)")) %>%
      hc_add_series(
        name = "Employment Rate",
        data = employment_status_by_region$`Employment Rate`,
        colorByPoint = TRUE
      ) %>%
      hc_tooltip(pointFormat = "Employment Rate: {point.y}%") %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
  }) 
  
  # Logic Bar chart for Regional Education and Employment Rates
  output$educationEmploymentChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Regional Education and Employment Rates") %>%
      hc_xAxis(categories = Regional_education_and_employment_rates$Region, title = list(text = "Region")) %>%
      hc_yAxis(title = list(text = "Rates (%)")) %>%
      hc_add_series(
        name = "Primary Education Rate",
        data = Regional_education_and_employment_rates$`Primary Education Rate`,
        color = "#1E90FF"
      ) %>%
      hc_add_series(
        name = "Secondary Education Rate",
        data = Regional_education_and_employment_rates$`Secondary Education Rate`,
        color = "#32CD32"
      ) %>%
      hc_add_series(
        name = "Employment Rate",
        data = Regional_education_and_employment_rates$`Employment Rate`,
        color = "#FFA500"
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>%
      hc_legend(enabled = TRUE)
  })
  
  
  # Logic  Waterfall Chart for Cumulative Job Creation towards Target
  output$waterfallChart <- renderHighchart({
    selected_column <- input$column_select  # Selected column for display
    
    # Find the maximum cumulative jobs to set the y-axis limit dynamically
    max_jobs <- max(Targets_job$Cumulative_Jobs, na.rm = TRUE) * 1.2  # Increase by 20% for better visibility
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = paste("Cumulative Job Creation in", selected_column, "Sector (2021-2026)")) %>%
      hc_xAxis(categories = Targets_job$Year, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Jobs Created"), min = 0, max = max_jobs) %>%  # Set y-axis max to increase visibility
      hc_add_series(
        name = "Jobs Created",
        data = Targets_job[[selected_column]],  # Dynamic selection
        dataLabels = list(enabled = TRUE, format = '{point.y}')
      ) %>%
      hc_add_series(
        name = "Cumulative Jobs",
        data = Targets_job$Cumulative_Jobs,
        color = "#007bff"
      ) %>%
      hc_tooltip(pointFormat = "Total: {point.y}")
  })
  
  
  
  # Logic Display Job Creation Table
  output$jobCreationTable <- DT::renderDataTable({
    DT::datatable(Targets_job)
  })
  # Role of Vocational Training Bar Chart
  output$vocationalTrainingChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Participation in Vocational Training by Course Type") %>%
      hc_xAxis(categories = Vocational_training_role$Year, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Number of Participants")) %>%
      hc_add_series(name = "Technical Courses", data = Vocational_training_role$`Technical Courses`) %>%
      hc_add_series(name = "Vocational Courses", data = Vocational_training_role$`Vocational Courses`) %>%
      hc_add_series(name = "Apprenticeships", data = Vocational_training_role$Apprenticeships) %>%
      hc_tooltip(shared = TRUE, pointFormat = "{series.name}: {point.y}") %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
  })
  
  # Logic Enrollment over time
  output$enrollmentLineChart <- renderHighchart({
    # Extract years and values from the Enrollment_over_time dataset
    years <- Enrollment_over_time$Year 
    Target_Enrollment_Increases <- Enrollment_over_time$`Target Enrollment Increase`
    
    # Create the line chart
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Enrollment Over Time") %>%
      hc_xAxis(categories = years, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Enrollment")) %>%
      hc_add_series(name = "Enrollment", data = Target_Enrollment_Increases, color = "#2E86C1") %>%
      hc_tooltip(pointFormat = "Enrollment: {point.y}") %>%
      hc_plotOptions(line = list(marker = list(enabled = TRUE)))
  }) 
  
  
  # Logic create a bullet chart
  output$bulletChart <- renderPlot({
    ggplot(Programs_progress, aes(x = Program, y = Beneficiaries / 1000)) + # Dividing by 1000 for readability
      geom_col(aes(fill = as.factor(Impact)), width = 0.6) +
      geom_segment(aes(x = Program, xend = Program, y = 0, yend = 100),
                   color = "gray70", linetype = "dashed") +
      geom_point(aes(y = Beneficiaries / 1000), color = "black", size = 3) +
      scale_y_continuous(name = "Beneficiaries (in Thousands)", limits = c(0, 100)) +
      scale_fill_manual(values = c("2" = "skyblue", "3" = "coral"), 
                        name = "Impact",
                        labels = c("Moderate (2)", "High (3)")) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Programs Progress - Beneficiaries and Impact",
        x = "Programs",
        y = "Beneficiaries (in Thousands)"
      )
  })
  
  
  # Logic Render bar chart using highchart based on selected program
  output$empowermentPieChart <- renderHighchart({
    
    # Select the funding column based on the chosen year
    selected_year <- input$year_select
    
    # Prepare the data for the selected year
    chart_data <- Empowerment_Programs1 %>%
      select(Program, !!sym(selected_year)) %>%
      rename(Funding = !!sym(selected_year))  # Rename for easier referencing
    
    # Add total funding amount to program names
    chart_data <- chart_data %>%
      mutate(Program_with_Funding = paste(Program, "($", Funding, ")", sep = "")) %>%
      select(Program_with_Funding, Funding)  # Select the modified name column
    
    # Convert the data frame to a list format for the highchart pie chart
    data_list <- chart_data %>%
      filter(!is.na(Funding)) %>%                # Filter out any missing values
      mutate(Funding = as.numeric(Funding)) %>%  # Ensure numeric
      rename(name = Program_with_Funding, y = Funding) %>%
      list_parse2()                              # Parse to list format for highchart
    
    # Create the pie chart using highcharter
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = paste("Funding Distribution -", selected_year)) %>%
      hc_add_series(name = "Funding", data = data_list) %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.y}')
  })
  
  
  #  Logic Outcomes and Strategies chart
  output$outcomeStrategyChart <- renderHighchart({
    # Filter data for the selected priority area
    selected_data <- Outcomes_and_Strategies %>%
      filter(`Priority area` == input$priority_area)
    
    # Ensure there is data to display
    if (nrow(selected_data) == 0) return(NULL)
    
    # Extract funding data for each year
    years <- c("2020/21", "2021/22", "2022/23", "2023/24", "2024/25")
    funding_values <- as.numeric(selected_data[1, 2:6])
    
    # Create a line chart for funding over years
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = paste("Funding for", input$priority_area)) %>%
      hc_xAxis(categories = years, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Funding (RWF)")) %>%
      hc_add_series(name = input$priority_area, data = funding_values) %>%
      hc_tooltip(pointFormat = "Funding: {point.y} RWF")
  }) 
  
  
  # Render Stacked Bar Chart for Youth Employment and Poverty Status
  output$employmentPovertyChart <- renderHighchart({
    filtered_data <- employment_poverty_status %>%
      filter(if (input$region != "All") Region == input$region else TRUE)
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Youth Employment, Unemployment, and Poverty Rates by Region") %>%
      hc_xAxis(categories = filtered_data$Region) %>%
      hc_yAxis(title = list(text = "Rate (%)")) %>%
      hc_add_series(name = "Employment Rate", data = filtered_data$`Youth Employment Rate (%)`, stack = "rate") %>%
      hc_add_series(name = "Unemployment Rate", data = filtered_data$`Youth Unemployment Rate (%)`, stack = "rate") %>%
      hc_add_series(name = "Poverty Rate", data = filtered_data$`Poverty Rate (%)`, stack = "rate") %>%
      hc_add_series(name = "Extreme Poverty Rate", data = filtered_data$`Extreme Poverty Rate (%)`, stack = "rate") %>%
      hc_plotOptions(series = list(stacking = "normal"))
  })
  
  
  # Render Grouped Bar Chart for Youth Education and Skills Development
  output$educationSkillsChart <- renderHighchart({
    filtered_data <- Education_and_Skills %>%
      filter(if (input$education_level != "All") `Education Level` == input$education_level else TRUE)
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Youth Education Levels and Gender Distribution") %>%
      hc_xAxis(categories = filtered_data$`Education Level`) %>%
      hc_yAxis(title = list(text = "Percentage (%)")) %>%
      hc_add_series(name = "Total Youth Attending", data = filtered_data$`% of Youth Attending`, color = "#0073C2") %>%
      hc_add_series(name = "Male (%)", data = if (input$gender == "All" || input$gender == "Male (%)") filtered_data$`Male (%)` else NULL, color = "#66CD00") %>%
      hc_add_series(name = "Female (%)", data = if (input$gender == "All" || input$gender == "Female (%)") filtered_data$`Female (%)` else NULL, color = "#ff7f0e") %>%
      hc_plotOptions(column = list(grouping = TRUE))
  })  
  
  
  # Render Grouped Bar Chart for Youth Health and Wellbeing
  output$healthChart <- renderHighchart({
    filtered_health <- Youth_Health %>%
      filter(if (input$health_metric != "All") `Health Metric` == input$health_metric else TRUE)
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Youth Health Indicators") %>%
      hc_xAxis(categories = filtered_health$`Health Metric`) %>%
      hc_yAxis(title = list(text = "Youth Population (%)")) %>%
      hc_add_series(name = "Youth Population (%)", data = filtered_health$`Youth Population (%)`, color = "#1f78b4") %>%
      hc_add_series(name = "Male (%)", data = filtered_health$`Male (%)`, color = "#4daf4a") %>%
      hc_add_series(name = "Female (%)", data = filtered_health$`Female (%)`, color = "#e41a1c") %>%
      hc_plotOptions(column = list(grouping = TRUE)) %>%
      hc_tooltip(shared = TRUE, pointFormat = '<b>{series.name}</b>: {point.y}%')
  })
  
  
  
  # Render Donut Chart for Financial Inclusion and Entrepreneurship
  output$financeChart <- renderHighchart({
    # Filter data based on selected financial metric
    filtered_finance <- Finance_and_Business %>%
      filter(`Finance and Business Metric` == input$finance_metric)
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = paste("Financial Access and Targeted Increase by 2025 for:", input$finance_metric)) %>%
      hc_plotOptions(pie = list(
        innerSize = "60%",  # Creates the donut chart effect
        dataLabels = list(enabled = TRUE, format = '{point.name}: {point.y}%')
      )) %>%
      hc_add_series(
        name = "Percentage",
        data = list(
          list(name = "Current Percentage", y = filtered_finance$`Percentage (%)`, color = "#007bff"),
          list(name = "Target Increase by 2025", y = filtered_finance$`Targeted Increase by 2025 (%)`, color = "#FFD700")
        ),
        colorByPoint = TRUE
      ) %>%
      hc_tooltip(pointFormat = "{point.name}: {point.y}%")
  }) 
  
  
  # Render the grouped bar chart for ICT and Digital Literacy
  output$digitalLiteracyChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "ICT and Digital Literacy by Region") %>%
      hc_xAxis(categories = ICT_and_digital_literacy_data$Region, title = list(text = "Region")) %>%
      hc_yAxis(title = list(text = "Percentage (%)")) %>%
      hc_add_series(name = "Computer Literacy (%)", 
                    data = ICT_and_digital_literacy_data$`Computer Literacy (%)`, 
                    color = "#3498db") %>%
      hc_add_series(name = "Mobile Ownership (%)", 
                    data = ICT_and_digital_literacy_data$`Mobile Ownership (%)`, 
                    color = "#2ecc71") %>%
      hc_add_series(name = "Internet Usage (%)", 
                    data = ICT_and_digital_literacy_data$`Internet Usage (%)`, 
                    color = "#e74c3c") %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>%
      hc_legend(enabled = TRUE)
  }) 
  # Train a predictive model (linear regression example)
  model <- lm(Unemployment_Rate ~ Employment_Rate + Underemployment_Rate, 
              data = Employment_and_Regional_Distribution)
  
  
  # Predict function
  observeEvent(input$predict, {
    # Filter data for the selected region
    selected_region <- input$region
    region_data <- Employment_and_Regional_Distribution %>%
      filter(Region == selected_region)
    
    # Perform prediction
    prediction <- predict(model, newdata = region_data)
    
    # Display the predicted unemployment rate
    output$predictionResult <- renderPrint({
      paste("Predicted Unemployment Rate for", selected_region, ":", 
            round(prediction, 2), "%")
    })
    
    # Render the Highcharter plot
    output$plot <- renderHighchart({
      highchart() %>%
        hc_chart(type = "scatter") %>%
        hc_title(
          text = paste("Unemployment Rate vs Employment Rate in", selected_region)
        ) %>%
        hc_xAxis(
          title = list(text = "Employment Rate (%)"),
          plotLines = list(list(
            value = region_data$Employment_Rate,
            color = "red",
            width = 2,
            label = list(text = "Selected Region", style = list(color = "red"))
          ))
        ) %>%
        hc_yAxis(title = list(text = "Unemployment Rate (%)")) %>%
        hc_add_series(
          name = "Regions",
          data = list_parse2(
            Employment_and_Regional_Distribution %>% 
              select(Employment_Rate, Unemployment_Rate)
          ),
          type = "scatter",
          marker = list(radius = 5),
          color = "#7cb5ec"
        ) %>%
        hc_add_series(
          name = "Linear Fit",
          data = list_parse2(
            data.frame(
              x = Employment_and_Regional_Distribution$Employment_Rate,
              y = predict(model, newdata = Employment_and_Regional_Distribution)
            )
          ),
          type = "line",
          color = "#434348",
          lineWidth = 2
        ) %>%
        hc_add_series(
          name = "Prediction",
          data = list(list(
            x = region_data$Employment_Rate,
            y = prediction
          )),
          type = "scatter",
          color = "red",
          marker = list(radius = 8, symbol = "circle")
        ) %>%
        hc_tooltip(
          pointFormat = "<b>{series.name}</b><br>Employment Rate: {point.x}%<br>Unemployment Rate: {point.y}%"
        )
    })
  })
  
}

shinyApp(ui = ui, server = server)
