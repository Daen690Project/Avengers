
## packages required 
library(shiny)
library(readxl)
library(janitor)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(plotly)

## loading data from excel
# df = read_excel("Dataset.xlsx", sheet = 2)

# saving data as csv for easier processing
# write.csv(df,"RunwayIncursions.csv", row.names = F)

## loading csv data
df = read.csv("RunwayIncursions.csv")

## cleaning names of columns
df = clean_names(df)

## converting to date
df$Date = as.Date(df$date_of_event)

## adding new variables Year and Month
df$Year = year(df$Date)
df$Month = month(df$Date)

## getting list of 
eventTypes = unique(df$event_type)

incidentTypes = unique(df$incident_type_category)

severityCategories = unique(df$severity_category)%>% as.character()
severityCategories = severityCategories[severityCategories != "N/A"]

airportCodes = unique(df$airport_code)

regionIDs = unique(df$region_id) %>% na.omit() %>% as.character()

## states data is empty 
states = unique(df$state)

service_area_desc = unique(df$service_area_desc)

traffic_mix = unique(df$traffic_mix)


DayMax = df%>%
  group_by(region_id,Date)%>%
  summarise(Count = n())%>%
  as.data.frame() %>%
  filter(Count == max(Count))%>%
  head(1)%>%
  select(Count) %>% as.numeric()


MonthMax = df%>%
  group_by(region_id,Year,Month)%>%
  summarise(Count = n()) %>%
  mutate(Date = as.Date( paste0(Year,"-",Month,"-01"))) %>%
  as.data.frame() %>%
  filter(Count == max(Count))%>%
  head(1)%>%
  select(Count) %>% as.numeric()


YearMax = df%>%
  group_by(region_id,Year)%>%
  summarise(Count = n())%>%
  as.data.frame() %>%
  filter(Count == max(Count))%>%
  head(1)%>%
  select(Count) %>% as.numeric()




# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Data Analysis: Runway Incursions"),
    # style = "background: #EFF8FF;",
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          h2("Data Filters"),
          pickerInput(
            inputId = "regionIDs",
            label = "Region IDs",
            choices = regionIDs,
            selected = regionIDs[1],
            options = list(`actions-box` = T),
            multiple = T
          ), 
          pickerInput(
            inputId = "severityCategories",
            label = "Severity Categories",
            choices = severityCategories,
            selected = severityCategories,
            options = list(`actions-box` = T),
            multiple = T
          ), 
          pickerInput(
            inputId = "incidentTypes",
            label = "Incident Types",
            choices = incidentTypes,
            selected = incidentTypes,
            options = list(`actions-box` = T),
            multiple = T
          ),
          pickerInput(
            inputId = "airportCodes",
            label = "Airport Codes",
            choices = airportCodes,
            selected = airportCodes,
            options = list(`actions-box` = T),
            multiple = T
          ),
          hr(),
          h2("Plot Filters"),
          pickerInput(
            inputId = "plotType",
            label = "Plot Type",
            choices = c("Bar" = "bar","Line" = "scatter"),
            selected = "Bar"
          ),
          pickerInput(
            inputId = "dateGroupby",
            label = "Aggregate by",
            choices = c("Day","Month","Year"),
            selected = "Day"
          )
        ),

        #
        mainPanel(
           uiOutput("barPlots"),
        )
    )
)

# Define server 
server <- function(input, output) {

  
  
   output$barPlots = renderUI({
     Temp = df %>%
       filter(airport_code %in% input$airportCodes)%>%
       filter(severity_category %in% input$severityCategories)%>%
       filter(incident_type_category %in% input$incidentTypes)
       
     names(df)
     
     
     plots_list = lapply(input$regionIDs, function(i) {
       
       Temp = Temp %>%
         filter(region_id %in% i)
       
       MaxY = DayMax
       if(input$dateGroupby == "Day"){
         Temp = Temp %>%
             group_by(Date)%>%
             summarise(Count = n())%>%
           as.data.frame()
         
       } 
       if(input$dateGroupby == "Month"){
         Temp = Temp %>%
           group_by(Year,Month)%>%
           summarise(Count = n()) %>%
           mutate(Date = as.Date( paste0(Year,"-",Month,"-01"))) %>%
           as.data.frame()
       
         MaxY = MonthMax  
       }
      if(input$dateGroupby == "Year"){
         Temp = Temp %>%
           group_by(Year)%>%
           summarise(Count = n()) %>%
           mutate(Date = paste(Year))%>%
           as.data.frame()
         
         MaxY = YearMax
       }
       
       
       fig  = plot_ly(Temp,
               x =~ Date,
               y =~ Count,
               type = input$plotType,
               mode = "lines+markers"
       ) %>%
         layout(title = paste("Region:", i) ,
                xaxis = list(title = ""),
                yaxis = list(title = "",range = c(0,MaxY)))
       
       wellPanel(style = "background:white;", fig)
     })
      
     plots_list
    
   })

}

# Run the application 
shinyApp(ui = ui, server = server)
