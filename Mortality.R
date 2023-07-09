# library the packages needed 
library(shiny)
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(expss)

# read the data 
mortality <- read.csv(here("data","finaldata.csv"))

# select the variables needed 

mortality_app <- mortality %>%
  select(country_name, region, year, MatMor, InfMor, NeoMor, Un5Mor, armconf1) %>%
  mutate(armconf1 = case_when(armconf1 == 0 ~ "No",
                              armconf1 == 1 ~ "Yes")) 

mortality_app <- apply_labels(mortality_app,
                              country_name = "Country", 
                              region = "Region", 
                              year = "Year", 
                              MatMor = "Maternal mortality", 
                              InfMor = "Infant mortality", 
                              NeoMor = "Neonatal mortality", 
                              Un5Mor = "Under-5 mortality", 
                              armconf1 = "Arm conflict")

# Define UI 

ui <- fluidPage(
  titlePanel("Mortality trend for 2000-2019"),
  
  fluidRow(
    column(
      width = 4,
      wellPanel(
        selectizeInput(
          inputId = "region",
          label = "Region selected:",
          choices = unique(mortality_app$region),
          selected = unique(mortality_app$region),
          multiple = TRUE
        ),
        radioButtons(
          inputId = "mor",
          label = "Mortality selected:",
          choices = list("Maternal mortality" = "MatMor", 
                         "Infant mortality" = "InfMor", 
                         "Neonatal mortality" = "NeoMor", 
                         "Under-5 mortality" = "Un5Mor"),
          selected = "MatMor"
        ),
        checkboxGroupInput(
          inputId = "arm",
          label = "Arm conflict selected:",
          choices = unique(mortality_app$armconf1),
          selected = unique(mortality_app$armconf1)
        )
      )
    ),
    column(
      width = 8,
      plotlyOutput("plot")
    )
  ),
  fluidRow(
    column(width = 6,
           wellPanel(conditionalPanel(condition = "input.region.indexOf('Southern Asia') != -1", # check if Southern Asia is selected
                            selectizeInput(
                              inputId = "Sa",
                              label = "Country from Southern Asia selected:",
                              choices = unique(mortality_app[mortality_app$region == 'Southern Asia',]$country_name),
                              selected = unique(mortality_app[mortality_app$region == 'Southern Asia',]$country_name),
                              multiple = TRUE
                              )),
                     conditionalPanel(condition = "input.region.indexOf('Melanesia') != -1", # check if Melanesia is selected
                                      selectizeInput(
                                        inputId = "Me",
                                        label = "Country from Melanesia selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Melanesia',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Melanesia',]$country_name),
                                        multiple = TRUE
                                      )),
                     conditionalPanel(condition = "input.region.indexOf('Polynesia') != -1", # check if Polynesia is selected
                                      selectizeInput(
                                        inputId = "Po",
                                        label = "Country from Polynesia selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Polynesia',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Polynesia',]$country_name),
                                        multiple = TRUE
                                      )),
                     conditionalPanel(condition = "input.region.indexOf('Western Europe') != -1", # check if Western Europe is selected
                                      selectizeInput(
                                        inputId = "We",
                                        label = "Country from Western Europe selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Western Europe',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Western Europe',]$country_name),
                                        multiple = TRUE
                                      )),
                     conditionalPanel(condition = "input.region.indexOf('Northern Europe') != -1", # check if Northern Europe is selected
                                      selectizeInput(
                                        inputId = "Ne",
                                        label = "Country from Northern Europe selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Northern Europe',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Northern Europe',]$country_name),
                                        multiple = TRUE
                                      )),
                     conditionalPanel(condition = "input.region.indexOf('Eastern Europe') != -1", # check if Eastern Europe is selected
                                      selectizeInput(
                                        inputId = "Ee",
                                        label = "Country from Eastern Europe selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Eastern Europe',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Eastern Europe',]$country_name),
                                        multiple = TRUE
                                      )),
                     conditionalPanel(condition = "input.region.indexOf('Northern America') != -1", # check if Northern America is selected
                                      selectizeInput(
                                        inputId = "Nam",
                                        label = "Country from Northern America selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Northern America',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Northern America',]$country_name),
                                        multiple = TRUE
                                      )),
                     conditionalPanel(condition = "input.region.indexOf('Latin America and the Caribbean') != -1", # check if Latin America and the Caribbean is selected
                                      selectizeInput(
                                        inputId = "Lac",
                                        label = "Country from Latin America and the Caribbean selected:",
                                        choices = unique(mortality_app[mortality_app$region == 'Latin America and the Caribbean',]$country_name),
                                        selected = unique(mortality_app[mortality_app$region == 'Latin America and the Caribbean',]$country_name),
                                        multiple = TRUE
                                      )),
                      conditionalPanel(condition = "input.region.indexOf('Southern Europe') != -1", # check if Southern Europe is selected
                            selectizeInput(
                              inputId = "Se",
                              label = "Country from Southern Europe selected:",
                              choices = unique(mortality_app[mortality_app$region == 'Southern Europe',]$country_name),
                              selected = unique(mortality_app[mortality_app$region == 'Southern Europe',]$country_name),
                              multiple = TRUE
                                             )))
           ),
  column(width = 6,
         wellPanel(conditionalPanel(condition = "input.region.indexOf('Sub-Saharan Africa') != -1", # check if Sub-Saharan Africa is selected
                          selectizeInput(
                            inputId = "Ssa",
                            label = "Country from Sub-Saharan Africa selected:",
                            choices = unique(mortality_app[mortality_app$region == 'Sub-Saharan Africa',]$country_name),
                            selected = unique(mortality_app[mortality_app$region == 'Sub-Saharan Africa',]$country_name),
                            multiple = TRUE
                                   )),
                   conditionalPanel(condition = "input.region.indexOf('Micronesia') != -1", # check if Micronesia is selected
                                    selectizeInput(
                                      inputId = "Mi",
                                      label = "Country from Micronesia selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'Micronesia',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'Micronesia',]$country_name),
                                      multiple = TRUE
                                    )),
                   conditionalPanel(condition = "input.region.indexOf('Central Asia') != -1", # check if Central Asia is selected
                                    selectizeInput(
                                      inputId = "Ca",
                                      label = "Country from Central Asia selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'Central Asia',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'Central Asia',]$country_name),
                                      multiple = TRUE
                                    )),
                   conditionalPanel(condition = "input.region.indexOf('Western Asia') != -1", # check if Western Asia is selected
                                    selectizeInput(
                                      inputId = "Wa",
                                      label = "Country from Western Asia selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'Western Asia',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'Western Asia',]$country_name),
                                      multiple = TRUE
                                    )),
                   conditionalPanel(condition = "input.region.indexOf('Eastern Asia') != -1", # check if Eastern Asia is selected
                                    selectizeInput(
                                      inputId = "Ea",
                                      label = "Country from Eastern Asia selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'Eastern Asia',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'Eastern Asia',]$country_name),
                                      multiple = TRUE
                                    )),
                   conditionalPanel(condition = "input.region.indexOf('South-eastern Asia') != -1", # check if South-eastern Asia is selected
                                    selectizeInput(
                                      inputId = "Sea",
                                      label = "Country from South-eastern Asia selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'South-eastern Asia',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'South-eastern Asia',]$country_name),
                                      multiple = TRUE
                                    )),
                   conditionalPanel(condition = "input.region.indexOf('Australia and New Zealand') != -1", # check if Australia and New Zealand is selected
                                    selectizeInput(
                                      inputId = "Anz",
                                      label = "Country from Australia and New Zealand selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'Australia and New Zealand',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'Australia and New Zealand',]$country_name),
                                      multiple = TRUE
                                    )),
                   conditionalPanel(condition = "input.region.indexOf('Northern Africa') != -1", # check if Northern Africa is selected
                                    selectizeInput(
                                      inputId = "Na",
                                      label = "Country from Northern Africa selected:",
                                      choices = unique(mortality_app[mortality_app$region == 'Northern Africa',]$country_name),
                                      selected = unique(mortality_app[mortality_app$region == 'Northern Africa',]$country_name),
                                      multiple = TRUE
                                    ))
                  )))
)

# Define server logic

server <- function(input, output) {
  selected <- reactive({
    mortality_app %>%
      filter(region %in% input$region) %>%
      filter(armconf1 %in% input$arm) %>%
      filter(country_name %in% input$Se | country_name %in% input$Sa | country_name %in% input$Na | 
               country_name %in% input$We | country_name %in% input$Lac | country_name %in% input$Ssa | 
               country_name %in% input$Wa | country_name %in% input$Anz | country_name %in% input$Ee | 
               country_name %in% input$Sea | country_name %in% input$Nam | country_name %in% input$Ea |
               country_name %in% input$Ne | country_name %in% input$Mi | country_name %in% input$Me | 
               country_name %in% input$Ca | country_name %in% input$Po)
    })
  
  output$plot <- renderPlotly({
      plot_ly(data = selected(), x = ~year, y = ~.data[[input$mor]], color = ~country_name) %>%
      add_lines(linetype = ~armconf1) %>%
      layout(title = "Longitudinal plot", xaxis = list(title = 'Year'),
             yaxis = list(title = 'Mortality',type = 'log'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)