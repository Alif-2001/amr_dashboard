library(shiny)
library(shinydashboard)
library(DT)
library(arrow)
library(plotly)
library(dplyr)

library(e1071)
library(caret)

data <- read_parquet("AMRData.parquet")
literature_data <- read.csv("literature.csv")

generateLiterature <- function() {
  boxes <- list()
  
  for (i in 1:nrow(literature_data)) {
    boxes[[i]] <- box(
      title = literature_data[i,]$Title,
      width = 6,
      h5(literature_data[i,]$Year),
      h5(literature_data[i,]$Author),
      a(literature_data[i,]$Link)
    )
  }
  return(boxes)
}



############################
# Load Naive Bayes for bacteria #
############################
naive_data <- read.csv("models/naive/naive_data.csv")
naive <- readRDS("models/naive/naive.rds")


ui <- dashboardPage(
  dashboardHeader(title = "WY - dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Summary", tabName = "summary"),
      #menuItem("Use our Model", tabName = "model"),
      menuItem("Model #1", tabName = "method"),
      menuItem("Literature", tabName = "literature"),
      menuItem("Data", tabName = "data")
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "home",
            fluidRow(
              column(
                width = 12,
                align = "center",
                h1("Welcome to the Weyland-Yutani Corporation Dashboard"),
                h3("\"Building Better Worlds\"", style = "margin-bottom: 32px;"),
                box(
                  title = HTML(
                    "<strong style='font-size: 32px;'>The Research Problem</strong>"
                  ),
                  width = 12,
                  solidHeader = TRUE,
                  p(
                    "The challenge of antimicrobial resistance is vast and all-encompassing,
                 presenting a complexity that surpasses the capacity of any individual to
                 tackle alone. This holds true not only for regular individuals like
                 ourselves but also for dedicated professionals, including veterinarians.
                 The intricacies involve diverse drug tiers, various classes of bacteria,
                 the phenomenon of horizontal gene transfer, and potentially more complexities.
                 It is unsurprising that many veterinarians may not readily engage in antimicrobial
                 stewardship, as addressing it comprehensively would constitute an additional
                 full-time commitment atop their already demanding responsibilities in pet care.
                 This is precisely where our role becomes crucial — alleviating a significant
                 portion of the mental burden carried by veterinarians. We aim to provide them
                 with the necessary tools and information, streamlining the decision-making process,
                 and concurrently serving as a valuable resource for individuals like us embarking
                 on the journey of understanding and learning about this multifaceted problem
                 at the outset of the semester.",
                    style = "font-size: 16px;"
                  )
                ),
                box(
                  title = HTML(
                    "<strong style='font-size: 32px;'>Our Research Questions</strong>"
                  ),
                  width = 12,
                  solidHeader = TRUE,
                  p(
                    "What is the most effective antimicrobial treatment for any given type of infection in cats?",
                    style = "font-size: 24px;"
                  ),
                  p(
                    "We endeavored to address this question using a naive Bayes classifier, specifically a multinomial variant, chosen for its suitability in handling discrete data.
                  This classifier excels in categorizing unlabeled data points by leveraging existing data and conditional probabilities.
                    Although it simplistically presumes independence between attributes, the effectiveness of this algorithm is well-documented.",
                    style = "font-size: 16px;"
                  ),
                  p(
                    "Which pathogens are most commonly found in canines during different times of the year?",
                    style = "font-size: 24px;"
                  ),
                  p(
                    "Again, the naive Bayes classifier appears most fitting for our inquiry, especially considering the independence of the columns utilized in the classification.
                  We aim to predict a pathogen class that is discrete or categorical, encompassing 282 different types of pathogens in our dataset.
                  The naive Bayes classifier is particularly adept for this scenario, as it proficiently calculates the likelihood or probability of a bacterium's presence.
                  Our assumption is that the occurrence of a pathogen is unrelated to both the month and the species of the pet in question.
                    Alternatively, logistic regression could be employed to ascertain the presence of specific bacteria across different months.
                    However, this approach necessitates conducting individual tests for each type of bacteria, adding complexity to the analysis.",
                    style = "font-size: 16px;"
                  )
                )
              )
            ),),
    tabItem(tabName = "summary",
            fluidRow(
              box(
                title = "Summary of the Data",
                width = 6,
                selectInput("county_filter",
                            "Select County",
                            choices = c("All", unique(data$county)))
              ),
              box(
                title = "Total Tests",
                width = 3,
                status = "success",
                h3(textOutput("total_tests"))
              ),
              box(
                title = "Tests by Type",
                width = 3,
                solidHeader = TRUE,
                status = "success",
                background = "black",
                div(h4("Canine"),
                    h3(textOutput(
                      "total_tests_canine"
                    ))),
                div(h4("Feline"),
                    h3(textOutput(
                      "total_tests_feline"
                    )))
              ),
              box(
                title = "Tests Over Time",
                width = 12,
                plotlyOutput("total_tests_plot")
              ),
              box(
                title = "Bacteria Summary",
                width = 12,
                plotlyOutput("total_bacteria_plot")
              )
            )),
    # tabItem(
    #   tabName = "model",
    #   fluidRow(
    #     box(
    #       title = h3("Use Our Model"),
    #       width = 12
    #     ),
    #     box(
    #       title = h4("Your inputs here"),
    #       width = 5,
    #       selectInput(
    #         "general_model_county_input",
    #         "Select County",
    #         choices = unique(data$county)),
    #       selectInput(
    #         "general_model_species_input",
    #         "Select Species",
    #         choices = unique(data$species)),
    #       selectInput(
    #         "general_model_month_input",
    #         "Select Month",
    #         choices = unique(data$order_month)),
    #       selectInput(
    #         "general_model_source_input",
    #         "Select Source",
    #         choices = unique(data$source))
    #     ),
    #     box(
    #       width = 2,
    #       actionButton("general_model_run", "Get Predictions")
    #     ),
    #     box(
    #       title = h4("Drugs Likely To Be Effective"),
    #       width = 5,
    #       plotlyOutput("general_model_output")
    #     )
    #   )
    # ),
    tabItem(tabName = "method",
            fluidRow(
              h1("Model #1", style = "text-align: center;"),
              h3(
                "Which pathogens are most commonly found in canines during different times of the year?",
                style = "text-align: center;"
              ),
              br(),
            ),
            fluidRow(
              box(
                title = h4("Model Inputs", style = "text-align: center"),
                width = 6,
                selectInput(
                  "naive_model_county_input",
                  "Select County",
                  choices = unique(data$county)
                ),
                selectInput(
                  "naive_model_species_input",
                  "Select Species",
                  choices = unique(data$species)
                ),
                selectInput(
                  "naive_model_month_input",
                  "Select Month",
                  choices = unique(data$order_month)
                ),
                selectInput(
                  "naive_model_source_input",
                  "Select Source",
                  choices = unique(data$source)
                )
              ),
              box(
                title = h4("Bacteria Predictions", style = "text-align: center"),
                width = 6,
                plotlyOutput("naive_model_output")
              )
            ),
            fluidRow(
              box(
                title = h3("Model Overall Stats", style = "text-align: center;"),
                width = 12,
                tags$style(HTML(".center-code { text-align: center; }")),
                div(
                  class = "center-code",
                  code(
                    "Accuracy : 0.3659",
                    br(),
                    "95% CI : (0.3607, 0.3711)",
                    br(),
                    "No Information Rate : 0.2619",
                    br(),
                    "P-Value [Acc > NIR] : < 2.2e-16",
                    br(),
                    "Kappa : 0.1972",
                    br(),
                    "Mcnemar's Test P-Value : NA"
                  )
                )
              )
            )
    ),
    tabItem(tabName = "literature",
            fluidRow(box(
              title = h2("Literature Used"),
              width = 12,
              do.call(fluidRow, generateLiterature())
            ))),
    tabItem(tabName = "data",
            fluidRow(
              box(
                title = h2("Download Data"),
                width = 12,
                downloadButton("downloadData", h2("Original")),
                downloadButton("downloadNaiveData", h2("Naive Data")),
              ),
              box(
                title = h2("Data Table"),
                width = 12,
                DTOutput("dataTable")
              )
            ))
  ))
)

countyFilteredData <- function(select) {
  return(if (select == "All")
    data
    else
      data[data$county == select,])
}

server <- function(input, output) {
  output$total_tests <- renderText({
    as.character(nrow(countyFilteredData(input$county_filter)))
  })
  output$total_tests_canine <- renderText({
    temp = countyFilteredData(input$county_filter)
    as.character(nrow(temp[temp$species == "CANINE",]))
  })
  output$total_tests_feline <- renderText({
    temp = countyFilteredData(input$county_filter)
    as.character(nrow(temp[temp$species == "FELINE",]))
  })
  output$total_tests_plot <- renderPlotly({
    temp = countyFilteredData(input$county_filter)
    temp$month_year = as.character(paste(temp$order_year, temp$order_month))
    plot_ly(temp, x = ~ month_year) %>%
      layout(
        title = paste(
          "Tests In",
          input$county_filter,
          if (input$county_filter == "All")
            "Counties"
          else
            "County"
        ),
        xaxis = list(title = 'Year-Month'),
        yaxis = list(title = 'Number of Tests')
      )
  })
  output$total_bacteria_plot <- renderPlotly({
    temp = countyFilteredData(input$county_filter)
    top_values <- temp %>%
      group_by(org_standard) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice(1:10)
    plot_ly(
      top_values,
      x = ~ count,
      y = ~ org_standard,
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        title = paste(
          "Top 10 Bacteria and Their Counts In",
          input$county_filter,
          if (input$county_filter == "All")
            "Counties"
          else
            "County"
        ),
        xaxis = list(title = 'Count'),
        yaxis = list(title = 'Bacteria')
      )
  })
  
  ## Naive model output
  output$naive_model_output <- renderPlotly({
    naive_input <- data.frame(
      county = input$naive_model_county_input,
      order_month = input$naive_model_month_input,
      species = input$naive_model_species_input,
      source = input$naive_model_source_input
    )
    prediction <- predict(naive, naive_input, "raw")
    prediction <- as.data.frame(prediction)
    selected_columns <- prediction %>%
      select_if( ~ all(. >= 0.01))
    selected_columns <- as.matrix(selected_columns)
    other <- c(1 - rowSums(selected_columns))
    selected_columns <- cbind(selected_columns, other)
    
    plot_ly(
      labels = colnames(selected_columns),
      values = selected_columns[1,],
      type = "pie"
    )
  })
  
  
  ## Download page
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("AMRData", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  output$downloadNaiveData <- downloadHandler(
    filename = function() {
      paste("AMRNaiveData", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(naive_data, file)
    }
  )
  output$dataTable <- renderDT({
    datatable(data,
              options = list(scrollX = TRUE,
                             scrollY = TRUE))
  })
}

shinyApp(ui = ui, server = server)