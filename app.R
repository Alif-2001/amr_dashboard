library(shiny)
library(shinydashboard)
library(DT)
library(arrow)
library(plotly)
library(dplyr)

data <- read_parquet("AMRData.parquet")
head(data)

literature_data <- read.csv("literature.csv")

generateLiterature <- function() {
  boxes <- list()

  for ( i in 1:nrow(literature_data)) {
    boxes[[i]] <- box(
      title = literature_data[i, ]$Title,
      width = 6,
      h5(literature_data[i, ]$Year),
      h5(literature_data[i, ]$Author),
      a(literature_data[i, ]$Link)
    )
  }
  return(boxes)
}



############################
# Naive Bayes for bacteria #
############################

library(e1071)
library(arrow)
library(caret)
library(dplyr)

# cleanup data
naive_data <- read_parquet("AMRData.parquet")[c(3, 6, 7, 12, 13)]
naive_data <- na.omit(naive_data)

naive_data$county <- as.factor(naive_data$county)
naive_data$order_month <- as.factor(naive_data$order_month)
naive_data$org_standard <- as.factor(naive_data$org_standard)
naive_data$species <- as.factor(naive_data$species)
naive_data$source <- as.factor(naive_data$source)

set.seed(1)
index <- createDataPartition(naive_data$org_standard, p = 0.7, list=FALSE)
naive_train <- naive_data[index, ]
naive <- naiveBayes(org_standard~., data=naive_train)



ui <- dashboardPage(
  dashboardHeader(title = "WY - dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Use our Model", tabName = "model"),
      menuItem("Methods", tabName = "method"),
      menuItem("Literature", tabName = "literature"),
      menuItem("Data", tabName = "data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = h2("Description of the problem"),
            width = 6,
            h4("The challenge of antimicrobial resistance is vast and all-encompassing, 
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
            at the outset of the semester.")
          ),
          box(
            title = h2("Summary of the data"),
            width = 6,
            fluidRow(
              box(
                width = 12,
                selectInput(
                    "county_filter",
                    "Select County", 
                    choices = append(c("All"), unique(data$county)))
              ),
              box(
                title = h1("Total Tests"), status="success", background = "black",
                width = 6,
                h3(textOutput("total_tests"))
              ),
              box(
                width = 6,
                fluidRow(
                  box(
                    title = "Canine", status="success", background = "black",
                    width = 6,
                    h3(textOutput("total_tests_canine"))
                  ),
                  box(
                    title = "Feline", status="success", background = "black",
                    width = 6,
                    h3(textOutput("total_tests_feline"))
                  )
                )
              )
            )
          ),
          box(
            title = "Tests over time",
            width = 12,
            plotlyOutput("total_tests_plot")
          ),
          box(
            title = "Bacteria summary",
            width = 12,
            plotlyOutput("total_bacteria_plot")
          )
        )
      ),
      tabItem(
        tabName = "model",
        fluidRow(
          box(
            title = h3("Use Our Model"),
            width = 12
          ),
          box(
            title = h4("Your inputs here"),
            width = 5,
            selectInput(
              "general_model_county_input",
              "Select County", 
              choices = unique(naive_data$county)),
            selectInput(
              "general_model_species_input",
              "Select Species", 
              choices = unique(naive_data$species)),
            selectInput(
              "general_model_month_input",
              "Select Month", 
              choices = unique(naive_data$order_month)),
            selectInput(
              "general_model_source_input",
              "Select Source", 
              choices = unique(naive_data$source))
          ),
          box(
            width = 2,
            actionButton("general_model_run", "Get Predictions")
          ),
          box(
            title = h4("Drugs Likely To Be Effective"),
            width = 5,
            plotlyOutput("general_model_output")
          )
        )
      ),
      tabItem(
        tabName = "method",
        fluidRow(
          box(
            title = h3("Using Naive Bayes to Predict Bacteria"),
            width = 12,
            h4("This method seems to be the most appropriate for this question, 
               given that the columns used for the classification are independent 
               from each other. The pathogen class that is being predicted is 
               discrete or categorical, with 282 different types of pathogens 
               in the data. The naive bayes is well-suited for this task since 
               it would give more insight on the probability or the likelihood 
               of the presence of a bacteria. The variables used in this case are 
               the county, the species, the month of the year and the source of 
               the bacteria")
          ),
          box(
            title = h4("Your inputs here"),
            width = 6,
            selectInput(
              "naive_model_county_input",
              "Select County", 
              choices = unique(naive_data$county)),
            selectInput(
              "naive_model_species_input",
              "Select Species", 
              choices = unique(naive_data$species)),
            selectInput(
              "naive_model_month_input",
              "Select Month", 
              choices = unique(naive_data$order_month)),
            selectInput(
              "naive_model_source_input",
              "Select Source", 
              choices = unique(naive_data$source))
          ),
          box(
            title = h4("Bacteria Predictions"),
            width = 6,
            plotlyOutput("naive_model_output")
          )
        )
      ),
      tabItem(
        tabName = "literature",
        fluidRow(
          box(
            title = h2("Literature Used"),
            width = 12,
            do.call(fluidRow,generateLiterature())
          )
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = h2("Download Data"),
            width = 12,
            downloadButton("downloadData", h2("Download"))
          ),
          box(
            title = h2("Data Table"),
            width = 12,
            DTOutput("dataTable")
          )
        )
      )
    )
  )
)

countyFilteredData <- function(select) {
  return(if (select == "All") data else data[data$county == select, ])
}

server <- function(input, output) {
  
  output$total_tests <- renderText({
    as.character(
      nrow(countyFilteredData(input$county_filter))
    )
  })
  output$total_tests_canine <- renderText({
    temp = countyFilteredData(input$county_filter)
    as.character(nrow(temp[temp$species == "CANINE", ]))
  })
  output$total_tests_feline <- renderText({
    temp = countyFilteredData(input$county_filter)
    as.character(nrow(temp[temp$species == "FELINE", ]))
  })
  output$total_tests_plot <- renderPlotly({
    temp = countyFilteredData(input$county_filter)
    temp$month_year = as.character(paste(temp$order_year, temp$order_month))
    plot_ly(temp, x = ~month_year) %>% 
      layout(
        title = paste("Tests In", input$county_filter, 
                      if (input$county_filter == "All") "Counties" else "County"
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
      x = ~count,
      y = ~org_standard,
      type = "bar",
      orientation = "h"
    ) %>% 
      layout(
        title = paste("Top 10 Bacteria and Their Counts In", 
                      input$county_filter,
                      if (input$county_filter == "All") "Counties" else "County"
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
      select_if(~all(.>=0.01))
    selected_columns <- as.matrix(selected_columns)
    other <- c(1 - rowSums(selected_columns))
    selected_columns <- cbind(selected_columns, other)
    
    plot_ly(
      labels = colnames(selected_columns), 
      values = selected_columns[1, ], 
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
  output$dataTable <- renderDT({
    datatable(
      data,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE
      ))
  })
}

shinyApp(ui = ui, server = server)