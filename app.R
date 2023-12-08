library(shiny)
library(shinydashboard)
library(DT)
library(arrow)
library(plotly)
library(dplyr)
library(e1071)
library(caret)

#############
# Read Data #
#############

data <- read_parquet("AMRData.parquet")
literature_data <- read.csv("literature.csv")

generateLiterature <- function() {
  boxes <- list()
  
  for (i in 1:nrow(literature_data)) {
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



#################################
# Load Naive Bayes for bacteria #
################################
naive_data <- read.csv("models/naive/naive_data.csv")
naive <- readRDS("models/naive/naive.rds")

###################################
# Load GLM for E Coli predictions#
##################################
glm_data <- read.csv("models/glm/LRdata.csv")
glm <- readRDS("models/glm/glm.rds")


#############
# Create UI #
#############

ui <- dashboardPage(
  dashboardHeader(title = "Weyland-Yutani"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Summary", tabName = "summary"),
      menuItem("Model #1", tabName = "model1"),
      menuItem("Model #2", tabName = "model2"),
      menuItem("Literature", tabName = "literature"),
      menuItem("Data", tabName = "data")
    )
  ),
  dashboardBody(
    tabItems(
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
                 ourselves, but also for dedicated professionals, including veterinarians.
                 The intricacies involve diverse drug tiers, various classes of bacteria,
                 the phenomenon of horizontal gene transfer, and potentially more complexities.
                 It is unsurprising that many veterinarians may not readily engage in antimicrobial
                 stewardship, as addressing it comprehensively would constitute an additional
                 full-time commitment atop their already demanding responsibilities in pet care.
                 This is precisely where our role becomes crucial: alleviating a significant
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
                      "Which pathogens are most commonly found based on the time of the year,
                    the species, the county, and the source of the infection?",
                      style = "font-size: 24px;"
                    ),
                    p(
                      HTML(
                        "<strong>This model was implemented. Please see tab \"Model #1\"</strong>"
                      ),
                      style = "font-size: 16px;"
                    ),
                    p(
                      "A naive Bayes classifier appears most fitting for this inquiry, especially considering the independence of the columns in the classification.
                    We aim to predict a pathogen class that is discrete or categorical, encompassing 282 different types of pathogens in our dataset.
                    Our assumption is that the occurrence of a pathogen is unrelated to both the month and the species of the pet in question.
                    Alternatively, logistic regression could be employed to ascertain the presence of specific bacteria across different months.
                    However, this approach necessitates conducting individual tests for each type of bacteria, which would add complexity to the analysis.",
                      style = "font-size: 16px;"
                    ),
                    p("Model: naive Bayes classifier",
                      style = "font-size: 16px;"),
                    p("Independent variables: county, species, month, source",
                      style = "font-size: 16px;"),
                    p("Dependent variable(s): bacteria",
                      style = "font-size: 16px;"),
                    p(
                      "How does the antibiotic resistance profile of E.Coli in canines and felines differ between counties in North America given antibiotic R1?",
                      style = "font-size: 24px;"
                    ),
                    p(
                      HTML(
                        "<strong>This model was implemented. Please see tab \"Model #2\"</strong>"
                      ),
                      style = "font-size: 16px;"
                    ),
                    p("In this question, our independent variable is the categorical variable 'county', while the dependent variable is 'resistance' represented by either R(Resistant) or S(Susceptible)
                      which represents the likelihood of antibiotic resistance. Since our dependent variable is binary, we decided to use logistic regression to help answer this question. 
                      By employing logistic regression, we can effectively model how the county variable influences the likelihood of antibiotic resistance among E.coli.
                      This approach enables us to derive valuable insights regarding regional variations in antibiotic resistance profiles among canines and felines in North America.",
                      style = "font-size: 16px;"
                    ),
                    p("Model: Logistic Regression",
                      style = "font-size: 16px;"),
                    p("Independent variables: county",
                      style = "font-size: 16px;"),
                    p("Dependent variable(s): resistance",
                      style = "font-size: 16px;"),
                    p(
                      "What is the likelihood that escherichia coli in a companion animal will be resistant to a given antibiotic?",
                      style = "font-size: 24px;"
                    ),
                    p(
                      "A multinomial logistic regression model was chosen as the most appropriate method to answer this question. This is because
                    it allows us to label resistance as any of the various labels present in our data, as well as it being able to account for
                    the different magnitudes to which the independent variables affect resistence. This allows us to come much closer to representing
                    the real world scenario we are attempting to emulate.",
                      style = "font-size: 16px;"
                    ),
                    p("Model: multinomial naive Bayes classifier",
                      style = "font-size: 16px;"),
                    p(
                      "Independent variables: source, species, org_standard",
                      style = "font-size: 16px;"
                    ),
                    p("Dependent variable(s): resistance level",
                      style = "font-size: 16px;"),
                    
                    p(
                      "Is there a relationship between infection site and bacteria in canines?",
                      style = "font-size: 24px;"
                    ),
                    p(
                      "The model designed to address this question is tasked with predicting a multi-class nominal variable (bacteria) using a set of independent qualitative variables such as location, infection site, and the age of the animal.
                    Our community partner's data encompasses over 200 distinct bacteria, and the model is expected to calculate probabilities for each type of bacteria, given the independent variables.
                    In this context, a multinomial logistic regression model is highly appropriate since it is ideally suited for predicting dependent variables that fall into more than two categories.
                    A significant advantage of this approach is the lack of a requirement for normality in the training data.
                    This aspect offers considerable flexibility in the types of data that can be effectively utilized within the model, potentially enhancing its applicability and accuracy.",
                      style = "font-size: 16px;"
                    ),
                    p("Model: multinomial logistic regression classifier",
                      style = "font-size: 16px;"),
                    p("Independent variables: infection site",
                      style = "font-size: 16px;"),
                    p("Dependent variable(s): bacteria",
                      style = "font-size: 16px;"),
                    
                  )
                )
              ), ),
      tabItem(tabName = "summary",
              fluidRow(
                h1("Summary of the Data", style = "text-align: center;"),
                br(),
                box(
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
      tabItem(
        tabName = "model1",
        fluidRow(
          h1("Model #1", style = "text-align: center;"),
          h3(
            "Which pathogens are most commonly found based on the time of the year,
          the species, the county, and the source of the infection?",
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
        fluidRow(box(
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
        ))
      ),
      tabItem(
        tabName = "model2",
        fluidRow(
          h1("Model #2", style = "text-align: center;"),
          h3(
            "How does the antibiotic resistance profile of E.Coli in canines and felines differ between counties in North America given antibiotic R1?",
            style = "text-align: center;"
          ),
          br(),
        ),
        fluidRow(
          box(
            title = h4("Model Inputs", style = "text-align: center"),
            width = 6,
            selectInput(
              "glm_model_county_input",
              "Select County",
              choices = unique(data$county)
            )
          ),
          box(
            title = h4("Resistance Predictions", style = "text-align: center"),
            width = 6,
            plotlyOutput("glm_model_output")
          )
        ),
        fluidRow(box(
          title = h3("Model Overall Stats", style = "text-align: center;"),
          width = 12,
          tags$style(HTML(".center-code { text-align: center; }")),
          div(
            class = "center-code",
            code(
              "Accuracy : 0.3059",
              br(),
              "95% CI : (0.2954, 0.3166)",
              br(),
              "No Information Rate : 0.6951",
              br(),
              "P-Value [Acc > NIR] : 1",
              br(),
              "Kappa : 0",
              br(),
              "Mcnemar's Test P-Value : <2e-16"
            )
          )
        ))
      ),
      tabItem(tabName = "literature",
              fluidRow(
                h1("Literature Used", style = "text-align: center;"),
                box(width = 12,
                    do.call(fluidRow, generateLiterature()))
              )),
      tabItem(tabName = "data",
              fluidRow(
                h1("Download Data", style = "text-align: center;"),
                box(
                  width = 12,
                  align = "center",
                  downloadButton("downloadData", h2("Original")),
                  downloadButton("downloadNaiveData", h2("Model 1 Data")),
                  downloadButton("downloadGlmData", h2("Model 2 Data"))
                ),
                box(
                  title = h2("Data Table"),
                  width = 12,
                  DTOutput("dataTable")
                )
              ))
    )
  )
)

countyFilteredData <- function(select) {
  return(if (select == "All")
    data
    else
      data[data$county == select, ])
}

server <- function(input, output, session) {
  output$total_tests <- renderText({
    as.character(nrow(countyFilteredData(input$county_filter)))
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
      select_if(~ all(. >= 0.01))
    selected_columns <- as.matrix(selected_columns)
    other <- c(1 - rowSums(selected_columns))
    selected_columns <- cbind(selected_columns, other)
    
    plot_ly(
      labels = colnames(selected_columns),
      values = selected_columns[1, ],
      type = "pie"
    )
  })
  
  ## GLM model output
  output$glm_model_output <- renderPlotly({
    glm_input <- data.frame(county = input$glm_model_county_input)
    Susciptible <- predict(glm, glm_input, type = "response")
    Susciptible <- as.data.frame(Susciptible)
    selected_columns <- Susciptible %>%
    select_if(~ all(. >= 0))
    selected_columns <- as.matrix(selected_columns)
    Resistant <- c(1 - rowSums(selected_columns))
    selected_columns <- cbind(selected_columns, Resistant)
    
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
  output$downloadNaiveData <- downloadHandler(
    filename = function() {
      paste("AMRModel1Data", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(naive_data, file)
    }
  )
  output$downloadGlmData <- downloadHandler(
    filename = function() {
      paste("AMRModel2Data", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(glm_data, file)
    }
  )
  output$dataTable <- renderDT({
    datatable(data,
              options = list(scrollX = TRUE,
                             scrollY = TRUE))
  })
}

shinyApp(ui = ui, server = server)