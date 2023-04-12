library(shiny)
library(dplyr)
data <- read.csv("awesome-resources - Sheet1.csv")
data$url <- paste0("<a href='", data$link, "' target='_blank'>", data$link, "</a>")
# Define UI for application that draws a histogram
ui <- fluidPage(
  br(), 
  br(), 
  column(4, offset = 4, titlePanel("Anesthesiology, Pharmacology and Therapeutics: Resources")),
  tags$figure(
    align = "right",
    tags$img( src = "apt.jpeg", align = "left", height = "40%", width = "40%")), 
  br(), br(), br(), br(), br(), br(), br(), br(),
  br(),
  br(),
  tags$hr(style = "border-color: Blue;"), 
  h4("APT Students and Faculty Resources at the Tip of your Fingers!"), 
  h6("This app gives you a curated list of opportunities for students in the department of Anesthesiology, Pharmacology and Therapeutics at UBC. You can filter the opportunities available by level and type"),
  tags$hr(style = "border-color: Blue;"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gradlev", 
                  label = "Level", 
                  choices = c("undergrad", "Grad", "all", "Faculty")),
      
      checkboxGroupInput(inputId = "type",
                         label = "Type",
                         choices = unique(data$type),
                         selected = "research")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"),
      dataTableOutput("table")
    )
  )
)


server <- function(input, output) {
  ind <- which(colnames(data) %in% c("level", "link"))
  filtered_data <- reactive({data[-ind] %>% 
      filter(data$level == input$gradlev,
             data$type %in% input$type)})
  
  output$table <- renderDataTable(data[-ind] %>% 
                                    filter(data$level == input$gradlev,
                                           data$type %in% input$type),  
                                  escape = FALSE)
  output$text <- renderText(paste(nrow(filtered_data()), "results have been generated!"))
}

# Run the application 
shinyApp(ui = ui, server = server)

