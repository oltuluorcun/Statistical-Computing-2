
library(shiny)

# 1st application - playing with HTML !
ui_1 <- fluidPage(
    
    sidebarPanel(
        h1("Hello World !", align="center"),
        h4("This is my", strong("first"), "shiny app !"),
        br()
    ),
    
    mainPanel(
        h2("My name is Orcun Oltulu"),
        br(),
        p("I love ", span("METU.", style = "color:red")),
        br(),
        p("Here is a", a("LINK", 
                           href="https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/"), 
          "that you can find further information about shiny"),
        br(),
        p("Here is the lecture video for this week's topic: R Shiny"),
        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/YrllSpmoPaU" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
        
    )
   
)
server_1 <- function(input, output) {}

# Run the application 
shinyApp(ui = ui_1, server = server_1)

################################################################################

# 2nd application
# Calculate BMI for a person
# for multiple people

ui_2 <- fluidPage(
    
    titlePanel("BMI in shiny"),
    
    sidebarLayout(
        
        sidebarPanel(
            textInput(inputId = "Name", label = "Your Name"),
            textInput(inputId = "Surname", label = "Your Surname"),
            textInput(inputId = "Age", label = "Your age"),
            numericInput(inputId = "Height", 
                         label = "Please enter your height in cm",
                         value = 180),
            numericInput(inputId = "Weight",
                         label = "Please enter your weight in kg",
                         value = 70),
            actionButton(inputId = "Calculate", label = "Calculate")
        ),
        
        mainPanel(
            p(h3("Personel Information")),
            textOutput("NameSurname_out"),
            textOutput("Age"),
            br(),
            p(h3("BMI calculation")),
            textOutput("bmi_out"), 
        )
    )
)

server_2 <- function(input, output) {
    
    output$NameSurname_out <- renderText({
        input$Calculate
        isolate(paste("Name Surname:", input$Name, input$Surname))
    })
    
    output$Age <- renderText({
        input$Calculate
        isolate(paste("Age:", input$Age))
    })
    
    bmi <- reactive({
        input$Weight / (input$Height^2)*10000
    })
    
    output$bmi_out <- renderText({
        input$Calculate
        isolate(paste("BMI:", bmi()))
    })
}

shinyApp(ui_2, server_2)

################################################################################

# 3rd application 
# Generate a random sample from normal dist.
# plot the histogram

ui_3 <- fluidPage(
    sidebarPanel(
        h2("Generate a Random Sample from Normal Distribution", align="center"),
        sliderInput(inputId = "Size", 
                    label = "Choose Sample Size", 
                    value = 100, min = 10, max = 1000),
        numericInput(inputId = "MeanValue", 
                     label = "Mean", value = 0),
        numericInput(inputId = "StdValue", 
                     label = "Standard Deviation", value = 1),
        actionButton(inputId = "NewData", label = "New Data"),
        textInput(inputId = "Title", 
                  label = "Title", value = "Histogram"),
        selectInput(inputId = "Color", label = "Color",
                    choices = c("Blue","Gold", 
                                "Dark Green", "Purple", 
                                "Red","Orange")),
        checkboxInput(inputId = "Summary", 
                      label = "Check if you want to see summary statistics",
                      value = FALSE)
    ),
    
    mainPanel(
        plotOutput(outputId = "Hist"),
        verbatimTextOutput(outputId = "SummaryStatistics")
    )
)

server_3 <- function(input, output) {
    
    data <- eventReactive(input$NewData,{
        rnorm(input$Size, input$MeanValue, input$StdValue)
    })
    
    output$Hist <- renderPlot({
        hist(data(),
             col = input$Color,
             main = input$Title,
             xlab = "Random Sample")
    })
    
    summary_condition <- reactive({ input$Summary })
    
    output$SummaryStatistics <- renderPrint({
        if(summary_condition()) {
            summary(data())
        }
    })
    
}

# Run the application 
shinyApp(ui = ui_3, server = server_3)

################################################################################
