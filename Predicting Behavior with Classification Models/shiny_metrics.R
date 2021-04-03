library(shinydashboard)


colwidth <- 5

ui <- dashboardPage(
  dashboardHeader(title = "Behavior Analysis with Machine Learning and R", titleWidth = 500),
  
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Performance Metrics", tabName = "metrics", icon = icon("dashboard"))
    ), helpText("Try changing the values of the confusion matrix and see how the performance metrics change. P: positive instances. N: negative instances."),
    withMathJax(),
    helpText('$$accuracy = \\frac{TP+TN}{TP+TN+FP+FN}$$'),
    helpText('$$recall = \\frac{TP}{P}$$'),
    helpText('$$specificity = \\frac{TN}{N}$$'),
    helpText('$$precision = \\frac{TP}{TP+FP}$$'),
    helpText('$$F1score = 2\\frac{precision*recall}{precision+recall}$$')
    
  ),
  
  dashboardBody(
    
    fixedRow(
      column(width = 2, h3("")),
      column(width = 5, h3("True class"))
    ),
    
    fixedRow(
      column(1, offset = 3, HTML("<b>P</b>")),
      column(1, offset = 1, HTML("<b>N</b>"))
    ),
    
    fixedRow(
      column(width = 2, HTML("<h3 style='text-align:center;'></br>Predicted </br> class</h3>")),
      
      # fields
      column(width = 5,
             
             fixedRow(
               column(1, HTML("</br></br><b>P</b>")),
               column(width = colwidth,
                      div(id='my_tp', numericInput(
                        "tp",
                        "TP",
                        min = 0,
                        value = 90
                      ))),
               tags$style(type="text/css", "#my_tp {color : green;}"),
               column(width = colwidth,
                      div(id='my_fp', numericInput(
                        "fp",
                        "FP",
                        min = 0,
                        value = 2
                      ))),
               tags$style(type="text/css", "#my_fp {color : red;}")
             ),
             
             fixedRow(
               column(1, HTML("</br></br><b>N</b>")),
               column(width = colwidth,
                      div(id='my_fn', numericInput(
                        "fn",
                        "FN",
                        min = 0,
                        value = 3
                      ))),
               tags$style(type="text/css", "#my_fn {color : red;}"),
               column(width = colwidth,
                      div(id='my_tn', numericInput(
                        "tn",
                        "TN",
                        min = 0,
                        value = 5
                      ))),
               tags$style(type="text/css", "#my_tn {color : green;}")
             )
             
      )
      
    ),
    
    fluidRow(
      valueBoxOutput("boxAcc"),
      valueBoxOutput("boxRecall"),
      valueBoxOutput("boxSpec"),
      valueBoxOutput("boxPrec"),
      valueBoxOutput("boxF")
      
    )
  )
)



# Define server logic.
server <- function(input, output) {
    
    results <- reactive({
      
        tp <- input$tp
        tn <- input$tn
        fp <- input$fp
        fn <- input$fn
        
        accuracy <- (tp + tn) / (tp + tn + fp + fn)
      
        recall <- tp / (tp + fn)
        
        specificity <- tn / (tn + fp)
        
        precision <- tp / (tp + fp)
        
        fs <- 2 * ((precision*recall) / (precision+recall))
      
      list(accuracy=accuracy, recall=recall, specificity=specificity, precision=precision, fs=fs)
    })
    
    
    output$boxAcc <- renderValueBox({
      valueBox(
        formatC(results()$accuracy, digits = 4, format = "f"),
        "Accuracy", color = "yellow"
      )
    })
    
    
    output$boxRecall <- renderValueBox({
      valueBox(
        formatC(results()$recall, digits = 4, format = "f"),
        "Recall", color = "yellow"
      )
    })
    
    
    output$boxSpec <- renderValueBox({
      valueBox(
        formatC(results()$specificity, digits = 4, format = "f"),
        "Specificity", color = "yellow"
      )
    })
    
    
    output$boxPrec <- renderValueBox({
      valueBox(
        formatC(results()$precision, digits = 4, format = "f"),
        "Precision", color = "yellow"
      )
    })
    
    
    output$boxF <- renderValueBox({
      valueBox(
        formatC(results()$fs, digits = 4, format = "f"),
        "F1-score", color = "yellow"
      )
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
