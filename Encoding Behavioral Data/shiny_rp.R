library(shinydashboard)
library(pheatmap)

DM <- NULL

# Euclidean distance
norm2 <- function(x, y){
  return(sqrt((x - y)^2))
}

# Function that computes a distance matrix and a RP
# and returns both.
rp <- function(x, e, f=norm2){
  #x: vector.
  #e: threshld.
  #f: norm (distance function).
  N <- length(x)
  
  # This variable will store the recurrence plot.
  M <- matrix(nrow=N, ncol=N)
  
  # This variable will store the distance matrix.
  D <- matrix(nrow=N, ncol=N)
  
  for(i in 1:N){
    for(j in 1:N){
      
      # Compute distance between a pair of points.
      d <- f(x[i], x[j])
      
      # Store result in D.
      # Start filling values from bottom left.
      D[N - (i-1), j] <- d 
      
      if(d <= e){
        M[N - (i-1), j] <- 1
      }
      else{
        M[N - (i-1), j] <- 0
      }
    }
  }
  return(list(D=D, RP=M))
}



ui <- dashboardPage(
  
  dashboardHeader(title = "Behavior Analysis with Machine Learning and R", titleWidth = 500),
  
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Recurrence plots", tabName = "rp", icon = icon("th"))
    ), helpText("Choose a file from the HAND GESTURES dataset."),
    
    fileInput("file1", "Choose file",
              multiple = FALSE,
              accept = c(".txt")),
    tags$style(type="text/css", ".shiny-file-input-progress { display: none }"),
    
    selectInput("axis", "axis:",
                choices=c("X","Y","Z"), selected = "X"),
    
    sliderInput("slider1", "Threshold:", 0, 5, 0.5, step=0.1)
    
    
  ),
  
  dashboardBody(
    
    fluidRow(
      box(title="raw data", plotOutput("raw"), width = 12)
    ),
    
    fluidRow(
      box(title="Distance Matrix", plotOutput("dm")),
      box(title="Recurrence Plot", plotOutput("rp"))
    )
    
    
  )#end of body
)

# Define server logic.
server <- function(input, output) {
  
    df <- reactive({
      
        filepath <- input$file1$datapath
        
        df <- NULL
        if(is.character(filepath))df <- read.csv(filepath, header = F)

      list(df=df)
    })
    
    DM <- reactive({
      DM <- NULL
      if(is.null(df()$df)==F){
        selAxis <- input$axis
        selAxis <- which(c("X","Y","Z") == selAxis)
        resrp <- rp(df()$df[,selAxis], 0.5, norm2)
      }
      DM <- resrp$D
      DM
    })
    
    output$raw <- renderPlot({
      if(is.null(df()$df)==F){
        selAxis <- input$axis
        selAxis <- which(c("X","Y","Z") == selAxis)
        
        plot(df()$df[,selAxis],
             type="l", main="", xlab = "time", ylab = "Acceleration")
      }
      
    })
    
    
    
    output$dm <- renderPlot({
      if(is.null(df()$df)==F){
        
        pheatmap(DM(), main="", cluster_row = FALSE,
                 cluster_col = FALSE,
                 show_rownames = T,
                 show_colnames = T,
                 legend = F,
                 color = colorRampPalette(c("white", "black"))(50))
      }
      
    })
    
    output$rp <- renderPlot({
      if(is.null(df()$df)==F){

        if(is.null(DM()) == F){
          
          RP <- matrix(0, nrow=nrow(DM()), ncol=nrow(DM()))
          RP[DM() <= input$slider1] <- 1
          
          pheatmap(RP, main="", cluster_row = FALSE,
                 cluster_col = FALSE,
                 show_rownames = T,
                 show_colnames = T,
                 legend = F,
                 color = colorRampPalette(c("white", "black"))(50))
        }
      }
      
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
