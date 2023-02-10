
library(shiny)
library(lavaan)
library(DiagrammeR)
library(tidyverse)

source("scripts/gen_starts.R")
source("scripts/usefulFunctions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Plotting Patterns of Stability"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("This app generates data based on the parameter values you specify. It then shows what the patterns of stability would look like for increasingly long intervals. You can also upload a correlation matrix for your own two-variable data to compare."),
            numericInput("w",
                         "Number of Waves:",
                         min = 2,
                         max = 50,
                         value = 10,
                         width = "30%"
                         ),
            helpText("This option is ignored if you provide your own data. The number of waves will be set to the number of waves in the data that you provide"),
            br(),
            h4("Enter Your Own Data (Optional)"),
            fileInput(
                "file1",
                "Choose CSV File",
                accept = c(
                    "text/csv",
                    "text/comma-separated-values, text/plain",
                    ".csv"
                ),
                width = "50%"
            ),
            helpText("You can optionally upload a matrix of correlations (with the same number of waves as you specify in the app) and the app will plot these stabilities, too. You should upload a symmetric correlation matrix as a csv file, and the variables should be ordered by wave (e.g., X1, Y1, X2, Y2,...)"),
            p(),
            h2("Specify The Data Generating Model"),
            helpText("The model to the right shows the possible components you can include. If you include stable trait variance, state variance, and autoregressive variance, the data-generating model will be the STARTS model. You can specify variance components to be zero to fit reduced models (like the RI-CLPM or CLPM). Use the buttons below to select a default model that you can then modify. Also note that you can specify values for which the data are impossible to generate (e.g., high stability plus strong cross-lagged paths). If you get an error, just try adjusting the values you set."),
            radioButtons(
                "modelType",
                "Initial Model Type",
                choices = c("CLPM",
                            "RI-CLPM",
                            "STARTS",
                            "ARTS",
                            "Stable Trait"),
                inline = TRUE
                ),
            h4("Variances"),
            fluidRow(
                column(4, numericInput("st_x",
                                       "X Stable Trait",
                                       min = 0,
                                       step = .05,
                                       value = 0)),
                column(4, numericInput("ar_x",
                                       "X Autoregressive",
                                       min = 0,
                                       step = .05,
                                       value = 1)),
                column(4, numericInput("state_x",
                                       "X State",
                                       min = 0,
                                       step = .05,
                                       value = 0))),
            fluidRow(
                column(4, numericInput("st_y",
                                       "Y Stable Trait",
                                       min = 0,
                                       step = .05,
                                       value = 0)),
                column(4, numericInput("ar_y",
                                       "Y Autoregressive",
                                       min = 0,
                                       step = .05,
                                       value = 1)),
                column(4, numericInput("state_y",
                                       "Y State",
                                       min = 0,
                                       step = .05,
                                       value = 0))),
            h4("Autoregressive Parameters"),
            fluidRow(
                column(4, numericInput("stability_x",
                                       "Stability of X",
                                       min = 0,
                                       max = 1,
                                       step = .05,
                                       value = .5)),
                column(4, numericInput("stability_y",
                                       "Stability of Y",
                                       min = 0,
                                       max = 1,
                                       step = .05,
                                       value = .5))),
            fluidRow(
                column(4, numericInput("XonY",
                                       "Cross-lag: Y Predicting X",
                                       min = -1,
                                       max = 1,
                                       step = .05,
                                       value = .2)),
                column(4, numericInput("YonX",
                                       "Cross-lag: X Predicting Y",
                                       min = -1,
                                       max = 1,
                                       step = .05,
                                       value = .2))
            ),
            h4("Correlations"),
            fluidRow(
                column(4, numericInput("st_cor",
                                       "Correlation Between Stable Traits:",
                                       min = -1,
                                       max = 1,
                                       step = .05,
                                       value = .5)),
                column(4, numericInput("ar_cor",
                        "Correlation Between Initial AR:",
                        min = -1,
                        max = 1,
                        step = .05,
                        value = .5))
            ),
            p("Source code is available here:", tags$a(href = "https://github.com/rlucas11/compare-cor", target = "_blank", "https://github.com/rlucas11/compare-cor")),
            p("For a discussion of why these patterns of stability are important consider, see this preprint on the problems with the cross-lagged panel model: ", tags$a(href = "https://psyarxiv.com/pkec7/", target = "_blank", "https://psyarxiv.com/pkec7/"))
        ),

        # Show data generating model and results
        mainPanel(
            h3("Specified Model"),
            grVizOutput('starts', width = "50%"),
            h3("Stability Coefficients"),
            plotOutput(
                outputId = "corPlot",
                width="600px"
            )
        )
    )
)

# Draw STARTS Model
server <- function(input, output) {
    ## Get values depending on modelType selection
    observe({
        if (input$modelType == "CLPM") {
            st_x <- 0
            st_y <- 0
            ar_x <- 1
            ar_y <- 1
            state_x <- 0
            state_y <- 0
        }
        if (input$modelType == "RI-CLPM") {
            st_x <- 1
            st_y <- 1
            ar_x <- 1
            ar_y <- 1
            state_x <- 0
            state_y <- 0
        }
        if (input$modelType == "STARTS") {
            st_x <- 1
            st_y <- 1
            ar_x <- 1
            ar_y <- 1
            state_x <- 1
            state_y <- 1
        }
        if (input$modelType == "ARTS") {
            st_x <- 0
            st_y <- 0
            ar_x <- 1
            ar_y <- 1
            state_x <- 1
            state_y <- 1
        }
        if (input$modelType == "Stable Trait") {
            st_x <- 1
            st_y <- 1
            ar_x <- 0
            ar_y <- 0
            state_x <- 1
            state_y <- 1
        }
        updateNumericInput(
            inputId = "st_x",
            value = st_x
        )
        updateNumericInput(
            inputId = "st_y",
            value = st_y
        )
        updateNumericInput(
            inputId = "ar_x",
            value = ar_x
        )
        updateNumericInput(
            inputId = "ar_y",
            value = ar_y
        )
        updateNumericInput(
            inputId = "state_x",
            value = state_x
        )
        updateNumericInput(
            inputId = "state_y",
            value = state_y
        )
    })
    ## Change color for missing elements
    no_state_y <- reactive({
        if(input$state_y==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change graphviz
    ## Change color for missing elements
    no_st_x <- reactive({
        if(input$st_x==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_st_y <- reactive({
        if(input$st_y==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_state_x <- reactive({
        if(input$state_x==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_ar_x <- reactive({
        if(input$ar_x==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_ar_y <- reactive({
        if(input$ar_y==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_yx <- reactive({
        if(input$YonX == 0 |
           input$ar_x == 0 |
           input$ar_y == 0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_xy <- reactive({
        if(input$XonY==0 |
           input$ar_x == 0 |
           input$ar_y == 0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    ## Change color for missing elements
    no_trait_r <- reactive({
        if(input$st_cor==0 |
           input$st_x==0 |
           input$st_y==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
        }
    })
    ## Change color for missing elements
    no_ar_r <- reactive({
        if(input$ar_cor==0 |
           input$ar_x==0 |
           input$ar_y==0) {
            obj_color <- "style='invis'"
        } else {
            obj_color <- "style=''"
            }
    })
    


    output$starts <- renderGrViz({
        grViz( " digraph D {
  ranksep=.5;
  node [shape = ellipse];
  STX [label='X Stable\nTrait', @@1]; 
  STY [label='Y Stable\nTrait', @@2];
  node [shape = ellipse, width = 1, @@5];
  ARX1 ARX2 ARX3 ARX4 ARX5;
  node [shape = ellipse, width = 1, @@6];
  ARY1 ARY2 ARY3 ARY4 ARY5;
  node [shape = box, width = 1, style=''];
  X1 X2 X3 X4 X5
  Y1 Y2 Y3 Y4 Y5
  node [shape = circle, width = .05, fontsize='10', @@3];
  sx1 sx2 sx3 sx4 sx5
  node [shape = circle, width = .05, fontsize='10', @@4];
  sy1 sy2 sy3 sy4 sy5
    
  {rank = same X1 X2 X3 X4 X5}
  {rank = same ARX1 ARX2 ARX3 ARX4 ARX5}
  {rank = same ARY1 ARY2 ARY3 ARY4 ARY5}
  {rank = same Y1 Y2 Y3 Y4 Y5}
  {rank = same sx1 sx2 sx3 sx4 sx5}
  {rank = same sy1 sy2 sy3 sy4 sy5}
  
  edge [dir = back, label = '', @@5];
  X1 -> ARX1
  X2 -> ARX2
  X3 -> ARX3
  X4 -> ARX4
  X5 -> ARX5
  
  edge [dir = ''];
  ARX1 -> ARX2 -> ARX3 -> ARX4 -> ARX5
  
  edge [label ='', @@7]
  ARX1 -> ARY2
  ARX2 -> ARY3
  ARX3 -> ARY4
  ARX4 -> ARY5
  edge [label ='', @@8]
  ARY1 -> ARX2
  ARY2 -> ARX3
  ARY3 -> ARX4
  ARY4 -> ARX5
  
  edge [label ='', @@6]
  ARY1 -> ARY2 -> ARY3 -> ARY4 -> ARY5
  
  ARY1 -> Y1
  ARY2 -> Y2
  ARY3 -> Y3
  ARY4 -> Y4
  ARY5 -> Y5

  edge [@@1]
  STX -> {X1, X2, X3, X4, X5}
  
  edge [dir=back, @@2];
  {Y1, Y2, Y3, Y4, Y5} -> STY
  
  edge [dir='', @@3]
  
  sx1 -> X1
  sx2 -> X2
  sx3 -> X3
  sx4 -> X4
  sx5 -> X5
  
  edge [dir=back, @@4]
  Y1 -> sy1
  Y2 -> sy2
  Y3 -> sy3
  Y4 -> sy4
  Y5 -> sy5
  
  edge [style=invis]
  STX -> {sx1, sx2, sx3, sx4, sx5}
  {sy1, sy2, sy3, sy4, sy5} -> STY
  
  edge [style = '', dir=both, label = '', @@10];
  ARX1 -> ARY1
  edge [@@9]
  STX -> STY
  }
[1]: no_st_x()
[2]: no_st_y()
[3]: no_state_x()
[4]: no_state_y()
[5]: no_ar_x()
[6]: no_ar_y()
[7]: no_yx()
[8]: no_xy()
[9]: no_trait_r()
[10]: no_ar_r()
"
  )})
    
    
    output$corPlot <- renderPlot({
        ## Check for and load user data
        inFile <- input$file1
        if (!is.null(inFile)) {
            userInput <- TRUE
            userCorMat <- as.matrix(read.csv(inFile$datapath))
            userCors <- summarizeR(userCorMat, 2)
            nwaves <- ncol(userCorMat) / 2
        } else {
            userInput <- FALSE
            nwaves <- input$w
        }
        data <- gen_starts(
            n = 10000,
            nwaves = nwaves, # Number of waves
            ri_x = input$st_x, # Random intercept variance for X
            ri_y = input$st_y, # Random intercept variance for Y
            cor_i = input$st_cor, # Correlation between intercepts (as correlation)
            x = input$ar_x, # AR variance for X
            y = input$ar_y, # AR variance for Y
            stab_x = input$stability_x, # Stability of X
            stab_y = input$stability_y, # Stability of Y
            yx = input$YonX, # Cross lag (Y regressed on X)
            xy = input$XonY, # Cross lag (X regressed on Y)
            cor_xy = input$ar_cor, # Correlation between X and Y (as correlation)
            xr = input$state_x, # Measurement error for X
            yr = input$state_y # Measurement error for Y
        )
        ## Reorder generated data for summarizeR
        data <- data[, paste0(c("x", "y"), rep(1:nwaves, each = 2))]
        cors <- summarizeR(cor(data), 2)
        if (!is.null(inFile)) {
            cors <- cbind(cors, userCors)
        }
            plotCors(cors, user = userInput)
        })
}



    

# Run the application 
shinyApp(ui = ui, server = server)
