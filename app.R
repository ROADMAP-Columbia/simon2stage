#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(clinfun)
library(shinyWidgets)
library(diagram)


# Define UI for application that draws a histogram
ui = shiny::fluidPage(
    shiny::titlePanel("Simon's two-stage design"),
    
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::p("This program generates Simon's optimal two-stage designs (Simon, 1989) for Phase II single arm clinical trials."),
            
            shiny::h4("Design parameters"),
            
            
            shiny::numericInput(
                "p1",
                label = withMathJax("Acceptable response rate, p1 (alternative hypothesis):"), 
                value = 0.3,
                min = 0,
                max = 1,
                step = 0.01
            ),
            
            shiny::numericInput(
                "p0",
                label = withMathJax("Unacepptable response rate, p0 (null hypothesis):"), 
                value = 0.1,
                min = 0,
                max = 1,
                step = 0.01
            ),
            
            shiny::numericInput(
                "alpha",
                label = withMathJax("Type I error,  $\\alpha$ (one-sided):"),
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.005
            ),
            
            shiny::numericInput(
                "power",
                label = withMathJax("Power,  1 - $\\beta$:"),
                value = 0.80,
                min = 0,
                max = 1,
                step = 0.005
            ),
            
            shiny::numericInput(
                "n",
                label = "Maximum Sample Size, n:",
                value = 100,
                min = 100,
                max = 500,
                step = 1
            ), 
            
            awesomeRadio(
                "method", 
                "Method:",
                c("Minimax"= "minimax", 
                  "Optimal" = "optimal")
            )
        ),
        shiny::mainPanel(
            tabsetPanel(
            tabPanel("Results",
            shiny::h4("Sample size calculation for phase II trials using the Simon's two-stage design"),
            dataTableOutput("tab1"),
            
            shiny::em("Note: Note: r1 - the threshold for the first stage to stop the trial for futility, 
                      r - overall threshold to stop the trial for futility, 
                      n1 - number of accrual subjects for stage I, 
                      n - total number of subjects, 
                      EN(p0) - expected sample size for the trial when the true response rate is p0, 
                      PET(p0) - probability of early termination when the true response rate is p0."),
            
            shiny::h4("For the design"),
            shiny::textOutput("text1"),
            
            shiny::h4("Design Summary"),
            shiny::plotOutput("plot1"),
            
            shiny::h4("Reference"),
            h6("Simon R (1989). Optimal two-stage designs for phase II clinical trials, Controlled Clinical Trials 10: 1-10.")), 
            tabPanel("Design Summary", 
                     tags$iframe(style = "height:400px; width:100%; scrolling=yes", 
                                 src = "simon2stage_design.pdf"))
            
            )
        )
    )
)

# Define server logic required to draw a histogram
server = function(input, output) {
    
    output$tab1 <- renderDataTable({
        fun <- clinfun::ph2simon(input$p0, input$p1, input$alpha, (1 - input$power), input$n)
        tab <- rbind(fun$out[which.min(fun$out[, 5]), ], fun$out[1, ])
        tab <- round(tab, 3)
        tab <- cbind(" " = c("Optimal", "Minimax"), tab)
        tab 
    }, extensions = 'Buttons', 
    options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), rownames= FALSE)
    
    
    output$text1 <- shiny::renderText({
        fun <- clinfun::ph2simon(
            input$p0, 
            input$p1, 
            input$alpha, 
            (1 - input$power), 
            input$n
            
        )
        
        if(input$method == "optimal"){
            paste(
                "Using Simon's optimal two-stage design, if you see more than",
                fun$out[which.min(fun$out[, 5]), 1],
                "responses out of the first",
                fun$out[which.min(fun$out[, 5]), 2],
                "participants in the first stage, then accrue to a total of",
                fun$out[which.min(fun$out[, 5]), 4],
                "participants. Otherwise if you see",
                fun$out[which.min(fun$out[, 5]), 1],
                "or fewer responses out of the first",
                fun$out[which.min(fun$out[, 5]), 2],
                "participants in the first stage, then stop the trial. After accruing",
                fun$out[which.min(fun$out[, 5]), 4],
                "participants, if you see more than",
                fun$out[which.min(fun$out[, 5]), 3],
                "responses, then the intervention is considered worthy of further testing.")
        }
        else{
            paste(
                "Using Simon's minimax two-stage design, if you see more than",
                fun$out[1, 1],
                "responses out of the first",
                fun$out[1, 2],
                "participants in the first stage, then accrue to a total of",
                fun$out[1, 4],
                "participants. Otherwise if you see",
                fun$out[1, 1],
                "or fewer responses out of the first",
                fun$out[1, 2],
                "participants in the first stage, then stop the trial. After accruing",
                fun$out[1, 4],
                "participants, if you see more than",
                fun$out[1, 3],
                "responses, then the intervention is considered worthy of further testing.")
        }
    })
    
    output$plot1 <- shiny::renderPlot({
        
        fun <- clinfun::ph2simon(
            input$p0, 
            input$p1, 
            input$alpha, 
            (1 - input$power), 
            input$n)
            
        par(mar = c(0, 0, 0, 0), mai=c(0,0,0,0))
        openplotmat()
        elpos <- coordinates (c(1, 2, 1, 2)) #, mx = 0.1, my = -0.1)
        elpos[2,] <- c(0.25, 0.6)
        elpos[3,] <- c(0.75, 0.6)
        elpos[5,] <- c(0.25, 0.1)
        elpos[6,] <- c(0.75, 0.1)
        
        straightarrow(from = elpos[1, ], to = elpos[2, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
        straightarrow(from = elpos[1, ], to = elpos[3, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
        straightarrow(from = elpos[2, ], to = elpos[4, ], arr.pos=0.76, arr.col = "blue", lcol = "blue")
        straightarrow(from = elpos[4, ], to = elpos[5, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
        straightarrow(from = elpos[4, ], to = elpos[6, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
        
        if(input$method == "optimal"){ 
            a <- paste0("Enroll ",  fun$out[which.min(fun$out[, 5]), 2], " subjects in first stage \n and test their response")
            b <- paste0("If ", (fun$out[which.min(fun$out[, 5]), 1] + 1), " or more response are observed, \n then another ", fun$out[1, 4] - fun$out[1, 2], " subjects are enrolled")
            c <- paste("Enroll ", (fun$out[which.min(fun$out[, 5]), 4] - fun$out[which.min(fun$out[, 5]), 2]), " subjects in second \n stage")
            d <- paste0("If ", fun$out[which.min(fun$out[, 5]), 3] + 1, " or more responses are \n observed, then reject H0")
        }
        else{
            a <- paste0("Enroll ",  fun$out[1, 2], " subjects in first stage \n and test their response")
            b <- paste0("If ", (fun$out[1, 1] + 1), " or more response are observed, \n then another ", fun$out[1, 4] - fun$out[1, 2], " subjects are enrolled")
            c <- paste("Enroll ", (fun$out[1, 4] - fun$out[1, 2]), " subjects in second \n stage")
            d <- paste0("If ", fun$out[1, 3] + 1, " or more responses are \n observed, then reject H0")
        }
        
        textround(elpos[1, ], lab = a,
                  cex = 1.5, radx = 0.162, rady=0.05, shadow.size=0)
        textround(elpos[2, ], lab = b,
                  cex = 1.5, radx=0.162, rady=0.05, shadow.size=0)
        textround(elpos[3, ], lab = "Otherwise, fail to reject H0  \n and early terminate the study",
                  cex = 1.5, radx=0.162, rady=0.05, shadow.size=0)
        textround(elpos[4, ], lab = c,
                  cex = 1.5, radx=0.162, rady=0.05, shadow.size=0)
        textround(elpos[5, ], lab = d,
                  cex = 1.5, radx=0.162, rady=0.05, shadow.size=0)
        textround(elpos[6, ], lab = "Otherwise, fail to reject H0",
                  cex = 1.5, radx=0.162, rady=0.05, shadow.size=0)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
