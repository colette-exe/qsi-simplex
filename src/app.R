# Angelica Nicolette U. Adoptante
# CMSC 150 - B1L
# 2020-01692
# Exercise 10: Integration

# Shiny

# libraries
library(rlang)
library(shiny)
library(shinythemes)

#source codes
source("AdoptanteEx08.R")
source("AdoptanteEx09.R")

x = y = vector()

initial_list <<- list()
list_fxns <<- list()
n_list <<- list()
z <<- ""
index <<- 1
enteredZ <<- FALSE
num_terms <<- vector()
numeric_terms <<- vector()
each_term <<- vector()
each_temp <<- list()
z_num_terms <<- vector()
z_coefficient <<- vector()
empty <<- TRUE

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$br(),
titlePanel("QSI & Simplex Calculator"),
  tags$br(),

 tabsetPanel(
   #Home
   tabPanel("Home",
            wellPanel(
              #Welcome
              tags$h2("Welcome")
              ,tags$br()
              ,tags$p("This is an app that enables you 
                      to use Quadratic Spline Interpolation and Simplex Methods of computation for
                      a range of values you'll be entering.")
              ,tags$br()
              ,tags$h4("Brief Definitions")
              ,fluidRow(
                column(4,
                       tags$b("Quadratic Spline Interpolation")
                )
                ,column(8,
                       tags$em('"In numerical analysis: Defined as a method of constructing new data points
                               from a discrete set of known data points.
                               Quadratic Interpolation (also known as Parabolic) uses second-order
                               polynomial/s connecting three points."')
                )
                
              )
              ,tags$br()
              ,fluidRow(
                column(4,
                       tags$b("Simplex Method")
                )
                ,column(8,
                        tags$em('"The simplex method is a linear programming method which is based 
                                on the assumption that the optimum will be at an extreme point. 
                                The constraint inequalities in this method are reformulated as 
                                equalities by using slack variables."')
                )
              )
              ,tags$br()
              ,tags$br()
              ,tags$cite("Definitions provided by the University of the Philippines Los Baños")
              ,tags$br()
              ,tags$br()
              
              
              #Instructions
             ,tags$h2("Instructions") 
             ,tags$br()
             ,tags$h4("Navigate through this app using the options in the menu bar, just below the title.")
             ,tags$br()
             ,tags$p("For Quadratic Spline Interpolation all you have to do is type the values: the value to be evaluated, the independent, and dependent values. 
                     You can check if the values you entered are correct by observing the reactive printing of the vectors just next to the input boxes.
                     Once you've checked the values, hit 'Compute' and the solution will appear below, on the 'Solution's part.")
             ,tags$p("To move on to your next computation, just write your value to evaluate, x and y values again then hit 'Compute'.")
             ,tags$b("qsi.fxns are the polynomials per interval, and y is your value x evaluated at the appropriate function for x's interval")
             ,tags$br()
             ,tags$br()
             ,tags$p("For Simplex Method, type your constraints and Z equation. The 'Remove' button will remove the last function you entered, 
                     while retyping the Z equation will rewrite your previously entered Z equation. The 'Reset' button will erase all of the functions you previously entered.")
             ,tags$p("You will be able to see all of the functions you entered below the input boxes. The solution will show the final tableau, basic solution, and optimal value.")
             ,tags$p("For maximization, your slack variables are the x variables, and the opposite for minimization.")
             ,tags$p("You must strictly follow this format: ")
             ,tags$p("1) When entering equations, you must always include the coefficient regardless if it's 1 or not;")
             ,tags$p("2) You must always include all variables, ex.: 1x1 + 0x2 <= 9;")
             ,tags$p("3) This program doesn't check whether you put the right variables, and their subscripts, or not. It assumes all of your equations have the right order of variables. (x1 x2 x3 x4 ...);")
             ,tags$p("4) You must not 
                     forget to add 1 to x1, this program does not accept x1 as a valid term. Please bear that in mind.")
             ,tags$p("5) You must always input equations with the same number of variables.")
             ,tags$br()
             ,tags$br()
             
      )
   )
   
   ,tabPanel("Test Cases"
      ,fluidRow(
        wellPanel(
          tags$h3("You can use these test cases for easy testing.", align = "center")
        )
        ,column(4,
          tags$h3("For Quadratic Spline Interpolation")
          ,tags$p("value to be evaluated: 5")
          ,tags$p("x-values: 3.0, 4.5, 7.0, 9.0")
          ,tags$p("y-values: 2.5, 1.0, 2.5, 0.5")
        )
        ,column(8,
          tags$h3("For Simplex Method")
          ,tags$p("Copy these equations one-by-one:")
          ,tags$h4("Maximize:")
          ,tags$p("Z = 150x1 + 175x2")
          ,tags$p("7x1 + 11x2 <= 77")
          ,tags$p("10x1 + 8x2 <= 80")
          ,tags$p("1x1 + 0x2 <= 9")
          ,tags$p("0x1 + 1x2 <= 6")
          ,tags$h4("Minimize:")
          ,tags$p("Z = 10x1 + 8x2 + 6x3 + 5x4 + 4x5 + 6x6 +5x7 + 4x8 + 3x9 + 6x10 + 3x11 + 4x12 + 5x13 + 5x14 + 9x15")
          ,tags$p("1x1 + 1x2 + 1x3 + 1x4 + 1x5 + 0x6 + 0x7 + 0x8 + 0x9 + 0x10 + 0x11 + 0x12 + 0x13 + 0x14 + 0x15 <= 310")
          ,tags$p("0x1 + 0x2 + 0x3 + 0x4 + 0x5 + 1x6 + 1x7 + 1x8 + 1x9 + 1x10 + 0x11 + 0x12 + 0x13 + 0x14 + 0x15 <= 260")
          ,tags$p("0x1 + 0x2 + 0x3 + 0x4 + 0x5 + 0x6 + 0x7 + 0x8 + 0x9 + 0x10 + 1x11 + 1x12 + 1x13 + 1x14 + 1x15 <= 280")
          ,tags$p("1x1 + 0x2 + 0x3 + 0x4 + 0x5 + 1x6 + 0x7 + 0x8 + 0x9 + 0x10 + 1x11 + 0x12 + 0x13 + 0x14 + 0x15 >= 180")
          ,tags$p("0x1 + 1x2 + 0x3 + 0x4 + 0x5 + 0x6 + 1x7 + 0x8 + 0x9 + 0x10 + 0x11 + 1x12 + 0x13 + 0x14 + 0x15 >= 80")
          ,tags$p("0x1 + 0x2 + 1x3 + 0x4 + 0x5 + 0x6 + 0x7 + 1x8 + 0x9 + 0x10 + 0x11 + 0x12 + 1x13 + 0x14 + 0x15 >= 200")
          ,tags$p("0x1 + 0x2 + 0x3 + 1x4 + 0x5 + 0x6 + 0x7 + 0x8 + 1x9 + 0x10 + 0x11 + 0x12 + 0x13 + 1x14 + 0x15 >= 160")
          ,tags$p("0x1 + 0x2 + 0x3 + 0x4 + 1x5 + 0x6 + 0x7 + 0x8 + 0x9 + 1x10 + 0x11 + 0x12 + 0x13 + 0x14 + 1x15 >= 220")
        )
      )
      )
   
   #exercise 8: quadratic spline interpolation
   ,tabPanel("Quadratic Spline Interpolation"
     ,fluidRow(
         column(10,
            tags$h3("Quadratic Spline Interpolation")
            ,tags$br()
            ,sidebarPanel(
              numericInput(inputId = "to_eval", "Enter value to evaluate", value = 0)
              ,textInput(inputId = "x_vec", "Enter x values", placeholder = "1,2,3")
              ,textInput(inputId = "y_vec", "Enter y-values", placeholder = "4, 5, 6")
            ),
            mainPanel(
              tags$h4("x-values"),
              verbatimTextOutput("x_values"),
              tags$h4("y-values"),
              verbatimTextOutput("y_values"),
              actionButton(inputId = "solve", label = "Compute")
              
            )
     )
     ,column(12,
             tags$hr(),
             tags$h4("Solution"),
             verbatimTextOutput("solution.qsi")
     )
     )
   ) #----
   
   #exercise 9: simplex method
   ,tabPanel("Simplex Method"
       ,tags$h3("Simplex Method")
       ,tags$br()
       ,fluidPage(
         fluidRow(
           column(4,
              textInput(inputId = "equations", "Type constraints equations here:", placeholder = "2x1 + 3x2 <= 5   or   4x1 + 3x2 >= 8")
              ,actionButton(inputId = "add", label = "Add Equation")
              ,actionButton(inputId = "remove", label = "Remove")
          ) 
          ,column(5, offset = 2,
            textInput(inputId = "equationZ", "Type Z equation here:", placeholder = "Z = 3x1 + 4x2")
            ,actionButton("addZ", "Add equation")
          )
          ,column(6,
             tags$br()
             ,tags$hr()
             ,tags$h4("Choose operation")
             ,actionButton(inputId = "min", label = "Minimization")
             ,actionButton(inputId = "max", label = "Maximization")
             ,actionButton(inputId = "reset.simplex", label = "Reset")
          )
         )
         
       )
      ,tags$hr()
      ,fluidRow(
        column(4,
         tags$h4("Equations")
         ,verbatimTextOutput("equations_text")
         ,verbatimTextOutput("equationZ_text")
        )
        ,column(8,
          tags$h4("Solution")
          ,verbatimTextOutput("solution.simplex")
        )
      )
   )
   ,tabPanel("About",
             tags$br()
             ,tags$br()
             ,tags$br()
               ,column(5, align = "center", offset = 3,
                      tags$p("Programmed by Angelica Nicolette U. Adoptante")
                      ,tags$p("CMSC 150 - B1L")
                      ,tags$p("Exercise 10")
                      ,tags$br()
                      ,tags$br()
                      ,tags$br()
                      ,tags$br()
                      ,tags$em("I hope you liked what I made!")
        )
      )
   ,tabPanel("Themes"
      ,shinythemes::themeSelector()
    )
 ) 
)

server <- function(input, output) {
#----QSI-------------------------
  output$x_values <- renderPrint({
    x <<- as.numeric(unlist(strsplit(input$x_vec, ",")))
    print(x)
  })
  output$y_values <- renderPrint({
    y <<- as.numeric(unlist(strsplit(input$y_vec, ",")))
    print(y)
  })

  
  
  observe({
    if (input$solve > 0) {
      output$solution.qsi <- renderPrint({
        answer <- poly.qsi(list(x,y), input$to_eval)
        print(answer)
      })
    }
  })
#--------------------------------
  
#----Simplex---------------------
fxns <- reactiveValues()
fxns_num <- reactiveValues()

#inputs-------------------------
  #adding
observeEvent(
  input$add,
  {
    flip <<- FALSE
    if (isTRUE(isolate(input$equations)) == ""){
      output$equations_text <- renderPrint({
        "Enter Equation"
     })
    }
    else {
      #split <= or >=
      if (grepl("<=", isolate(input$equations))) {
        funct = strsplit(isolate(input$equations), "<=")
        flip <<- TRUE
      }
      #must multiply by -1
      else if (grepl(">=", isolate(input$equations))){
        funct = strsplit(isolate(input$equations), ">=")
      }
      else {
        funct = NA
      }
      
      #checking if equation entered is valid
      if (is.na(funct)){ #if NA, prompt user
        output$equations_text <- renderPrint({
          "Invalid Equation"
        })
      } else { #if valid, convert to numeric and store in a new list
        list_fxns[[index]] <<- isolate(input$equations)
        fxns$dList <<- list_fxns
        output$equations_text <- renderPrint({
          fxns$dList
        })
        
        #split terms
        terms <- strsplit(funct[[1]][1], "\\+")
        
        #get each coefficient
        for (i in 1:length(terms[[1]])){
          each_term[i] <<- (strsplit(terms[[1]][i], "x")[1])
          #store in a vector
          for (j in 1:length(each_term)){
            if (each_term[[j]][1] == "") {
              num_terms[j] <<- 1
              }
            else {
              num_terms[j] <<- eval(parse(text = each_term[[j]][1]))
            }
            empty <<- FALSE
          }
        }
      }
      #add rhs at the end of the vector
      num_terms[(length(num_terms))+1] = eval(parse(text = funct[[1]][2]))
      #if flip
      if (flip) num_terms = num_terms*(-1)
      
      #n_list <<- c(isolate(fxns_num$dList), isolate(num_terms))
      n_list[[index]] <<- num_terms
      index <<- index + 1
      fxns_num$dList <<- n_list
    }
  }
)

  #removing
observeEvent(
  input$remove,{
    fxns$dList <<- fxns$dList[-length(fxns$dList)]
    n_list <<- n_list[-length(n_list)]
    if (index != 1) {
      index <<- index - 1
      empty <<- FALSE
    } else { 
      index <<- 1
      empty <<- TRUE
    }
    output$equations_text <- renderPrint({
      fxns$dList
    })
    
    fxns_num$dList <<- n_list
    list_fxns <<- fxns$dList
  }
)

observeEvent(
  input$reset.simplex, {
    index <<- 1
    n_list <<- NULL
    empty <<- TRUE
    enteredZ <<- FALSE
    list_fxns <<- list()
    output$solution.simplex <<- renderPrint({"[Enter equations]"})
    output$equations_text <<- renderPrint({"Reset"})
    output$equationZ_text <<- renderPrint({"Reset"})
    initial_list <<- 0
    each_term <<- ""
    tableau <<- 0
    z_each_term <<- list()
    z_num_terms <<- list()
    z_terms <<- list()
    num_terms <<- 0
    numeric_terms <<- 0
    temp <<- 0
    z <<- ""
    z_coefficient <<- 0
  }
)

 #adding Z
observeEvent(
  input$addZ, {
    enteredZ <<- TRUE
    z <<- input$equationZ
    output$equationZ_text <- renderPrint(z)
    
    z_terms <<- strsplit(z, "=")
    #print(z_terms[[1]][2])
    
    #split each term [+]
    z_each_term <<- strsplit(z_terms[[1]][2], "\\+")
    print(z_each_term)
    for (i in 1:length(z_each_term[[1]])) {
      z_num_terms[i] <<- strsplit(z_each_term[[1]][i], "x")
      for (j in 1:length(z_num_terms)) {
        z_coefficient[j] <<- eval(parse(text=z_num_terms[[j]][1]))
      }
    }
    z_coefficient[(length(z_coefficient))+1] <<- 1
  } 
)
#--------------------------------
  
#computation---------------------
#if min, simplex(tableau, FALSE, FALSE)
#set functions from list to vectors
#pass to set_up(fxns, FALSE)
#pass to simplex
observeEvent(
  input$min, {
    if (isFALSE(empty) && enteredZ){
      initial_list <<- n_list
      initial_list[[(length(initial_list))+1]] <<- z_coefficient
      
      tableau <<- set_up(initial_list, FALSE)
      output$solution.simplex <<- renderPrint({simplex(tableau, FALSE, FALSE)})
    } else {
      output$solution.simplex <<- renderPrint({"Enter Equations"})
    }
  }
)


#if max, simplex(tableau, TRUE, FALSE)
observeEvent(
  input$max, {
    if (isFALSE(empty) && enteredZ) {
      for (i in 1:length(n_list)){
        n_list[[i]] <<- n_list[[i]]*(-1)
      }
      initial_list <<- n_list
      temp <<- (z_coefficient[1:(length(z_coefficient)-1)])*(-1)
      temp[(length(temp)+1)] <<- z_coefficient[length(z_coefficient)]
      initial_list[[(length(initial_list))+1]] <<- temp
      print(initial_list)
      tableau <<- set_up(initial_list, TRUE)
      print(tableau)
      output$solution.simplex <<- renderPrint({simplex(tableau, TRUE, FALSE)})
    } else {
      output$solution.simplex <<- renderPrint({"Enter Equations"})
    }
  }
)
#--------------------------------

}

shinyApp(ui = ui, server = server)