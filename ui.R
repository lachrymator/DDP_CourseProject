library(shiny)
library(kernlab)
library(caret)
library(randomForest)
library(ggplot2)
library(plyr)
library(knitr)
library(datasets)


data <- mtcars #Make a copy of the original data
factorme <- function(x) as.factor(x) #Create a function to factor columns
data[,c(2,8,9,10,11)]<-colwise(factorme)(data[,c(2,8,9,10,11)]) #Make factors
data$am<- factor(data$am, levels = c(0,1), labels = c("Automatic","Manual"))

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
     
     headerPanel("Machine Learning Miles Per Gallon Estimator"),
     
     sidebarPanel(
          h3("MPG Estimator"),
          p("This interface will estimate the mpg of a new car using the input parameters and the mtcars dataset"),
          p("Select the stat of interest to plot (for comparison)."),
          p("The algorithm will perform a prediction using ALL the data availble. To perofrm a prediction, select the stats of the new car."),
          p("The red dot represents where your new car would find itself in the data displayed"),
          p(""),
          selectInput("variable", "Horizontal Axis to Display", 
                      list("Cylinders" = "cyl", 
                           "Displacement (cu.in)"="disp",
                           "Gross Horsepower" = "hp",
                           "Rear Axle Ratio" = "drat",
                           "Weight (lb/1000)" = "wt",
                           "V engine or Standard" = "vs",
                           "Transmission" = "am",                       
                           "Gears" = "gear",
                           "Carburators" = "carb")),
          
          radioButtons("alg", "Select an algorithm",
                       list("Random Forest" = "rf",
                            "Support Vector Machines" = "svmLinear",
                            "Generalized Linear Model" = "glm")),

            h3("Input New Vehicle Stats"),
          
            textInput("name", "Eneter your new vehicle's name", value = "My Shiny New Car"),
          
            sliderInput('disp','Displacement (cu.in.)',
                        min=min(data$disp), max=max(data$disp), value=mean(data$disp)),
            sliderInput('hp','Gross Horsepower',
                        min=min(data$hp), max=max(data$hp), value=mean(data$hp)),
            sliderInput('drat','Rear Axle Ratio',
                        min=min(data$drat), max=max(data$drat), value=mean(data$drat)),
            sliderInput('wt','Weight (1000 lb)',
                        min=min(data$wt), max=max(data$wt), value=mean(data$wt)),
            sliderInput('qsec','Quarter mile time (seconds)',
                        min=min(data$qsec), max=max(data$qsec), value=mean(data$qsec)),
            radioButtons('carb', 'Carburetors',
                              list("1"="1","2"="2","3"="3","4"="4","6"="6","8"="8")),          
            radioButtons('am', 'Transmission type',
                         list("automatic"="0", "manual"="1")),
            radioButtons('gear', 'Number of forward gears',
                         list("3"="3", "4"="4", "5"="5")),
            radioButtons('vs', 'V engine or Straight Engine',
                         list("V"="0", "Straight"="1")),
            radioButtons("cyl", 'Cylinders',
                         list("4"="4", "6"="6","8"="8"))
   
     ),
     
     # Show the caption and plot of the requested variable against mpg
     mainPanel(
          h3(textOutput("caption")),
          
          plotOutput("mpgPlot"),
          
          h3(paste("MPG Estimate (miles per gallon)")),
          h5(textOutput("name")),
          h5(textOutput("mpgEstimate")),
          p(""),
          h5("Calculation run:"),
          verbatimTextOutput("model"),
          h5("New car data frame:"),
          tableOutput("df")
          
          
     )
))
