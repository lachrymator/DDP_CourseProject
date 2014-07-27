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
numberme <- function(x) as.numeric(x)
#data[,c(2,8,9,10,11)]<-colwise(factorme)(data[,c(2,8,9,10,11)]) #Make factors
#data$am<- factor(data$am, levels = c(0,1), labels = c("Automatic","Manual"))

shinyServer(function(input, output) {
     
   
     formulaText <- reactive({
          paste("mpg ~", input$variable)
     })
     
     
     mpgEstimate <- reactive({
          set.seed(1234)
          model<-train(mpg~., data=mtcars, method=input$alg)
          newdata<- data.frame(
               cyl=as.numeric(input$cyl), 
               disp=as.numeric(input$disp),
               hp=as.numeric(input$hp),
               drat=as.numeric(input$drat),
               wt=as.numeric(input$wt),
               qsec=as.numeric(input$qsec), 
               vs=as.numeric(input$vs), 
               am=as.numeric(input$am), 
               gear=as.numeric(input$gear),
               carb=as.numeric(input$carb))
          #newdata<-colwise(numberme)(newdata)          
          output$name <- renderText({input$name})
          print(newdata)
          c(round(predict(model, newdata),1), newdata[,colnames(newdata)==input$variable])
             
     })
     
     xVar <- reactive({
          newdata<- data.frame(
               cyl=input$cyl, 
               disp=input$disp,
               hp=input$hp,
               drat=input$drat,
               wt=input$wt,
               qsec=input$qsec, 
               vs=input$vs, 
               am=input$am, 
               gear=input$gear,
               carb=input$carb)
               newdata<-colwise(numberme)(newdata)
               cat(names(input))
               print(input$variable)
     })
     
     
     # Return the formula text for printing as a caption
     output$caption <- renderText({
          formulaText()
     })
     
     # Generate a plot of the requested variable against mpg and only 
  
     output$mpgPlot <- renderPlot({
          plot(as.formula(formulaText()), 
                  data = data)
          points(mpgEstimate()[2],mpgEstimate()[1], pch = 16, cex = 4, col="red")
          })
          
     output$mpgEstimate <- renderText({mpgEstimate()[1]})
})