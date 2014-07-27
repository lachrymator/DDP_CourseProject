library(shiny)
library(kernlab)
library(randomForest)
library(e1071)
library(ggplot2)
library(plyr)
library(knitr)
library(datasets)

data <- mtcars #Make a copy of the original data
factorme <- function(x) as.factor(x) #Create a function to factor columns
numberme <- function(x) as.numeric(x)

shinyServer(function(input, output) {
     
   
     formulaText <- reactive({
          paste("mpg ~", input$variable)
     })
     
     mpgEstimate <- reactive({
          set.seed(1234)
          if(input$alg == "rf"){
               model<-randomForest(mpg~., data=mtcars, mtry=6)
          }
          else if(input$alg == "svm") {
               model<-svm(mpg~., data=mtcars, gamma=0.1) }
          else {
               model<-glm(mpg~., data=mtcars)
               
          }
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
                  
          output$name <- renderText({input$name})
          dataframe<<-newdata
          c(round(predict(model, newdata),1), 
                 newdata[,colnames(newdata)==input$variable])
     })
     
     newDataFrame <- reactive({
             data.frame(
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
     })
     
     modelStatement <- reactive({
          if(input$alg == "rf"){
               print("model<-randomForest(mpg~., data=mtcars, mtry=6)")
          }
          else if(input$alg == "svm") {
               print("model<-svm(mpg~., data=mtcars, gamma=0.1)") }
          else {
               print("model<-glm(mpg~., data=mtcars)")  }   
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
          
     output$mpgEstimate <- renderText({mpgEstimate()[[1]]})
     output$model <- renderText(modelStatement())
     output$df <- renderTable(newDataFrame())
})