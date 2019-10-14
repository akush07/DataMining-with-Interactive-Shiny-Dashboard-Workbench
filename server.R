library(shiny)
library(shinydashboard)

options(shiny.maxRequestSize=3*1024^2)

shinyServer(
  function(input, output, session){
    
    observe({
      x <- input$datafile$name
      y <- input$datafile$datapath

      # Can use character(0) to remove all choices
      if (is.null(x) && is.null(y))
        x <- character(0)

      # Can also set the label and select items
      if (length(x)==0)
        updateCheckboxGroupInput(session, "inCheckboxGroup",
                                 label = paste("Files Uploaded: ", length(x)),
                                 choices = x)
      else
        updateCheckboxGroupInput(session, "inCheckboxGroup",
                                 label = paste("Files Uploaded: ", length(x)),
                                 choiceNames = x,
                                 choiceValues = y)
      
      
    })
    
    filedata <- reactive({
         infile <- input$datafile$datapath
         if (is.null(infile)) {
             # User has not uploaded a file yet
             return(NULL)
         }
         print(input$inCheckboxGroup)
         if (is.null(input$inCheckboxGroup)) {
           # User has not uploaded a file yet
           return(NULL)
         }
         else{
              if(length(input$inCheckboxGroup)==1){
                      read.csv(input$inCheckboxGroup,sep = input$sep, header = input$header, fill=T)
              }
              else{
                do.call(rbind, lapply(input$inCheckboxGroup, read.csv))
              }
         }
      })
    
    output$head <- renderTable({
           head(filedata())
         })
    
    output$tail <- renderTable({
      tail(filedata())
    })
    
    output$summary <- renderPrint({
      summary(filedata())
    })
    
    output$histogram <- renderPlot({
      df = filedata()
      hist(df$price, xlab = "Price Range", ylab = "frequency" , breaks = input$bins)
    })

    ########################################################################
    #Notification
    ########################################################################
    output$msgOutput <- renderMenu({
      msgs <- apply(read.csv("Dynamic Messages//message.csv",header = TRUE, sep=","), 1, function(row){
        messageItem(from = row[["from"]], message = row[["message"]])
      })
      dropdownMenu(type = "messages", .list = msgs)
    })

    output$msgNotifications <- renderMenu({
      msgs <- apply(read.csv("Dynamic Messages//notification.csv",header = TRUE, sep=","), 1, function(row){
          notificationItem(text = row[["text"]],icon = icon("warning"), status = row[["status"]])
      })
      dropdownMenu(type = "notifications", .list = msgs, icon = icon("anchor"))
    })

    output$msgTask <- renderMenu({
      msgs <- apply(read.csv("Dynamic Messages//task.csv",header = TRUE, sep=","), 1, function(row){
        taskItem(text = row[["task"]],value = row[["value"]], color = row[["color"]])
      })
      dropdownMenu(type = "tasks", .list = msgs,icon = icon("tasks"))
    })
    ########################################################################
    #infobox
    ########################################################################
    output$approvedSales <- renderInfoBox({
      df = filedata()
      infoBox("Total ",mean(df$price),icon=icon("bar-chart-o"))
    })
    
    output$dfshape <- renderInfoBox({
      infoBox("Dimension ",dim(filedata()),icon=icon("bar-chart-o"))
    })
    
    
})

