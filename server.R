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
    observe({
      filename <- input$datafile$name
      names(filename)<- input$datafile$datapath
      filepath <- input$inCheckboxGroup
      namelist <- filename[filepath]
      #print(filename)
      #print(filepath)
      #print(namelist)
      updateSelectInput(session, "select_0", 
                        "Select File",
                        choices=filename[filepath]
                        )
      
      
    })
    
    output$columns_selection<-renderUI({
            path_list <- input$inCheckboxGroup
            len = length(path_list)
            x <- list()
            i = 0
            for (path in path_list){
                i = i+1
                df = read.csv(path)
                column_names = colnames(df)
                x[[i]]=c(i, column_names)
            }
            
            
            #print(x)
            
            radio_var<- lapply(x, function(columns_n){
                        len = length(columns_n)
                        radioButtons(paste0("sel_col_dyn_id_",columns_n[[1]]), "Choose Features",
                                           choices  = columns_n[2:len],
                                           selected = character(0))
            })
    })
    
    
      output$coldata_0 <- renderText({
             toString(coldata()
      })    
    
      coldata <- reactive({
      
      filepath <- input$inCheckboxGroup
      length_0 <- length(filepath)
      #filename <- lapply(filepath, function(filepath, length){strsplit(filepath, split = "/")[[1]][3]}, length = length_0)
      #filename <- lapply(filename, function(filename){strsplit(filename, split)})
      if(!is.null(filepath)){
          count <- length_0
          len <- c(1:count)
      }
      else{
          len <- NULL
      }
      #print(paste("length_0",length_0))
      #print(paste("len",len))
      
      radio_id <- list()
      f <- function(i){
        return (paste0("sel_col_dyn_id_", i))
      }
      radio_id <- lapply(len, f)
      
      cols<- lapply(radio_id, function(id){
        if(!is.null(id)){
          return(eval(parse(text=paste0("input$",id))))
        }
        else{
          return(NULL)
        }
      })
      
      head<-lapply(len,function(i){
        #print(paste("This is :",cols[[i]]))
        head <- head(read.csv(filepath[[i]])[cols[[i]]])
        #print(head)
        
        #fluidRow(column(12,div("Head of Data:", tableOutput("head"))))
        })
      #o.call(rbind, lapply(input$inCheckboxGroup, read.csv))
      return(head)
      #print(cols)
      #print(filename)
      #print(zip(filepath,cols))
      
      
    })
    
    
    # filedata <- reactive({
    #      infile <- input$datafile$datapath
    #      if (is.null(infile)) {
    #          # User has not uploaded a file yet
    #          return(NULL)
    #      }
    #      if (is.null(input$inCheckboxGroup)) {
    #        # User has not uploaded a file yet
    #        return(NULL)
    #      }
    #      else{
    #           if(length(input$inCheckboxGroup)==1){
    #                   read.csv(input$inCheckboxGroup,sep = input$sep, header = input$header, fill=T)
    #           }
    #           else{
    #             do.call(rbind, lapply(input$inCheckboxGroup, read.csv))
    #           }
    #      }
    #   })
    # 
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

