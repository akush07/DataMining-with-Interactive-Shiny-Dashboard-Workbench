library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Interactive Dashboard", 
                    dropdownMenuOutput("msgOutput"),
                    dropdownMenuOutput("msgNotifications"),
                    dropdownMenuOutput("msgTask")),
    dashboardSidebar(
                    sidebarMenu(
                      sidebarSearchForm("searchText", "buttonSearch","search"),
                      menuItem("Raw Data", tabName = "raw_in"),
                      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),badgeLabel = "New", badgeColor = "green"),
                        menuSubItem("Information", tabName = "Information", icon = icon("wallet")),
                        menuSubItem("Plots", tabName = "Plots", icon = icon("piggy-bank"))
                    )),
    dashboardBody(
                  tabItems(
                          tabItem(tabName = "dashboard",
                                  fluidRow(
                                    infoBox("Sales", 1000, icon=icon("thumbs-up")),
                                    infoBoxOutput("dfshape"),
                                    infoBoxOutput("approvedSales")
                                  ),
                                  fluidRow(
                                    tabBox(
                                    tabPanel(title = "Histogram of Price", status = "primary", solidHeader = T, 
                                             plotOutput("histogram"),
                                             sliderInput("bins","", 1, 100, 50)),
                                    tabPanel(title = "Control for Dashboard", status = "warning", solidHeader = T, background = "black",
                                        "Use the control to fine tune your dashboard",br(),br(),
                                        "Donot use alot of controls as it confuses the user",br(),br(),
                                        sliderInput("bins","Number of Breaks", 1, 100, 50),
                                        textInput("text_input","search opportunities",value = "123456"))
                                    )
                                  )),
                          tabItem(tabName = "Information",
                                  h1("Information Dashboard")
                                  ),
                          tabItem(tabName = "Plots",
                                  h2("Graphs Dashboard")),
                          tabItem(tabName = "raw_in",
                                  fluidPage(
                                    headerPanel(title = "Upload File"),
                                    mainPanel(
                                      fileInput("datafile","Upload the file", accept=c('text/csv',
                                                                                       'text/comma-seperated-values',
                                                                                       'text/plain',
                                                                                       '.csv',
                                                                                       '.pdf'),
                                                multiple = T),
                                      h5("Max file size to upload is 3MB"),
                                      checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                                                         c("None")),
                                      radioButtons("sep","Seperator", choices = c(Comma = ',',Period=".",Tilde = "~", minus = "-")),
                                      checkboxInput("header","Header"),
                                    fluidRow(column(12,div("Head of Data:", tableOutput("head")))),
                                    fluidRow(column(12,div("Tail of Data:", tableOutput("tail")))),
                                    fluidRow(column(12,div("Summary of Data:", verbatimTextOutput("summary"))))
                                  )))
      )

    )
  )
)



