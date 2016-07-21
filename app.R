library(shinydashboard)
library(leaflet)
library(shiny)
library(DT)
source("globalr.r")

header <- dashboardHeader(
  title = "DELHI TRAFFIC"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mymap", height = 500)
           ),      tabsetPanel(
             #tabPanel("Plot", plotOutput("plot")), 
             #tabPanel("Summary", verbatimTextOutput("summary")), 
             ##tabPanel("table", tableOutput("table"))
             tabPanel("table", DT::dataTableOutput('x1'))
           ), box(verbatimTextOutput('x4'))
    ),
    actionButton("update", "Update"), 
    
   # selectInput("numTweets", "Number of Tweets:",
    #            choices = numChoices),
    
    column(width = 3,
           box(verbatimTextOutput('x5')
           ))
    
  )
)
ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

library(shiny)

server<-function(input, output, session) {
  
  statuses <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Gathering tweets...")
       # getTweets(input$numTweets)
        getTweets()
      })
    })
  })
  
  taggeddata <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    getTaggedData(statuses())
  })
  
  checklist <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Generating...checklist")
     # tochecklist <- 
        givestochecklist(taggeddata())
    })
    #})
  })
  
  toddfflist <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Generating...todfflist")
      #totoddffklist <- 
        givesddfflist(checklist())
    })
    #})
  })
  
  togivesddff.list <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Gathering.....locations list")
     # givesddff.list <- 
        givesddff.list(toddfflist())
    })
    #})
  })
  
  togivesoutput.list <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Printing......output")
     # output.list <-
        givesoutputss(togivesddff.list())
    })
    #})
  })
  
  ##############Make the wordcloud drawing predictable during a session
  ### wordcloud_rep <- repeatable(wordcloud)
  
  output$x1 <- DT::renderDataTable(
    statuses()[,c("text","screenName","created","retweetCount","latitude")],server= FALSE, selection ="single")
  
  output$mymap <- renderLeaflet({
    ## n <- leaflet()
    # n <- addTiles(n)
    #for(i in 1:length(ddff.list)){
    # n <- addMarkers(n, lng=ddff.list[[i]][[2]][,1], lat=ddff.list[[i]][[2]][,2], popup="The birthplace of R")
    #}
    #n
    
    
    
    s = input$x1_rows_selected
    if(length(s)){
      
      togivesoutput.list()[[s]]}
  })
  
  output$x4 = renderPrint({
    s = input$x1_rows_selected
    if (length(s)) {
      cat('This tweet is selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  output$x5 = renderPrint({
    
    s = input$x1_rows_selected
    if (length(s)) {
      cat('The tweet selected has locations:\n')
      if(nrow(toddfflist()[[s]])==0){
        cat("No Location Found In The Selected Tweet")
      }else{
      cat(paste(toddfflist()[[s]]$frm.database,collapse = "\n"))}
    }
  })




}
shinyApp(ui,server)