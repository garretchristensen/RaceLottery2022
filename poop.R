library(shiny)
shinyApp(
  ui = fluidPage(

    
    fluidRow(
      column(6, sliderInput("exp", label = h5("Change this"), min=2, max=5, value = 2)),
      
      column(12,
             tableOutput('table')
      )
    )
  ),
  server = function(input, output) {
    
    result <-eventReactive(input$exp, {  
    foo<-data.frame(matrix(ncol=8, nrow=1))
   
    colnames(foo)<-c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    foo$a<-input$exp+2
    foo$b<-2
    foo$c<-3
    return(foo)
    })
    output$table <- renderTable({result()})
  }
)



