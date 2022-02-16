library(ggplot2)
library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      plotOutput("graph", width = "100%", click = "plot_click"),
      verbatimTextOutput("click_info")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  data <- data.frame(x=c(1,2,1,2),
                     y=c(1,1,2,2),
                     values=c("A","B","C","D"),
                     stringsAsFactors=FALSE,
                     color=rep("1",4))
  makeReactiveBinding('data')

  output$graph <- renderPlot({
    ggplot(data=data,aes(x=x,y=y,label=values,color=color))+geom_text()+theme_classic()+guides(colour=FALSE)
  })

  observeEvent(input$plot_click, {
    # Get 1 datapoint within 15 pixels of click, see ?nearPoints
    np <- nearPoints(data, input$plot_click, maxpoints=1 , threshold = 15)

    output$click_info <- renderPrint({np$values})

    data$color <<- rep("1",length(data$x))
    data$color[data$values==np$values] <<- "2"
  })
})
shinyApp(ui=ui,server=server)
