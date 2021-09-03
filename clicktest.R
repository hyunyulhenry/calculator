ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      actionButton("clickBtn", "Click")
      
    ),
    
    
    mainPanel(
      # conditionalPanel(
      #   condition  = 'output.clickCount == "0"',
      #   print(1)
      # ),
      # 
      # conditionalPanel(
      #   condition  = 'output.clickCount != "0"',
      #   print(2)
      # )
        
      # print(counter$countervalue),
      textOutput("count"),
      
      conditionalPanel(
        condition  = 'input.clickBtn == "0"',
        print('시작을 눌러주세요')
      )
      
      
    )
  )
)

counter = 0

server = function(input, output, session) {
  
  counter <- reactiveValues(countervalue = 0) 
  
  observeEvent(input$clickBtn, {
    counter$countervalue <- counter$countervalue + 1     # if the add button is clicked, increment the value by 1 and update it
  })
  
  output$count <- renderText({
    counter$countervalue   # print the latest value stored in the reactiveValues object
  })
  
}

shinyApp(ui = ui, server = server)
