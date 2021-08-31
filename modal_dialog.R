modal_dialog <- function(type, selected_type, age, amt, edit) {
  
  if(edit) {
    x <- "계획 수정하기"
  } else {
    x <- "계획 추가하기"
  }
  
  shiny::modalDialog(
    title = "입/출금 계획표",
    div(
      class = "text-center",
      
      div(
        style = "display: inline-block;",
        shiny::selectInput(inputId = "type",
                           label = "입/출금 구분", 
                           width = "200px",
                           selected = selected_type,
                           choices = c('입금', '출금'))
      ),
      
      br(),
      
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "age",
                            label = "나이",
                            value = age, 
                            min = 0,
                            width = "200px")
      ),
      
      br(),
      
      div(
        style = "display: inline-block;",
        shinyWidgets::autonumericInput(inputId = "amt",
                                       label = "금액",
                                       value = amt, 
                                       min = 0,
                                       currencySymbolPlacement = "p",
                                       decimalPlaces = 0,
                                       digitGroupSeparator = ",",
                                       decimalCharacter = ".",
                                       align = 'left',
                                       width = "200px")
      )
      
      
    ),
    size = 'm',
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(inputId = "final_edit",
                          label   = x,
                          icon = shiny::icon("edit"),
                          class = "btn-info"),
      shiny::actionButton(inputId = "dismiss_modal",
                          label   = "Close",
                          class   = "btn-danger")
    )
    
    
  ) %>% shiny::showModal()
  
}