rm(list = ls())

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyBS)
library(tidyverse)
library(DT)
library(magrittr)
library(shinyWidgets)
library(lubridate)
library(plotly)

source("modal_dialog.R", encoding="utf-8")

nps_age = data.frame(
  'year' = c(0, 1952, 1953, 1957, 1961, 1965, 1969),
  'amt' = c(60, 60, 61, 62, 63, 64, 65)
)

create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                     ))
}

x <- create_btns(1:2)

plan <- data.frame(
  type = c('입금', '출금'),
  age = c(55, 60),
  amt = c(10000, 10000)
) %>% dplyr::bind_cols(tibble("Buttons" = x))


ui <- navbarPage(
  
  title = '연금 계산기',
  theme = shinytheme("united"),
  
  tabPanel(title = '계산하기',
           
           sidebarPanel(
             width = 3,
             div(
               class = "text-center",
               fluidRow(
                 uiOutput('tp'),
                 column(4,style=list("padding-right: 5px;"), uiOutput('birth')),
                 column(4,style=list("padding-left: 5px; padding-right: 5px;"), uiOutput('retire')),
                 column(4,style=list("padding-left: 5px;"), uiOutput('death'))
               ),
               fluidRow(
                 column(6,style=list("padding-right: 5px;"), uiOutput('wealth')),
                 column(6, conditionalPanel(
                   condition = "input.tp == 'a'", uiOutput('invest')
                   ),
                 conditionalPanel(
                   condition = "input.tp == 'b'", uiOutput('t_ret')
                 )
                 ),
               ),
               br(),
               fluidRow(
                 column(6,style=list("padding-right: 5px;"), uiOutput('pension')),
                 column(6,style=list("padding-left: 5px;"), uiOutput('nps_amt'))
               ),
               uiOutput('nps_dc'),
               uiOutput('nps_diff'),
               br(),
               uiOutput('going'),
               fluidRow(
                 column(6,style=list("padding-right: 5px;"), uiOutput('inf')),
                 column(6,style=list("padding-left: 5px;"), uiOutput('inc'))
               ),
               actionButton("showTable", "큰돈 입/출금 계획 수기로 입력하기",
                            icon = icon("table"), width = '100%'),
               hr(),
               actionButton("cal", "계산하기!",
                            icon = icon("calculator"), width = '100%',
                            class = "btn-info")
               
             )
           ),
           
           mainPanel(
             
             bsModal("modalExample", "입/출금 계획", "showTable", size = "large",
                     
                     print('특정 연령때 큰 금액의 입/출금이 있을 시 입력'),
                     br(),
                     print('예: 55세에 퇴직금 2억 입금, 60세에 자녀 결혼자금 1억 지출 등'),
                     
                     div(
                       class = "container",
                       div(
                         style = "margin-top: 20px;",
                         shiny::actionButton(
                           inputId = "add_data",
                           label = "계획 추가하기",
                           icon = shiny::icon("plus"),
                           class = "btn-info"
                         )
                       )
                     ),
                     
                     div(
                       class = "container",
                       style = "margin-top: 50px;",
                       DT::DTOutput(outputId = "dt_table", width = "73%")
                     ),
                     
                     shiny::includeScript("script.js")
             ),
             
             bsModal("npsamtdiff", "국민연금 수령시기 및 조기/연기 수령액 차이",
                     "nps_diff", size = "medium",
                     print('각 출생년도 별 국민연금 수령나이'),
                     dataTableOutput('nps_start'),
                     hr(),
                     print('조기수령 및 연기수령 별 차이'),
                     dataTableOutput('nps_early')
             ),
             
             tabsetPanel(type = "tabs",
                         tabPanel('자금계획 계산하기',
                                  br(),
                                  textOutput('msg'),
                                  plotlyOutput('graph', height = '700px') %>% withSpinner(color = '#ff6211')
                         ),
                         tabPanel('테이블',
                                  br(),
                                  dataTableOutput('tbl')),
                         tabPanel('입력값 확인하기',
                                  br(),
                                  includeMarkdown('input.Rmd')
                                  )
             )
           )
  ),
  
  tabPanel(title = '국민연금 수령액 확인하기',
           mainPanel(
             includeMarkdown('pension.Rmd')
           )
  )
)




server <- function(input, output, session) {
  
  source('output_ui.R', encoding="utf-8", local = TRUE)
  source('cal.R', encoding="utf-8", local = TRUE)
  
  rv <- shiny::reactiveValues(
    df = plan,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(plan) + 1
  )
  
  output$dt_table <- DT::renderDT(
    {
      shiny::isolate(rv$df
                     %>% set_colnames(c('입/출금 구분', '나이', '금액', '수정/삭제하기')))
    },
    escape = F,
    rownames = FALSE,
    selection = 'none',
    options = list(processing = FALSE,
                   dom = 't',
                   pageLength = 1000000,
                   lengthChange = FALSE,
                   rowCallback = JS(
                     "function(row, data) {",
                     "var num = data[2].toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
                     "$('td:eq(2)', row).html(num);",
                     "}"),
                   columnDefs = list(list(className = 'dt-right', targets = c(0:3))))
  )
  
  proxy <- DT::dataTableProxy("dt_table") 
  shiny::observe({
    DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE) 
  })
  
  ### delete row
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete"))
    rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    rv$df <- rv$df[-rv$dt_row, ]
  })
  
  # when edit button is clicked, modal dialog shows current editable row filled out
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
    rv$dt_row <- which(stringr::str_detect(rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    df <- rv$df[rv$dt_row, ]
    modal_dialog(
      type = plan$type,
      selected_type = df$type,
      age = df$age,
      amt = df$amt,
      edit = TRUE
    )
    rv$add_or_edit <- NULL
  })
  
  # when final edit button is clicked, table will be changed
  shiny::observeEvent(input$final_edit, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit") & is.null(rv$add_or_edit))
    
    rv$edited_row <- dplyr::tibble(
      type = input$type,
      age = input$age,
      amt = input$amt,
      Buttons = rv$df$Buttons[rv$dt_row]
    )
    
    rv$df[rv$dt_row, ] <- rv$edited_row
  })
  
  shiny::observeEvent(input$add_data, {
    modal_dialog(
      type = plan$type,
      age = "",
      amt = '',
      selected_type = NULL,
      edit = FALSE
    )
    rv$add_or_edit <- 1
  })
  
  shiny::observeEvent(input$final_edit, {
    shiny::req(rv$add_or_edit == 1)
    add_row <- dplyr::tibble(
      type = input$type,
      age = input$age,
      amt = input$amt,
      Buttons = create_btns(rv$keep_track_id)
    )
    rv$df <- add_row %>%
      dplyr::bind_rows(rv$df)
    rv$keep_track_id <- rv$keep_track_id + 1
  })
  
  
  ### remove edit modal when close button is clicked or submit button
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  shiny::observeEvent(input$final_edit, {
    shiny::removeModal()
  })
  
  ### Other Input
  
  opt_ret = eventReactive(input$cal, {
    opt_R()
  })
  
  opt_df = eventReactive(input$cal, {
    cal_df(opt_ret())
  })
  
  
  msg_fun = function() {
    
    df = opt_df()
    req_money = df[which(df$age == input$retire)[1], 'wealth']
    
    if (input$tp == 'a') {
      paste0('원하는 노후를 위한 은퇴시 필요금액은 약 ', round(req_money/100000000, 2), '억원이며,
    이를 달성하기 위한 요구 수익률은 약 ', round(opt_ret(), 4) * 100, '% 입니다.')
      
    } else {
      paste0('원하는 노후를 위한 은퇴시 필요금액은 약 ', round(req_money/100000000, 2), '억원이며,
    이를 달성하기 위한 월간 투자금액은 약 ', format(round(opt_ret(), 0), big.mark=",", scientific=FALSE),
             '원 입니다.')
    }
    
  }
  output$msg = eventReactive(input$cal, {
    msg_fun()
  })
  
  output$graph = renderPlotly({
    
    df = opt_df()
    
    df %>%
      distinct(age, .keep_all = TRUE) %>%
      plot_ly(x = ~age, y = ~wealth, type = 'bar',
              marker = list(color = '#ff6211')) %>%
      layout(
        xaxis = list(title = '나이'),
        yaxis = list(title = '자산 (원)', tickformat = ',.0f')
      )
  })
  
  output$tbl = renderDT({
    
    path = getwd()
    df = opt_df()
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    
    
    dtable =
      df %>% select(age, date, wealth, inv, ret, 'in', out, nps, withdraw, eop) %>%
      mutate(date = stringr::str_sub(date, 1, 7)) %>%
      set_colnames(c('나이', '날짜', '월초 잔액', '투자금', '수익',
                     '목돈 유입', '목돈 유출', '국민연금 수령액', '연금 인출액', '월말 잔액')) %>%
      datatable(rownames = FALSE,
                selection = 'none',
                extensions = c('Buttons'),
                options = list(dom = 'tB',
                               pageLength = 10000000,
                               scrollY = "700px",
                               buttons = c('copy', 'csv', 'excel'),
                               rowsGroup = list(0)
                )
      ) %>%
      formatCurrency(c(3:10), currency = '',
                     interval = 3, mark = ',', before = FALSE, digit = 0) 
    
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    
    return(dtable)
    
  })
  
  
  output$nps_start = renderDataTable({
    
    nps_age %>%
      filter(year > 0) %>%
      mutate(year = ifelse(year == 1952, paste0(year, '년 이전'), paste0(year, '년 이후')),
             amt = paste0(amt, '세') ) %>%
      set_colnames(c('출생년도', '수령나이')) %>%
      datatable(rownames = FALSE,
                selection = 'none',
                options = list(processing = FALSE,
                               dom = 't',
                               pageLength = 1000000,
                               columnDefs = list(list(className = 'dt-center', targets = c(0:1))))) %>%
      formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
    
  })
  
  output$nps_early = renderDataTable({
    
    data.frame('수령시기' = c('5년 조기수령', '4년 조기수령', '3년 조기수령',
                          '2년 조기수령', '1년 조기수령', '정상수령',
                          '1년 연기수령', '2년 연기수령', '3년 연기수령',
                          '4년 연기수령', '5년 연기수령'),
               '지급률' = c('70.0%', '76.0%', '82.0%', '88.0%', '94.0%', '100.0%',
                         '107.2%', '114.4%', '121.6%', '128.8%', '136.0%')) %>%
      datatable(rownames = FALSE,
                selection = 'none',
                options = list(processing = FALSE,
                               dom = 't',
                               pageLength = 1000000,
                               columnDefs = list(list(className = 'dt-center', targets = c(0:1))))) %>%
      formatStyle("수령시기", target = 'row', 
                  backgroundColor = styleEqual(c('정상수령', 0), c('orange', 'white'))) %>%
      formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
    
  })
  
}

shinyApp(ui = ui, server = server)

# rsconnect::deployApp()