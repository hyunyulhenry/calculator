output$tp = renderUI({

  radioGroupButtons(
  inputId = "tp",
  justified = TRUE,
  label = "원하는 계산을 선택하세요",
  choices = list('목표 수익률 계산' = 'a', '목표 투자금액 계산' = 'b'),
  checkIcon = list(
    yes = icon("ok", 
               lib = "glyphicon"))
  )
})



output$birth = renderUI({
  dateInput('birth',
            label = '생년월일',
            value = '1987-08-16',
            format = "yyyy-mm-dd"
            
  )
})

output$retire = renderUI({
  
  autonumericInput(inputId = "retire",
                   label = "은퇴연령",
                   value = 55, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center',
                   width = '100%')
})

output$death= renderUI({
  
  autonumericInput(inputId = "death",
                   label = "예상수명",
                   value = 90, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center',
                   width = '100%')
})

output$wealth= renderUI({
  
  autonumericInput(inputId = "wealth",
                   label = "현재 금융자산 (원)",
                   value = 100000000, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center')
})

output$invest= renderUI({
  
  autonumericInput(inputId = "invest",
                   label = "월간 추가 투자금액 (원)",
                   value = 400000, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center')
})

output$t_ret= renderUI({
  
  autonumericInput(inputId = "t_ret",
                   label = "예상 연간 수익률 (%)",
                   value = 5, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center')
})


output$pension= renderUI({
  
  autonumericInput(inputId = "pension",
                   label = HTML("은퇴 후 예상 월간 지출<br>(원/현재기준)"),
                   value = 4000000, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center')
})

output$nps_amt= renderUI({
  
  autonumericInput(inputId = "nps_amt",
                   label = HTML("예상 국민연금 수령액<br>(원/사이트에서 확인)"),
                   value = 2000000, 
                   min = 0,
                   currencySymbolPlacement = "p",
                   decimalPlaces = 0,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center')
})

nps_age = data.frame(
  'year' = c(0, 1952, 1953, 1957, 1961, 1965, 1969),
  'amt' = c(60, 60, 61, 62, 63, 64, 65)
)

output$nps_dc= renderUI({
  
  req(input$birth)
  
  x = lubridate::year(input$birth)
  cnt = nps_age[last(which(x >= nps_age$year)), 'amt']
  
  sliderInput("nps_dc",
              label = '국민연금 수령나이',
              min = cnt-5,
              max = cnt+5,
              step = 1,
              value = cnt)
  
})


output$nps_diff = renderUI({
  actionButton("nps_diff", "국민연금 수령시기 및 조기/연기 수령액 차이", width = '100%', icon = icon("exclamation-circle"))
})

output$going = renderUI({
  pickerInput(
    inputId = "going",
    label = HTML('은퇴 후에는 어떻게 투자하시겠습니까?'),
    select = 'No',
    choices = list('계속 적극적으로 투자' = 'Yes',
                   '원금 보장형에 투자' = 'No'),
    options = list(style = "btn-warning"),
    choicesOpt = list(
      content = c("<div style='text-align: center;'>계속 적극적으로 투자</div>",
                  "<div style='text-align: center;'>원금 보장형에 투자</div>"),
      style = c('text-align: center;', 'text-align: center;')
    )
  )
})

output$inf = renderUI({
  
  autonumericInput(inputId = "inf",
                   label = HTML("예상 인플레이션 (%)"),
                   value = 1.1, 
                   currencySymbolPlacement = "p",
                   decimalPlaces = 1,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center',
                   width = '100%')
})

output$inc = renderUI({
  
  autonumericInput(inputId = "inc",
                   label = HTML("연간 투자금액 증가율 (%)"),
                   value = 3, 
                   currencySymbolPlacement = "p",
                   decimalPlaces = 1,
                   digitGroupSeparator = ",",
                   decimalCharacter = ".",
                   align = 'center',
                   width = '100%')
})

output$cal = renderUI({
  
  actionButton("cal", "계산하기!",
               icon = icon("calculator"), width = '100%',
               class = "btn-info")
  
})

output$screen = renderUI({
  
  actionButton("screen", "페이지 저장하기",
               icon = icon("camera-retro"), width = '100%',
               class = "btn-success")
  
})