# birth = '1987-08-16'
# retire = 55
# death = 90
# wealth = 100000000
# invest = 400000
# pension = 4000000
# nps_amt = 2000000
# nps_dc = 65
# going = 'Yes'
# inf = 1.1
# inc = 3.2
# 
# plan <- data.frame(
#   type = c('입금', '출금'),
#   age = c(55, 55),
#   amt = c(10, 80)
# )
# ret = 0.0709

cal_df = function(i) {
  
  birth = input$birth
  retire = input$retire
  death = input$death
  wealth = input$wealth
  pension = input$pension
  nps_amt = input$nps_amt
  nps_dc = input$nps_dc
  going = input$going
  inf = input$inf
  inc = input$inc
  
  if (input$tp == 'a') {
    ret = i
    invest = input$invest
  }
  
  if (input$tp == 'b') {
    ret = input$t_ret / 100
    invest = i
  }
  
  plan <- rv$df %>% select(-Buttons)
  
  if (!'출금' %in% plan$type) {
    plan = plan %>% add_row(type = '출금', age = 0, amt = 0)
  }
  
  if (!'입금' %in% plan$type) {
    plan = plan %>% add_row(type = '입금', age = 0, amt = 0)
  }
  
  plan =  plan %>% group_by(type, age) %>%
    summarize(amt = sum(amt), .groups = 'drop')
  
  # if (!'출금' %in% plan$type) {
  #   plan = plan %>% add_row(type = '출금', age = 0, amt = 0)
  # }
  # 
  # if (!'입금' %in% plan$type) {
  #   plan = plan %>% add_row(type = '입금', age = 0, amt = 0)
  # }
  
   
  ret.m = (1 +ret)^(1/12) - 1
  nps_ratio = data.frame('diff' = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
             'dc' = c(0.70, 0.76, 0.82, 0.88, 0.94, 1, 1.072, 1.144, 1.216, 1.288, 1.360)) 
  
  nps_ratio = 
    nps_ratio[nps_ratio$diff == (nps_dc - nps_age[last(which(year(birth) >= nps_age$year)), 'amt']), 'dc']
  
  
  nps_amt_new = nps_amt * nps_ratio

  df = data.frame(
    date = seq(Sys.Date(), as.Date(birth) %m+% (years(death + 1)), by = 'month')
  ) 
    
  df = df %>%
    mutate(
      age = as.duration(interval(as.Date(birth), date)) %/% as.duration(years(1)),
     count = rep(0: (nrow(df)- 1)),
     cnt_pass = year(date) - year(Sys.Date()),
     cnt_nps = year(date) - year(date[which(age == nps_dc)[1]]),
     inv = if_else(age < retire, invest * (1+inc/100)^(cnt_pass), 0),
     need = if_else(age >= retire, pension * (1+inf/100)^(cnt_pass), 0),
     nps = if_else(age >= nps_dc, nps_amt_new * (1+inf/100)^(cnt_nps), 0)
     ) %>%
    select(-inv, everything(), inv)
  
  plan_sp =
    plan %>% 
    spread(type, amt) %>%
    left_join(df, by = 'age') %>%
    distinct(age, .keep_all = TRUE) %>%
    rename('in' = '입금', 'out' = '출금') %>%
    select(date, 'in', out)
  
  # plan_sp = plan %>% left_join(df, by = 'age') %>% distinct(age, .keep_all = TRUE) %>%
  #   spread(type, amt) %>%
  #   rename('in' = '입금', 'out' = '출금') %>%
  #   select(date, 'in', out)
  
  df = df %>% left_join(plan_sp, by = c('date')) %>% replace(is.na(.), 0)
  
  df[1, 'wealth'] = wealth
  df[1, 'bop'] = df[1, 'wealth'] + df[1, 'inv']
  
  df[1, 'ret'] = 0
  df[1, 'withdraw'] = df[1, 'need'] - df[1, 'nps']
  df[1, 'eop'] = df[1, 'bop'] + df[1, 'ret'] + df[1, 'in'] -
    df[1, 'out'] - df[1, 'withdraw']
  
  for (i in 2: nrow(df)) {
      
      df[i, 'wealth'] = df[(i-1), 'eop']
      df[i, 'bop'] = df[i, 'wealth'] + df[i, 'inv']
      
      df[i, 'ret'] =  df[i, 'bop'] * ret.m
      df[i, 'withdraw'] = df[i, 'need'] - df[i, 'nps']
      
      df[i, 'wealth'] = df[(i-1), 'eop']
      df[i, 'bop'] = df[i, 'wealth'] + df[i, 'inv']
      
      if (df[i, 'age'] < retire ) {
        df[i, 'ret'] = df[i, 'bop'] * ret.m
      } else {
        df[i, 'ret'] = df[i, 'bop'] * if_else(going == 'Yes', ret.m, inf/100/12)
      }
      
      df[i, 'eop'] = df[i, 'bop'] + df[i, 'ret'] + df[i, 'in'] -
        df[i, 'out'] - df[i, 'withdraw']
    
  }
  
  return(df)

}

opt_R = function() {
  
  target_func = function(input) {
    
    df = cal_df(input)
    
    return( abs(last(df$eop)) )
  }
  
  if (input$tp == 'a') { cap = 2 } 
  if (input$tp == 'b') { cap = 100000000 }
  
  ans = optimize(f = target_func,
                 interval = c(0,cap),
                 maximum = F)
  
  return(ans$minimum)
}
