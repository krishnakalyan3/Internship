library(shiny)
library(ggplot2)
library(lubridate)
library(forecast)


shinyServer(function(input, output) {
  
  # Forecast / Currently UI commented
  output$plot2 <- renderPlot({
    df = choice(input$radio,input$dates[1],input$dates[2])
    ts1 = ts((pmax(df$energy,0)),start=1,frequency = 48)
    m_naive = snaive(log(ts1 +.1), h=7*48,level=c(60,85,90))
    plot(m_naive)
  })
  
  # ggplot Simulator
  output$plot1 <- renderPlot({ 
    df = choice(input$radio,input$dates[1],input$dates[2])
    p = ggplot(df, aes(x=data, y=energy)) +
      theme_bw(base_size = 12) +
      geom_line(aes(group=1), alpha=0.3 , size=0.9,col='blue') +
      xlab("Time Line") + ylab("Energy in (MW)") +
      ggtitle('Energy vs Time')
    print(p)
  })
  
  output$table <- renderDataTable({ 
    df = choice(input$radio,input$dates[1],input$dates[2])
    df
    
  }, options = list(lengthMenu = c(7, 14, 21), pageLength = 7))
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('data_',input$radio, '.csv', sep='') },
    content = function(file) {
      df = choice(input$radio,input$dates[1],input$dates[2])
      write.csv(df, file, row.names=FALSE)
    }
  )
  
  choice = function(choice,date1,date2){
    data = (gendates(date1,date2))
    if(choice == '1'){
      df = wind(data,as.numeric(input$var))
    }
    if(choice == '2'){
      df = solar(data,as.numeric(input$var))
    }
    if(choice == '3'){
      df = nuclear(data,as.numeric(input$var),225)
    }
    if(choice == '4'){
      df = constant(data,as.numeric(input$var),200)
    }
    if(choice == '5'){
      df = constant(data,as.numeric(input$var),250)
    }
    return(df)
  }
  
  gendates = function(start,end){
    sd = as.POSIXct(paste(start,"00:00:00"),tz='UTC')
    ed = as.POSIXct(paste(end,"00:00:00"),tz='UTC')
    date = seq(sd, ed, 1800)
    return(date)
  }
  
  solar = function(data,jitter){
    load(file = "model/model_temp_year.RData")
    # Solar Mean - 200
    t.lub <- ymd_hms(data)
    h.lub <- hour(t.lub)
    condition1 = (h.lub > 19 | h.lub <= 7)
    # randvalue = runif(1, -jitter, jitter)
    df = as.data.frame(data)
    randvalue = runif(nrow(df), -jitter, jitter)
    df$energy = predict(modeltemp,df$data) * 11.8 + randvalue
    df$energy = ifelse(condition1 , 0 , df$energy)
    df$energy = ifelse(df$energy < 0, 0, df$energy)
    return(df)
  }
  
  wind = function(data,jitter){
    load(file = "model/model_speed_year.RData")
    # Wind Mean - 300
    df = as.data.frame(data)
    randvalue = runif(nrow(df), -jitter, jitter)
    df$energy = predict(modelspeed,df$data) * 120 + randvalue
    df$energy = ifelse(df$energy < 0, 0, df$energy)
    return(df)
  }
    
  nuclear = function(data, jitter,value){
    # Nuclear Mean 225
    df = as.data.frame(data)
    randvalue = runif(nrow(df), -jitter, jitter)
    t.lub <- ymd_hms(data)
    h.lub <- hour(t.lub)
    condition1 = h.lub > 2 & h.lub <= 8
    condition2 = h.lub > 8 & h.lub <= 16
    df$energy = ifelse(condition1 , 0 , 
                       ifelse(condition2,value/2,value)) + randvalue
    return(df)
  }
  
  constant = function(data,jitter,value){
    # Thermal mean - 250
    # Hydro mean- 200
    df = as.data.frame(data)
    randvalue = runif(nrow(df), -jitter, jitter)
    df$energy = value + randvalue
    return(df)
  }
})


