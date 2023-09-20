server <- function(input, output){
  
  select_column_c <- eventReactive(input$finish_c, {
    df_column <- dataset 
    return(df_column)
  })
  
  output$timedate_c <- renderUI({
    
    column_name <- input$column_c
    
    df <- dataset
    
    dates <- df$date 
    
    min_time <- (min(dates)) 
    max_time <- (max(dates))
    
    dateRangeInput("true_date_c", "Período de análise",
                   end = max_time,
                   start = min_time,
                   min    = min_time,
                   max    = max_time,
                   format = "dd/mm/yy",
                   separator = " - ",
                   language='pt-BR')
  })
  
  Info_DataTable_COMP <- eventReactive(input$finish_c,{
    df <- select_column_c()
    column_names <- input$column_c
    date_interval <- input$true_date_c
    
    datacut <- df[df$date >= date_interval[1] & df$date <= date_interval[2],]
    
    vetor_1 <- datacut[, c(column_names[1])]
    vetor_2 <- datacut[, c(column_names[2])]
    
    correlation <- cor(vetor_1, vetor_2)
    
    current_columns <- paste(column_names[1], column_names[2], sep = ' x ')
    
    df_tb <-  data.frame(current_columns, correlation)
    df_tb <- as.data.frame(t(df_tb))
    print(df_tb)
    return(df_tb)
  })
  
  observeEvent(input$finish_c, {
    Info_DataTable_COMP()
  })
  
  output$info_correlation <- renderDT({
    column_names <- input$column_c
    if (length(column_names)>1){
      Info_DataTable_COMP() %>%
        as.data.frame() %>% 
        DT::datatable(options=list(
          language=list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        ))
    }
  })
  
  output$sh <- renderPlot({
    df <- select_column_c()
    column_name <- input$column_c
    twin <- input$true_date_c
    
    datacut <- df[df$date >= twin[1] & df$date <= twin[2],]
    
    
    aux <- datacut[, column_name]

    aux1 <- min(aux)
    aux2 <- max(aux)
    print("aux 1")
    print(aux1)
    print("aux 2")
    print(aux2)
    print("column names")
    print(column_name)
    
    datacut$date <- ymd(datacut$date)
    
    tempo <- datacut$date
    variavel1 <- aux[1]
    variavel2 <- aux[2]
    
    tempo <- datacut$date

    a <- ggplot(data = datacut, aes(x = date)) +
      geom_line(aes(y = .data[[column_name[1]]]), color = "#069808", size = 1, alpha = 0.8) +
      geom_line(aes(y = .data[[column_name[2]]]), color = "#FF5733", size = 1, alpha = 0.8) +
      ylab(toString(column_name)) +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")
    
    print(a)

  })
  
  
  
}
