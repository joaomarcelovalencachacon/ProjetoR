server <- function(input, output){
  
  select_column_c <- eventReactive(input$finish_c, {
    
    dados_filtrados <- dataset %>%
      filter(City %in% c(input$selected_city))
    
    df_column <- dados_filtrados
    return(df_column)
  })
  
  output$timedate_c <- renderUI({
    
    column_name <- input$column_c
    
    dados_filtrados <- dataset %>%
      filter(City %in% c(input$selected_city))
    
    df <- dados_filtrados
    
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
  
  output$linha <- renderPlot({
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
  
  output$barras_media <- renderPlot({
    column_name <- input$column_c
    if (length(column_name)>1){
      df <- select_column_c()
      twin <- input$true_date_c
      
      datacut <- df[df$date >= twin[1] & df$date <= twin[2],]
      
      datacut$date <- ymd(datacut$date)
      
      vector_1 <- datacut[, c(column_name[1])]
      vector_2 <- datacut[, c(column_name[2])]
      
      mean_1 <- mean(vector_1)
      mean_2 <- mean(vector_2)
      
      columns <- c(column_name[1], column_name[2])
      means <- c(mean_1, mean_2)
      
      df_plot <- data.frame(columns, means)
      
      colors <- c("#069808", "#FF5733")
      
      m <- ggplot(df_plot, aes(x = columns, y = means, fill = columns)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = colors) +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        theme_minimal()
      
      print(m) 
    }
  })

  output$dispersao <- renderPlot({
    
    # All the inputs
    df <- select_column_c()
    column_name <- input$column_c
    twin <- input$true_date_c
    
    datacut <- df[df$date >= twin[1] & df$date <= twin[2],]
    
    auxA <- datacut[, column_name[1]]
    auxB <- datacut[, column_name[2]]
    
    datacut$date <- ymd(datacut$date)
    
    dados <- data.frame(
      auxA,
      auxB
    )
    
    # Criar o gráfico de dispersão
    a <- ggplot(dados, aes(x = auxA, y = auxB)) +
      geom_point() + # Adicionar pontos
      labs(x = column_name[1], y = column_name[2], title = "Gráfico de Dispersão")
    
    print(a)
  })
  
  ##########################################################################################
  
  select_column_m <- eventReactive(input$finish_m, {
    
    dados_filtrados <- dataset %>%
      filter(City %in% c(input$selected_city_m))
    
    df_column <- dados_filtrados
    return(df_column)
  })
  
  output$timedate_m <- renderUI({
    
    column_name <- input$column_m
    
    dados_filtrados <- dataset %>%
      filter(City %in% c(input$selected_city_m))
    
    df <- dados_filtrados
    
    dates <- df$date 
    
    min_time <- (min(dates)) 
    max_time <- (max(dates))
    
    dateRangeInput("true_date_m", "Período de análise",
                   end = max_time,
                   start = min_time,
                   min    = min_time,
                   max    = max_time,
                   format = "dd/mm/yy",
                   separator = " - ",
                   language='pt-BR')
  })
  
  Info_DataTable_COMP_m <- eventReactive(input$finish_m,{
    df <- select_column_m()
    column_names <- input$column_m
    date_interval <- input$true_date_m
    
    datacut <- df[df$date >= date_interval[1] & df$date <= date_interval[2],]
    datacut
    
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    
    moda <- Mode(datacut[, c(column_names)])
    mediana <- median(datacut[, c(column_names)])
    media <- mean(datacut[, c(column_names)])
    desvio_padrao <- sd(datacut[, c(column_names)])
    minimo <- min(datacut[, c(column_names)])
    maximo <- max(datacut[, c(column_names)])
    
    classe <- column_names
    
    df_tb_m <-  data.frame(classe, moda, media, mediana, desvio_padrao, maximo, minimo)
    df_tb_m <- as.data.frame(t(df_tb_m))
    print(df_tb_m)
    return(df_tb_m)
  })
  
  observeEvent(input$finish_c, {
    Info_DataTable_COMP_m()
  })
  
  output$info_m <- renderDT({
    column_names <- input$column_m
    if (length(column_names)>=1){
      Info_DataTable_COMP_m() %>%
        as.data.frame() %>% 
        DT::datatable(options=list(
          language=list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        ))
    }
  })
  
  output$linha_m <- renderPlot({
    df <- select_column_m()
    column_name <- input$column_m
    twin <- input$true_date_m
    
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
      geom_line(aes(y = .data[[column_name]]), color = "#069898", size = 1, alpha = 0.8) +
      ylab(toString(column_name)) +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")
    
    print(a)
    
  })
  
  output$hist_m <- renderPlot({
    df <- select_column_m()
    column_name <- input$column_m
    twin <- input$true_date_m
    datacut <- df[df$date >= twin[1] & df$date <= twin[2],]
    
    datacut %>%
        ggplot(aes(x = datacut[, c(column_name)])) +
        geom_histogram() +
        labs(x = column_name)
    
  })
  
  
  output$boxplot_m <- renderPlot({
    df <- select_column_m()
    column_name <- input$column_m
    twin <- input$true_date_m
    datacut <- df[df$date >= twin[1] & df$date <= twin[2],]
    
    datacut %>%
      ggplot(aes(y = datacut[, c(column_name)])) +
      geom_boxplot()+
      labs(y = column_name)
    
    
    
  })
  
  
  
}
