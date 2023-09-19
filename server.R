library(dplyr)

server <- function(input, output){
  
  select_column_comp <- eventReactive(input$finish_c, {
    df_column <- dataset 
    return(df_column)
  })
  
  output$timedate_c <- renderUI({
    
    column_name <- input$column_c
    
    df <- dataset
    
    dates <- df$date 
 
    min_time <- (min(dates)) 
    max_time <- (max(dates))
    
    dateRangeInput("true_date_comp", "Período de análise",
                   end = max_time,
                   start = min_time,
                   min    = min_time,
                   max    = max_time,
                   format = "dd/mm/yy",
                   separator = " - ",
                   language='pt-BR')
  })

}
