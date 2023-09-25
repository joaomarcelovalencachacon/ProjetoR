
library(shiny)
library(shinydashboard)
library(DT)

header <- dashboardHeader(title = "Projeto-R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "m", icon = icon("stats", lib = "glyphicon")),
    menuItem('Comparações', tabName = "c", icon = icon("chart-line"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'c',
            fluidRow(
              box(title = 'Selecione 2 opções',
                  width=12,
                  solidHeader = TRUE,
                  status = 'info',
                  selectInput('column_c', 'Dados', columns, multiple=TRUE),
                  selectInput('selected_city', 'Cidade', cities, multiple=FALSE),
                  uiOutput("timedate_c"),
                  actionButton('finish_c', 'Avançar')
              )
            ),
            fluidRow(
              box(title = "Correlação entre as colunas", width = 12, solidHeader = TRUE,
                  DTOutput('info_correlation')
              )
            ),
            fluidRow(
              box(title = "Gráfico de linha", width = 12, solidHeader = TRUE,
                  plotOutput('linha')
              )
            ),
            fluidRow(
              box(title = "Gráfico de barra das médias", width = 12, solidHeader = TRUE,
                  plotOutput('barras_media')
              )
            ),
            fluidRow(
              box(title = "Gráfico de dispersão", width = 12, solidHeader = TRUE,
                  plotOutput('dispersao')
              )
            )
            
    ),
    tabItem(tabName = 'm',
            fluidRow(
              box(title = 'Selecione a classe a ser analisada',
                  width=12,
                  solidHeader = TRUE,
                  status = 'info',
                  selectInput('column_m', 'Dados', columns, multiple=FALSE),
                  selectInput('selected_city_m', 'Cidade', cities, multiple=FALSE),
                  uiOutput("timedate_m"),
                  actionButton('finish_m', 'Avançar')
              )
            ),
            fluidRow(
              box(title = "Tabela", width = 12, solidHeader = TRUE,
                  DTOutput('info_m')
              ),
              fluidRow(
                box(title = "Gráfico em linha da série", width = 12, solidHeader = TRUE,
                    plotOutput('linha_m')
                )
              ),
              fluidRow(
                box(title = "Histograma da série", width = 12, solidHeader = TRUE,
                    plotOutput('hist_m')
                )
              ),
              fluidRow(
                box(title = "Boxplot da série", width = 12, solidHeader = TRUE,
                    plotOutput('boxplot_m')
                )
              )
            )
            
            
    )
    
  )
)

ui <- dashboardPage(
  header, sidebar, body
)
