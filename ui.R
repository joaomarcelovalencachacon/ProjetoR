library(shiny)
library(shinydashboard)
library(shinyWidgets)
install.packages("DT")
library(DT)

header <- dashboardHeader(title = "Projeto-R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "m", icon = icon("stats", lib = "glyphicon")),
    menuItem('Comparações', tabName = "c", icon = icon("chart-line"))
  )
)

body <- dashboardBody(
  tabItem(tabName = 'c',
          fluidRow(
            box(title = 'Selecione as classes',
                width=12,
                solidHeader = TRUE,
                status = 'info',
                selectInput('column_c', 'Dados', columns, multiple=TRUE),
                uiOutput("timedate_c"),
                actionButton('finish_c', 'Avançar')
            )
          ),
          fluidRow(
            box(title = "Correlação entre as colunas", width = 12, solidHeader = TRUE,
                DTOutput('info_correlation')
            )
          ),
          
)
)

ui <- dashboardPage(
  header, sidebar, body
)
