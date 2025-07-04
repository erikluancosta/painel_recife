library(DBI)
library(RPostgres)
library(tidyverse)
library(shiny)
library(bs4Dash)
library(ciTools)
library(lubridate)
library(vitaltable)
library(leaflet)
library(dplyr)
library(shinythemes)
library(janitor)
library(plotly)
library(reshape2)
library(forcats)
library(sf)
library(tidyr)
library(DT)
library(haven)
library(writexl)
library(fresh)
library(shinymanager)

# Configurar a conexão ao banco de dados PostgreSQL
rel <- vitaltable::rel

# Define o tema customizado com 'fresh'
tema <- fresh::create_theme(
  fresh::bs4dash_status(
    info = "#121E87",
    secondary = "#FF5054",
    danger = "#FFC73B",
    primary = "#0099D6",
    warning = "#121E54"
  )
)

# Define as credenciais para acesso (login e senha: "admin")
credentials <- data.frame(
  user = c("admin"),
  password = c("admin"),
  stringsAsFactors = FALSE
)

print(credentials)

# Interface principal do aplicativo (bs4DashPage)
app_ui <- bs4DashPage(
  header = dashboardHeader(
    title = bs4DashBrand(
      title = "Recife",
      color = "info",
      image = "https://www.vitalstrategies.org/wp-content/uploads/2019/05/vs_author_icon02-300x300.png"
    )
  ),
  sidebar = bs4DashSidebar(
    status = "secondary",
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "Mapa da violência",
        tabName = "mapa_viol",
        icon = icon("map")
      ),
      #bs4SidebarMenuItem(
      #  "Início",
      #  tabName = "home",
      #  icon = icon("house")
      #),
      menuItem("Análises descritivas", icon = icon("chart-column"), 
               menuSubItem("SINAN Violência", tabName = "tela_sinan"),
               menuSubItem("SINAN Intox Exogena", tabName = "tela_iexo"),
               menuSubItem("Hospitalizações", tabName = "tela_sih"),
               menuSubItem("Mortalidade", tabName = "tela_sim")
      ),
      menuItem("Linkage", icon = icon("link"),
               menuSubItem("Análise do linkage", tabName = "analise_linkage"),
               menuSubItem("Linha da vida", tabName = "linhavida")
      ),
      bs4SidebarMenuItem(
        "Nota técnica",
        tabName = "introducao",
        icon = icon("clipboard")
      )
    )
  ),
  body = bs4DashBody(
    fresh::use_theme(tema),
    # CSS customizado para alterar a cor de hover do pop-up dos pickerInput
    tags$head(
      tags$style(HTML("
        .bootstrap-select .dropdown-menu li a:hover,
        .bootstrap-select .dropdown-menu li a:focus,
        .bootstrap-select .dropdown-menu li a.active {
          background-color: #FFC73B !important;
          border-color: #FFC73B !important;
        }
      "))
    ),
    # CSS customizado para o sliderInput
    tags$style(type = "text/css", 
               ".irs-bar { background-color: transparent !important; }",
               ".irs-bar-edge { background-color: transparent !important; }",
               ".irs-slider { background-color: #337ab7 !important; }"),
    bs4TabItems(
     # bs4TabItem(
    #    tabName = "home",
    #    home_ui("home_")
     # ),
      bs4TabItem(
        tabName = "tela_sinan",
        sinan_ui("sinan")
      ),
      bs4TabItem(
        tabName = "tela_iexo",
        iexo_ui("iexo")
      ),
      bs4TabItem(
        tabName = "tela_sih",
        sih_ui("sih")
      ),
      bs4TabItem(
        tabName = "tela_sim",
        sim_ui("sim")
      ),
      bs4TabItem(
        tabName = "analise_linkage",
        linkage_ui("linkage")
      ),
      bs4TabItem(
        tabName = "linhavida",
        linhavida_ui("linhavida")
      ),
      bs4TabItem(
        tabName = "mapa_viol",
        mapa_ui("mapa_violencia")
      )
    )
  ),
  footer = dashboardFooter(
    left = "©Vital Strategies, Inc., 2024, 501(c)(3) not-for-profit organization"
  )
)

# Embrulha a interface principal com a função secure_app() do shinymanager
ui <- secure_app(app_ui)

server <- function(input, output, session) {
  # Verifica as credenciais - usuário somente consegue acessar o app se passar
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # Agora, carrega os módulos do aplicativo
 # home_server("home_")       # Servidor da tela inicial
  sinan_server("sinan")      # Servidor da tela do SINAN
  iexo_server("iexo")        # Servidor da tela do SINAN Intoxicação Exógena
  sih_server("sih")          # Servidor da tela do SIH
  sim_server("sim")          # Servidor da tela do SIM
  linkage_server("linkage")  # Servidor da tela de análise do linkage
  linhavida_server("linhavida")  # Servidor da linha da vida
  mapa_server("mapa_violencia")   # Servidor do mapa da violência
}

shinyApp(ui, server)