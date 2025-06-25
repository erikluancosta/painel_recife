library(sf)
library(DT)

home_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Adiciona estilo apenas para este tabItem específico
    tabItem(tabName = "inicio",
            div(style = "text-align: center; background-color: white; min-height: 100vh;",
                tags$style(HTML("
                  /* Aplica fundo branco apenas neste módulo */
                  #inicio {
                    background-color: white !important;
                  }
                "))#,
                #img(src = "Capa-linkageGBV.png", style = "max-width: 100%; height: auto;")
            )
    )
  )
}

home_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # SINAN ----
      
      
    }
  )}





