library(leaflet)
library(shiny)
library(sf)
library(readxl)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)
library(leaflet.extras)
library(DBI)
library(RPostgres)

# Carregamento dos dados
load('dados/cnes.RData')
load('dados/pontos_viol.RData')
load('dados/cnes_join.RData')
load('dados/pontos_viol_real.RData')

pontos_viol <- pontos_viol |> 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

df_linha_vida2 <- df_linha_vida |>
  left_join(cnes_join, by = "id_unico")

# Une os dados de pontos com os dados de linha de vida pela coluna "id_unico"
pontos_viol <- pontos_viol |>
  left_join(df_linha_vida2, by = c("id_unico" = "id_unico"))

pontos_viol <- pontos_viol |>
  mutate(cd_cnes_unid_not = as.numeric(cd_cnes_unid_not))

# Converte colunas de data no dataframe df_linha_vida2
df_linha_vida2 <- df_linha_vida2 |>
  mutate(
    dt_evento_inicio = as.Date(dt_evento_inicio),
    dt_evento_fim = as.Date(dt_evento_fim),
    dt_registro = as.Date(dt_registro),
    dt_comum = coalesce(dt_registro, dt_evento_inicio, dt_evento_fim)
  )

# Ler o arquivo GeoJSON do mapa
mapa <- st_read("dados/bairros.geojson")

# Módulo UI do mapa
mapa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(HTML(paste0("
      #mapa_container {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
      }
      .leaflet-container {
        height: 100vh !important;
        width: 100vw !important;
      }
      #", ns("filtros"), ", #", ns("mapa_panel"), " {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0px 0px 15px rgba(0, 0, 0, 0.3);
        display: none;
      }
    "))),
    div(id = "mapa_container", 
        leafletOutput(ns("mapa"))
    ),
    # Botão para mostrar/ocultar filtros
    absolutePanel(
      top = 80, right = 20,
      actionButton(ns("toggle_filtros"), "Filtros")
    ),
    # Painel de filtros
    absolutePanel(
      top = 130, right = 20,
      id = ns("filtros"),
      h4("Filtros"),
      numericInput(
        ns("filtro_id_pessoa"), "ID Pessoa:",
        value = NA,
        min = min(pontos_viol$id_pessoa, na.rm = TRUE),
        max = max(pontos_viol$id_pessoa, na.rm = TRUE),
        step = 0
      ),
      sliderInput(
        ns("filtro_idade"), "Idade:",
        min = min(pontos_viol$nu_idade_anos, na.rm = TRUE),
        max = max(pontos_viol$nu_idade_anos, na.rm = TRUE),
        value = c(min(pontos_viol$nu_idade_anos, na.rm = TRUE), 
                  max(pontos_viol$nu_idade_anos, na.rm = TRUE))
      ),
      selectInput(
        ns("filtro_raca"), "Raça/cor:",
        choices = c("Todas", "Branca", "Preta", "Parda", "Amarela", "Indígena", "Ignorada"),
        selected = "Todas"
      )
    ),
    # Botão para mostrar/ocultar opções do mapa
    #absolutePanel(
    #  top = 80, right = 120,
    #  actionButton(ns("toggle_mapa"), "Mapa")
    #),
    # Painel de opções do mapa
    absolutePanel(
      top = 130, right = 120,
      id = ns("mapa_panel"),
      h4("Mapa"),
      selectInput(
        ns("map_style"), "Estilo do mapa:",
        choices = c("Padrão", "Carto Positron"),
        selected = "Carto Positron"
      ),
      sliderInput(
        ns("fill_opacity"), "Intensidade do preenchimento:",
        min = 0, max = 1, value = 0.3, step = 0.1
      ),
      sliderInput(
        ns("line_opacity"), "Intensidade da borda:",
        min = 0, max = 1, value = 0.9, step = 0.1
      ),
      colourInput(
        ns("polygon_color"), "Cor do shapefile:",
        value = "#FFC73B"
      ),
      colourInput(
        ns("default_color"), "Cor padrão dos pontos:",
        value = "#121E87"
      ),
      colourInput(
        ns("highlight_color"), "Cor do destaque:",
        value = "#FF5054"
      ),
      actionButton(ns("toggle_heatmap"), "Ativar Heatmap")
    )
  )
}

# Módulo server do mapa
mapa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Variável para controlar o estado do heatmap
    heatmap_active <- reactiveVal(FALSE)
    
    # Variável para armazenar o CNES (unidade selecionada)
    selected_cnes <- reactiveVal(NULL)
    
    # Variável para monitorar o tempo do último clique (para detectar double-click)
    last_click_time <- reactiveVal(Sys.time())
    
    # Toggles para mostrar/ocultar filtros e painel do mapa
    observeEvent(input$toggle_filtros, {
      shinyjs::toggle("filtros")
    })
    
    observeEvent(input$toggle_mapa, {
      shinyjs::toggle("mapa_panel")
    })
    
    observeEvent(input$toggle_heatmap, {
      heatmap_active(!heatmap_active())
      updateActionButton(session, "toggle_heatmap",
                         label = ifelse(heatmap_active(), "Desativar Heatmap", "Ativar Heatmap"))
      update_map()
    })
    
    observeEvent(input$mapa_marker_click, {
      click <- input$mapa_marker_click
      if (!is.null(click$id) && click$id != "") {
        selected_cnes(click$id)
      }
    })
    
    observeEvent(input$mapa_click, {
      current_time <- Sys.time()
      time_diff <- as.numeric(difftime(current_time, last_click_time(), units = "secs"))
      # Se for double-click (< 0.5s) e houver um CNES selecionado, desmarcar
      if (time_diff < 0.5 && isTruthy(selected_cnes())) {
        selected_cnes(NULL)
      }
      last_click_time(current_time)
    })
    
    # Reactive para os dados filtrados – usamos req() para garantir que os inputs essenciais estão definidos
    filtered_pontos <- reactive({
      req(input$filtro_raca, input$filtro_idade)
      data <- pontos_viol
      
      if (input$filtro_raca != "Todas") {
        data <- data %>% filter(ds_raca == input$filtro_raca)
      }
      
      # Garante que input$filtro_idade tenha os dois valores
      if (length(input$filtro_idade) < 2) {
        return(data)
      }
      
      data <- data %>% filter(
        nu_idade_anos >= input$filtro_idade[1],
        nu_idade_anos <= input$filtro_idade[2]
      )
      
      data$highlight <- FALSE
      if (!is.null(input$filtro_id_pessoa) &&
          !is.na(input$filtro_id_pessoa) &&
          input$filtro_id_pessoa != "") {
        selected_id <- as.numeric(input$filtro_id_pessoa)
        data$highlight[data$id_pessoa == selected_id] <- TRUE
      }
      
      if (isTruthy(selected_cnes())) {
        data <- data %>% filter(cd_cnes_unid_not == as.numeric(selected_cnes()))
      }
      
      data
    })
    
    # Ícone para as unidades de saúde (CNES)
    hospitalIcon <- makeIcon(
      iconUrl = "www/hospital.png",
      iconWidth = 30,
      iconHeight = 30
    )
    
    # Função reativa para atualizar o mapa
    update_map <- reactive({
      req(input$map_style, input$fill_opacity, input$line_opacity, input$polygon_color)
      
      map <- leaflet() %>% setView(lng = -34.946671, lat = -8.039802, zoom = 12)
      
      if (input$map_style == "Carto Positron") {
        map <- map %>% addProviderTiles("CartoDB.Positron")
      } else if (input$map_style == "Stamen Toner") {
        map <- map %>% addProviderTiles("Stamen.Toner")
      } else if (input$map_style == "Stamen Watercolor") {
        map <- map %>% addProviderTiles("Stamen.Watercolor")
      } else {
        map <- map %>% addTiles()
      }
      
      map <- map %>% addPolygons(
        data = mapa,
        color = input$polygon_color, 
        weight = 1, 
        opacity = input$line_opacity,
        fillOpacity = input$fill_opacity
      )
      
      # Caso um CNES esteja selecionado, filtra os dados de CNES
      cnes_data <- cnes
      if (isTruthy(selected_cnes())) {
        cnes_data <- cnes_data %>% filter(cd_cnes_unid_not == as.numeric(selected_cnes()))
      }
      
      map <- map %>% addMarkers(
        data = cnes_data,
        lng = ~longitude_cnes,
        lat = ~latitude_cnes,
        icon = hospitalIcon,
        popup = ~paste("<b>Unidade de saúde:</b>", NO_FANTASIA),
        group = "cnes",
        layerId = ~cd_cnes_unid_not
      )
      
      if (heatmap_active()) {
        if (nrow(filtered_pontos()) > 0) {
          map <- map %>% addWebGLHeatmap(
            data = filtered_pontos(),
            lng = ~Longitude, 
            lat = ~Latitude,
            size = 60000,
            group = "heatmap"
          )
        }
      } else {
        if (nrow(filtered_pontos()) > 0) {
          map <- map %>% addCircleMarkers(
            data = filtered_pontos(),
            lng = ~Longitude, 
            lat = ~Latitude,
            color = ~ifelse(highlight, input$highlight_color, input$default_color),
            radius = ~ifelse(highlight, 5, 1),
            fillOpacity = 0.7,
            popup = ~paste(
              "<b>DADOS DEMOGRÁFICOS</b> <br>",
              "<b>Idade:</b>", nu_idade_anos, "<br>",
              "<b>Raça/cor:</b>", ds_raca_padronizada, "<br>",
              "<br><br>", texto_final
            ),
            group = "markers"
          )
        }
      }
      
      map
    })
    
    output$mapa <- renderLeaflet({
      update_map()
    })
    
    # Atualiza o mapa via leafletProxy sempre que os inputs mudam
    observe({
      leafletProxy("mapa") %>% 
        clearGroup("markers") %>% 
        clearGroup("heatmap") %>% 
        clearGroup("cnes")
      
      cnes_data <- cnes
      if (isTruthy(selected_cnes())) {
        cnes_data <- cnes_data %>% filter(cd_cnes_unid_not == as.numeric(selected_cnes()))
      }
      
      leafletProxy("mapa") %>%
        addMarkers(
          data = cnes_data,
          lng = ~longitude_cnes,
          lat = ~latitude_cnes,
          icon = hospitalIcon,
          popup = ~paste("<b>Unidade de saúde:</b>", NO_FANTASIA),
          group = "cnes",
          layerId = ~cd_cnes_unid_not
        )
      
      if (heatmap_active()) {
        if (nrow(filtered_pontos()) > 0) {
          leafletProxy("mapa") %>%
            addHeatmap(
              data = filtered_pontos(),
              lng = ~Longitude, 
              lat = ~Latitude,
              intensity = ~1,
              blur = 20,
              max = 0.05,
              radius = 15,
              gradient = colorRampPalette(c("blue", "red"))(5),
              group = "heatmap"
            )
        }
      } else {
        if (nrow(filtered_pontos()) > 0) {
          leafletProxy("mapa") %>%
            addCircleMarkers(
              data = filtered_pontos(),
              lng = ~Longitude, 
              lat = ~Latitude,
              color = ~ifelse(highlight, input$highlight_color, input$default_color),
              radius = ~ifelse(highlight, 5, 1),
              fillOpacity = 0.7,
              popup = ~paste(
                "<b>DADOS DEMOGRÁFICOS</b> <br>",
                "<b>Idade:</b>", nu_idade_anos, "<br>",
                "<b>Raça/cor:</b>", ds_raca_padronizada, "<br>",
                "<br><br>", texto_final
              ),
              group = "markers"
            )
        }
      }
    })
    
  })
}

