# Carregar bibliotecas necessárias
library(shiny)
library(bs4Dash)
library(plotly)
library(dplyr)
library(lubridate)
library(scales)
library(DBI)

#load("dados/df_linha_vida.RData")

# O CERTO
load("dados/linha_vida3.RData")

# Para ver violencia sexual e parto
#load("dados/temp_teste_vida.RData")

df_linha_vida <- df_linha_vida |> 
  group_by(id_pessoa) |> 
  mutate(idade_minima = min(nu_idade_anos, na.rm = TRUE)) |> 
  ungroup() |> 
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      idade_minima < 1 ~ "<1",
      idade_minima >= 1 & idade_minima <= 4 ~ "01-04",
      idade_minima >= 5 & idade_minima <= 9 ~ "05-09", 
      idade_minima >= 10 & idade_minima <= 19 ~ "10-19", 
      idade_minima >= 20 & idade_minima <= 29 ~ "20-29", 
      idade_minima >= 30 & idade_minima <= 39 ~ "30-39", 
      idade_minima >= 40 & idade_minima <= 49 ~ "40-49", 
      idade_minima >= 50 & idade_minima <= 59 ~ "50-59", 
      idade_minima >= 60 & idade_minima <= 69 ~ "60-69", 
      idade_minima >= 70 & idade_minima <= 79 ~ "70-79", 
      idade_minima >= 80 ~ "80+", 
      TRUE ~ as.character(idade_minima)
    )
  )

#vitallinkage::faixa_etaria()

linhavida_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = strong("Reconstituição de Trajetórias – Linha da Vida"),
        width = 12,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        p("Veja no gráfico abaixo a linha da vida de cada uma das mulheres identificadas nos bancos de dados. Considere que cada linha corresponde a uma mulher e em cada ícone é possível verificar por qual sistema de saúde ela passou."),
        p(HTML('<b>Como usar o gráfico interativo</b>')),
        tags$ul(
          tags$li(HTML("<strong>Destacar informações de uma mulher específica:</strong> Para visualizar detalhes sobre uma mulher que sofreu violência, basta clicar em um dos pontos que representam um registro dela em um dos sistemas de informação.")),
          tags$li(HTML("<strong>Desmarcar a seleção:</strong> Para desmarcar a mulher selecionada e retornar à visão geral, clique duas vezes fora do ponto selecionado.")),
          tags$li(HTML("<strong>Aplicar filtros:</strong> Para visualizar recortes específicos dos dados, utilize os filtros disponíveis. Selecione as características desejadas e o gráfico será atualizado automaticamente para exibir apenas as mulheres que correspondem aos critérios selecionados."))
        ),
        fluidRow(
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_raca"),
                               label = strong("Raça/cor"),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "Ignorada"),
                               selected = c("Branca", "Preta", "Parda"))
                 )),
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_banco"),
                               label = strong("Banco de dados"),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c("SINAN Violências" = "SINAN_VIOL",
                                           "SIM", "SIH",
                                           "SINAN Intoxicação Exógena" = "SINAN_IEXO"),
                               selected = c("SIM"))
                 )),
          column(4,
                 wellPanel(
                   pickerInput(
                     inputId = ns("filtro_idade"),
                     label = strong("Faixa Etária"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       noneSelectedText = "Nenhuma seleção"
                     ),
                     choices = c("<1", "01-04", "05-09", "10-19",
                                 "20-29", "30-39", "40-49",
                                 "50-59", "60-69", "70-79", 
                                 "80+"),
                     selected = c("<1", "01-04", "05-09", "10-19",
                                  "20-29", "30-39", "40-49",
                                  "50-59", "60-69", "70-79", 
                                  "80+")
                   )
                 ))
        ),
        fluidRow(
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_violencias2"),
                               label = "Tipo de Violência",
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c(
                                 "Violência física" = "fl_viol_fisic", 
                                 "Violência psicológica" = "fl_viol_psico", 
                                 "Tortura" = "fl_viol_tort",
                                 "Violência sexual" = "fl_viol_sexu",
                                 "Tráfico de humanos" = "fl_viol_traf",
                                 "Violência financeira" = "fl_viol_finan",
                                 "Negligência" = "fl_viol_negli",
                                 "Trabalho infantil" = "fl_viol_infan",
                                 "Intervenção legal" = "fl_viol_legal",
                                 "Outras violências" = "fl_viol_outr"
                               ),
                               selected = NULL)
                 )),
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_autoprovocada"),
                               label = strong("Violência auto provocada"),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c("Sim" = 1, "Não" = 0),
                               selected = c(1, 0))
                 )),
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_obitos"),
                               label = "Óbitos",
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c("Sim" = 1, "Não" = 0),
                               selected = c(1, 0))
                 ))
      
          ),
        
        div(
          HTML("<b>Legenda:</b>"),
          span(style = "color:#0099D6; font-size: 20px;", HTML("&#9670;")), " SINAN Intoxicação Exogena       ",
          #span(style = "color:#3bd80d; font-size: 23px;", HTML("&#9679;")), " Esus APS       ",
          span(style = "color:#FFC73B; font-size: 17px;", HTML("&#9650;")), " SIH       ",
          span(style = "color:#121E87; font-size: 17px;", HTML("&#9632;")), " SIM       ",
          span(style = "color:#ff5054; font-size: 12px;", HTML("&#10060;")), " SINAN Violências       "
        ),
        fluidRow(
          column(
            width = 8,
            plotlyOutput(ns("linha_vida_geral_"), height = "600px")
          ),
          column(
            width = 4,
            div(
              style = "height: 520px;",
              uiOutput(ns("selected_point_info"))
            )
          )
        ),
        p(''),
        p("Após o linkage, para cada banco de dados, foi selecionada a data mais importante de cada evento: no SINAN, a data de notificação; no SIH, a data da internação; nos Boletins de Ocorrência, a data do BO; no SIM, a data do óbito; e nos Boletins de Ocorrência Letais, a data do óbito. A partir dessas datas, foi possível construir a linha da vida das mulheres que tiveram notificação de violência no SINAN, permitindo um acompanhamento detalhado dos eventos ao longo do tempo.")
      )
    )
  )
}

linhavida_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns  # Namespacing
      
      selected_point <- reactiveVal(NULL)
      
      # Função para filtrar bancos com base nas colunas FL_
      filtrar_bancos <- function(df, bancos_selecionados) {
        if (length(bancos_selecionados) > 0) {
          condicoes <- list()
          if ("SINAN_VIOL" %in% bancos_selecionados) {
            condicoes <- append(condicoes, list(df$FL_SINAN_VIOL == 1))
          }
          if ("SIM" %in% bancos_selecionados) {
            condicoes <- append(condicoes, list(df$FL_SIM == 1))
          }
          if ("SIH" %in% bancos_selecionados) {
            condicoes <- append(condicoes, list(df$FL_SIH == 1))
          }
          if ("SINAN_IEXO" %in% bancos_selecionados) {
            condicoes <- append(condicoes, list(df$FL_SINAN_IEXO == 1))
          }
          if ("Boletins de Ocorrência Letais" %in% bancos_selecionados) {
            condicoes <- append(condicoes, list(df$FL_SESAP_OB == 1))
          }
          df <- df %>% filter(Reduce(`|`, condicoes))
        }
        df
      }
      
      # Reactive para filtrar dados
      df_filtrado <- reactive({
        df_filtrado <- df_linha_vida
        
        # Filtro de violências
        if (!is.null(input$filtro_violencias2) && length(input$filtro_violencias2) > 0) {
          df_filtrado <- df_filtrado %>%
            filter_at(vars(all_of(input$filtro_violencias2)), any_vars(. == 1))
        }
        
        # Aplicar outros filtros
        df_filtrado <- df_filtrado %>%
          filter(
            ds_raca_padronizada %in% input$filtro_raca,
            faixa_etaria %in% input$filtro_idade,
            FL_SIM %in% as.numeric(input$filtro_obitos)
          )
        
        # Novo filtro: Violência auto provocada
        if (!is.null(input$filtro_autoprovocada) && length(input$filtro_autoprovocada) > 0) {
          df_filtrado <- df_filtrado %>%
            filter(fl_les_autop %in% as.numeric(input$filtro_autoprovocada))
        }
        
        df_filtrado
      })
      
      # Definir cores e formas padronizadas
      colors_banco <- c(
        "SINAN_VIOL" = "#FF5054",
        "SIM" = "#121E87",
        "SIH" = "#FFC73B",
        "SINAN_IEXO" = "#0099D6",
        "Boletins de Ocorrência Letais" = "#3bd80d"
      )
      
      shapes_banco <- c(
        "SINAN_VIOL" = 4,
        "SIM" = 15,
        "SIH" = 17,
        "SINAN_IEXO" = 18,
        "Boletins de Ocorrência Letais" = 19
      )
      
      # Renderizar o gráfico
      output$linha_vida_geral_ <- renderPlotly({
        
        req(input$filtro_banco)
        
        selected_point_data <- selected_point()
        
        df_aux <- df_filtrado() %>%
          filter(!is.na(id_pareamento)) %>%
          filtrar_bancos(input$filtro_banco) %>%
          arrange(id_pareamento, dt_comum) %>%
          mutate(par_reduzido_1 = as.numeric(factor(id_pareamento)) * 20)
        
        ca <- df_aux %>%
          ggplot(aes(x = dt_comum, y = par_reduzido_1, text = id_pareamento))
        
        if (is.null(selected_point_data)) {
          ca <- ca +
            geom_line(aes(group = par_reduzido_1), color = 'lightgray', size = 0.15) +
            geom_point(aes(color = banco, shape = banco), size = 0.9) + 
            scale_color_manual(values = colors_banco) +
            scale_shape_manual(values = shapes_banco)
        } else {
          ca <- ca +
            geom_line(aes(group = par_reduzido_1, color = ifelse(par_reduzido_1 == selected_point_data$y, "black", "lightgray")), size = 0.15) +
            geom_point(aes(color = banco, shape = banco, alpha = ifelse(par_reduzido_1 == selected_point_data$y, 1, 0.65)), size = 0.9) +
            scale_color_manual(values = c(colors_banco, "black" = "black", "lightgray" = "lightgray")) +
            scale_shape_manual(values = shapes_banco)
        }
        
        ca <- ca +
          labs(x = "Ano do evento", y = "") +
          theme_minimal() +
          theme( panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.title.y = element_blank()) +
          scale_x_date(date_labels = "%Y")
        
        # Definir o source como "A" para o plotly e registrar eventos
        p <- ggplotly(ca, tooltip = "text", source = "A") %>%
          layout(
            showlegend = FALSE,
            hoverlabel = list(
              bgcolor = "#FAF4F0",
              font = list(color = "black")
            )
          )
        
        # Registrar eventos no plotly
        p <- event_register(p, 'plotly_click')
        p <- event_register(p, 'plotly_doubleclick')
        
        p
      })
      
      # Renderizar detalhes do ponto selecionado
      output$selected_point_info <- renderUI({
        point_data <- selected_point()
        
        if (is.null(point_data)) {
          box(
            title = strong("Detalhes do Evento Selecionado"),
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            collapsible = FALSE,
            style = "height: 520px; overflow-y: auto;",
            p(''),
            p(''),
            h5("Ao lado, cada linha representa a história de uma mulher com violência registrada no SINAN. Escolha uma e veja o que aconteceu.")
          )
        } else {
          req(input$filtro_banco)
          
          df_aux <- df_filtrado() %>%
            filter(!is.na(id_pareamento)) %>%
            filtrar_bancos(input$filtro_banco) %>%
            arrange(id_pareamento, dt_comum) %>%
            mutate(par_reduzido_1 = as.numeric(factor(id_pareamento)) * 20)
          
          selected_data <- df_aux %>%
            filter(par_reduzido_1 == point_data$y)
          
          box(
            title = strong("Detalhes do Evento Selecionado"),
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            collapsible = FALSE,
            style = "height: 520px; overflow-y: auto;",
            p(HTML(paste0("<b>Demográfico:</b><br><b>Raça/cor:</b> ", selected_data$ds_raca_padronizada[1], 
                          "<br> <b>Idade: </b>", selected_data$nu_idade_anos[1],
                          "<br>", selected_data$texto_final[1]))),
            div(style = "text-align: right;", actionButton(ns("ok_button"), "Finalizar leitura"))
          )
        }
      })
      
      # Eventos para seleção de pontos
      observeEvent(event_data("plotly_click", source = "A"), {
        selected_point(event_data("plotly_click", source = "A"))
      })
      
      observeEvent(event_data("plotly_doubleclick", source = "A"), {
        selected_point(NULL)
      })
      
      observeEvent(input$ok_button, {
        selected_point(NULL)
      })
      
    }
  )
}