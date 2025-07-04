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

df_linha_vida <- df_linha_vida |> 
  group_by(id_pessoa) |> 
  mutate(
    so_sinan = ifelse(all(banco == "SINAN_VIOL"), 1, 0)
  ) |> 
  ungroup()

# Para ver violencia sexual e parto
#load("dados/temp_teste_vida.RData")

df_linha_vida <- df_linha_vida |> 
  group_by(id_pessoa) |> 
  mutate(idade_minima = min(nu_idade_anos, na.rm = TRUE)) |> 
  ungroup() |> 
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      nu_idade_anos < 1 ~ "<1",
      nu_idade_anos >= 1 & nu_idade_anos <= 4 ~ "01-04",
      nu_idade_anos >= 5 & nu_idade_anos <= 9 ~ "05-09", 
      nu_idade_anos >= 10 & nu_idade_anos <= 14 ~ "10-14", 
      nu_idade_anos >= 15 & nu_idade_anos <= 19 ~ "15-19", 
      nu_idade_anos >= 20 & nu_idade_anos <= 29 ~ "20-29", 
      nu_idade_anos >= 30 & nu_idade_anos <= 39 ~ "30-39", 
      nu_idade_anos >= 40 & nu_idade_anos <= 49 ~ "40-49", 
      nu_idade_anos >= 50 & nu_idade_anos <= 59 ~ "50-59", 
      nu_idade_anos >= 60 & nu_idade_anos <= 69 ~ "60-69", 
      nu_idade_anos >= 70 & nu_idade_anos <= 79 ~ "70-79", 
      nu_idade_anos >= 80 ~ "80+", 
      TRUE ~ "Ignorada"
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
          tags$li(HTML("<strong>Aplicar filtros:</strong> Para visualizar recortes específicos dos dados, utilize os filtros disponíveis. Selecione as características desejadas e o gráfico será atualizado automaticamente para exibir apenas as mulheres que correspondem aos critérios selecionados.")),
          tags$li(HTML("<strong>Filtro <em>Exclusivo SINAN Violências</em>:</strong> Ao selecionar essa opção no filtro de bancos de dados, o gráfico apresentará <u>somente</u> as mulheres cujos registros estão exclusivamente no SINAN Violências. Você ainda pode combinar esse recorte com os demais filtros (raça/cor, faixa etária, tipo de violência, etc.). Se, depois de marcado <em>Exclusivo SINAN Violências</em>, você marcar qualquer outro banco (SIM, SIH, SINAN Intoxicação Exógena, etc.), o modo exclusivo é desativado automaticamente e o gráfico volta a mostrar os resultados de todos os bancos selecionados."))
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
                              # options = list(
                              #   `actions-box` = TRUE,
                              #   noneSelectedText = "Nenhuma seleção"
                              # ),
                               choices = c(
                                 "Exclusivo SINAN Violências"     = "EXCLUSIVO_SINAN_VIOL",
                                  "SINAN Violências" = "SINAN_VIOL",
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
                     choices = c("<1", "01-04", "05-09", "10-14", "15-19",
                                 "20-29", "30-39", "40-49",
                                 "50-59", "60-69", "70-79", "80+", "Ignorada"),
                     selected = c("<1", "01-04", "05-09", "10-14", "15-19",
                                  "20-29", "30-39", "40-49",
                                  "50-59", "60-69", "70-79", "80+", "Ignorada")
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
      ns <- session$ns
      selected_point <- reactiveVal(NULL)
      
      # ── Exclusividade “Exclusivo SINAN Violências” ─────────
      prev_sel <- reactiveVal(character())
      observeEvent(input$filtro_banco, ignoreNULL = FALSE, {
        sel  <- input$filtro_banco
        prev <- prev_sel()
        
        if ("EXCLUSIVO_SINAN_VIOL" %in% sel && length(sel) > 1) {
          if ("EXCLUSIVO_SINAN_VIOL" %in% prev) {
            updatePickerInput(session, "filtro_banco",
                              selected = setdiff(sel, "EXCLUSIVO_SINAN_VIOL"))
          } else {
            updatePickerInput(session, "filtro_banco", selected = "EXCLUSIVO_SINAN_VIOL")
          }
        }
        prev_sel(sel)
      })
      
      # ── Função de filtragem de bancos ─────────────────────
      filtrar_bancos <- function(df, bancos_sel) {
        if ("EXCLUSIVO_SINAN_VIOL" %in% bancos_sel) return(filter(df, so_sinan == 1))
        if (length(bancos_sel) > 0) {
          cond <- list()
          if ("SINAN_VIOL" %in% bancos_sel)  cond <- append(cond, list(df$FL_SINAN_VIOL == 1))
          if ("SIM"        %in% bancos_sel)  cond <- append(cond, list(df$FL_SIM        == 1))
          if ("SIH"        %in% bancos_sel)  cond <- append(cond, list(df$FL_SIH        == 1))
          if ("SINAN_IEXO" %in% bancos_sel)  cond <- append(cond, list(df$FL_SINAN_IEXO == 1))
          if ("Boletins de Ocorrência Letais" %in% bancos_sel)
            cond <- append(cond, list(df$FL_SESAP_OB == 1))
          df <- filter(df, Reduce(`|`, cond))
        }
        df
      }
      
      # ── Dados reativos (aplicação de filtros) ─────────────
      df_filtrado <- reactive({
        df_tmp <- df_linha_vida
        
        if (!is.null(input$filtro_violencias2) && length(input$filtro_violencias2) > 0)
          df_tmp <- filter_at(df_tmp, vars(all_of(input$filtro_violencias2)), any_vars(. == 1))
        
        df_tmp <- filter(df_tmp,
                         ds_raca_padronizada %in% input$filtro_raca,
                         faixa_etaria         %in% input$filtro_idade,
                         FL_SIM               %in% as.numeric(input$filtro_obitos))
        
        if (!is.null(input$filtro_autoprovocada) && length(input$filtro_autoprovocada) > 0)
          df_tmp <- filter(df_tmp, fl_les_autop %in% as.numeric(input$filtro_autoprovocada))
        
        df_tmp
      })
      
      # ── Escalas de cor/forma ──────────────────────────────
      colors_banco <- c("SINAN_VIOL" = "#FF5054",
                        "SIM"        = "#121E87",
                        "SIH"        = "#FFC73B",
                        "SINAN_IEXO" = "#0099D6",
                        "Boletins de Ocorrência Letais" = "#3bd80d")
      
      shapes_banco <- c("SINAN_VIOL" = 4,
                        "SIM"        = 15,
                        "SIH"        = 17,
                        "SINAN_IEXO" = 18,
                        "Boletins de Ocorrência Letais" = 19)
      
      # ── Gráfico principal ─────────────────────────────────
      output$linha_vida_geral_ <- renderPlotly({
        req(input$filtro_banco)
        
        df_aux <- df_filtrado() |>
          filter(!is.na(id_pareamento)) |>
          filtrar_bancos(input$filtro_banco) |>
          arrange(id_pareamento, dt_comum) |>
          mutate(par_reduzido_1 = as.numeric(factor(id_pareamento)) * 20)
        
        sel_pt <- selected_point()
        
        g <- ggplot(df_aux, aes(x = dt_comum, y = par_reduzido_1, text = id_pareamento))
        
        if (is.null(sel_pt)) {
          g <- g +
            geom_line(aes(group = par_reduzido_1),
                      color = "lightgray", size = 0.15) +
            geom_point(aes(color = banco, shape = banco), size = 0.9)
        } else {
          g <- g +
            geom_line(aes(group = par_reduzido_1,
                          color = ifelse(par_reduzido_1 == sel_pt$y,
                                         "black", "lightgray")),
                      size = 0.15) +
            geom_point(aes(color = banco, shape = banco,
                           alpha = ifelse(par_reduzido_1 == sel_pt$y, 1, 0.65)),
                       size = 0.9)
        }
        
        # ── Escala de cores única (sem avisos) ──────────────
        valores_cor <- colors_banco
        if (!is.null(sel_pt))
          valores_cor <- c(valores_cor, "black" = "black", "lightgray" = "lightgray")
        
        g <- g +
          scale_color_manual(values = valores_cor) +
          scale_shape_manual(values = shapes_banco) +
          labs(x = "Ano do evento", y = "") +
          theme_minimal() +
          theme(panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.y        = element_blank(),
                axis.ticks.y       = element_blank()) +
          scale_x_date(date_labels = "%Y")
        
        # ── Conversão para plotly ───────────────────────────
        p <- ggplotly(g, tooltip = "text", source = "A") |>
          layout(showlegend = FALSE,
                 hoverlabel = list(bgcolor = "#FAF4F0",
                                   font = list(color = "black")))
        
        # Registro explícito dos eventos
        p <- event_register(p, "plotly_click")
        p <- event_register(p, "plotly_doubleclick")
        p
      })
      
      # ── Caixa de detalhes ─────────────────────────────────
      output$selected_point_info <- renderUI({
        pt <- selected_point()
        
        if (is.null(pt)) {
          box(
            title       = strong("Detalhes do Evento Selecionado"),
            width       = 12,
            status      = "danger",
            solidHeader = TRUE,
            style       = "height: 520px; overflow-y: auto;",
            h5("Ao lado, cada linha representa a história ...")
          )
        } else {
          df_sel <- df_filtrado() |>
            filter(!is.na(id_pareamento)) |>
            filtrar_bancos(input$filtro_banco) |>
            arrange(id_pareamento, dt_comum) |>
            mutate(par_reduzido_1 = as.numeric(factor(id_pareamento)) * 20) |>
            filter(par_reduzido_1 == pt$y)
          
          box(
            title       = strong("Detalhes do Evento Selecionado"),
            width       = 12,
            status      = "danger",
            solidHeader = TRUE,
            style       = "height: 520px; overflow-y: auto;",
            p(HTML(paste0("<b>Demográfico:</b><br><b>Raça/cor:</b> ",
                          df_sel$ds_raca_padronizada[1],
                          "<br><b>Idade:</b> ", df_sel$nu_idade_anos[1],
                          "<br>", df_sel$texto_final[1]))),
            div(style = "text-align: right;",
                actionButton(ns("ok_button"), "Finalizar leitura"))
          )
        }
      })
      
      # ── Eventos de seleção (sem avisos plotly) ────────────
      observeEvent(
        suppressWarnings(event_data("plotly_click", source = "A")),
        {
          sel <- event_data("plotly_click", source = "A")
          if (!is.null(sel)) selected_point(sel)
        }
      )
      
      observeEvent(
        suppressWarnings(event_data("plotly_doubleclick", source = "A")),
        selected_point(NULL)
      )
      
      observeEvent(input$ok_button, selected_point(NULL))
    }
  )
}