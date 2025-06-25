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
library(shinyjs) # added shinyjs

load('dados/base_linkage2.RData')


base_linkage<- base_linkage |> 
    plyr::mutate(
      
      linkada = ifelse(!is.na(id_pareamento), 1,0))

base_linkage <- base_linkage |> 
  dplyr::mutate(
    nu_idade_anos_inicial = as.numeric(nu_idade_anos_inicial),
    faixa_etaria_padrao = dplyr::case_when(
      nu_idade_anos_inicial < 1 ~ "<1",
      nu_idade_anos_inicial >= 1 & nu_idade_anos_inicial <= 4 ~ "01-04",
      nu_idade_anos_inicial >= 5 & nu_idade_anos_inicial <= 9 ~ "05-09", 
      nu_idade_anos_inicial >= 10 & nu_idade_anos_inicial <= 19 ~ "10-19", 
      nu_idade_anos_inicial >= 20 & nu_idade_anos_inicial <= 29 ~ "20-29", 
      nu_idade_anos_inicial >= 30 & nu_idade_anos_inicial <= 39 ~ "30-39", 
      nu_idade_anos_inicial >= 40 & nu_idade_anos_inicial <= 49 ~ "40-49", 
      nu_idade_anos_inicial >= 50 & nu_idade_anos_inicial <= 59 ~ "50-59", 
      nu_idade_anos_inicial >= 60 & nu_idade_anos_inicial <= 69 ~ "60-69", 
      nu_idade_anos_inicial >= 70 & nu_idade_anos_inicial <= 79 ~ "70-79", 
      nu_idade_anos_inicial >= 80 ~ "80+", 
      TRUE ~ as.character(nu_idade_anos_inicial)
    )
  )


# base de óbitos
df_obitos <- base_linkage |> 
  filter(
    banco == "SIM"
  )

load('dados/icd_map_ufmg.Rdata')

icd_map <- icd_map |> 
  rename(
    cd_causabas = icd_code_4,
    causa_resumida = CIDBR_RESUMIDO_EXTERNAS
  ) |> 
  select(
    cd_causabas,
    causa_resumida
  )

df_obitos <- df_obitos |> 
  left_join(icd_map, by = c("cd_causabas")) |> 
  mutate(
    causa_resumida = case_when(
      causa_resumida == "Ignorado" ~ "Ignorado",
      TRUE ~ causa_resumida
    )
  )# |> 
  #mutate(
    #causa_resumida = str_to_title(causa_resumida)
  #) |> 
  
# Base de linkage
df_linkada <- base_linkage |> select(
  id_pareamento, id_pessoa,
  ds_raca, 
  faixa_etaria_padrao, 
  banco,
  fl_viol_fisic,
  fl_viol_psico, 
  fl_viol_tort,
  fl_viol_sexu,
  fl_viol_traf,
  fl_viol_finan,
  fl_viol_negli,
  fl_viol_infan,
  fl_viol_legal,
  fl_viol_outr,
  #FL_CAD_UNICO_PESSOA,
  sg_sexo,
  linkada
)

rm(base_linkage)

linkage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML(".info-box .info-box-number { font-size: 32px; }"))
    ),
    fluidRow(
      box(width = 12,
          title = strong("Filtros"),
          collapsible = FALSE,
          fluidRow(
            column(4,
                   wellPanel(
                     pickerInput(
                       inputId = ns("filtro_idade"),
                       label = "Faixa Etária",
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE),
                       choices = c("<1", "01-04","05-09", "10-19",
                                   "20-29", "30-39", "40-49",
                                   "50-59", "60-69", "70-79",
                                   "80+"),
                       selected = c("<1", "01-04", "05-09", "10-19",
                                    "20-29", "30-39", "40-49",
                                    "50-59", "60-69", "70-79",
                                    "80+")
                     )
                   )
            ),
            column(4,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_raca"),
                                 label = "Raça/cor",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
                                 choices = c(
                                   "Branca",
                                   "Preta",
                                   "Parda",
                                   "Indígena",
                                   "Amarela",
                                   "Ignorada"
                                 ),
                                 selected = c("Branca",
                                              "Preta",
                                              "Parda",
                                              "Indígena",
                                              "Amarela",
                                              "Ignorada")
                     )
                   )
            ),
            column(4,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_banco"),
                                 label = "Banco de dados",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
                                 choices = c("SIM",
                                             "SIH",
                                             "SINAN Violências" = "SINAN_VIOL",
                                             "SINAN Intox Exogena"="SINAN_IEXO"),
                                 selected = c("SIM", "SIH", "SINAN_VIOL", "SINAN_IEXO")
                     )
                   )
            )
          ),
          fluidRow(
            column(8,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_violencias"),
                                 label = "Tipo de Violência",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
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
                   )
            ),
            column(4,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_esus"), #filtro_cadunico
                                 label = "Registro no ESUS APS",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
                                 choices = c("Sim" = 1, "Não" = 0),
                                 selected = c(1, 0)
                     )
                   )
            )
          )
      )
    ),
    fluidRow(
      column(6,
             tags$div(
               id = ns("box_num_registros"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("num_registros"), width = 12)
             )
      ),
      column(6,
             tags$div(
               id = ns("box_reg_pareado"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("reg_pareado"), width = 12)
             )
      )
    ),
    fluidRow(
      column(6,
             tags$div(
               id = ns("box_mulheres"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("num_mulheres"), width = 12)
             )
      ),
      column(6,
             tags$div(
               id = ns("box_mulheres_pareadas"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("mulheres_pareadas"), width = 12)
             )
      )
    ),
    fluidRow(
      box(
        title = strong('Faixa etária'),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("faixa_etaria_graf")),
        downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")
      ),
      box(
        title = strong('Raça/cor'),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("raca_cor_graf")),
        downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela")
      )
    ),
    fluidRow(
      box(
        title = strong("Comparação das proporções das causas de óbito entre mulheres com notificação de violência e sem notificação de violência (2019 - 2021)"),
        width = 12,
        height = "700px", # <--- Added height argument
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("causas_obito_linkage"), height = "100%") # Consider adding height = "100%" or similar here if needed
      )
    )
  )
}

# ----- NO CHANGES NEEDED IN linkage_server -----
linkage_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns  # Get namespace
      
      # Initialize selected box
      selected_box <- reactiveVal("mulheres")  # Default selection
      
      # Capture clicks
      shinyjs::onclick("box_mulheres", {
        selected_box("mulheres")
      })
      
      shinyjs::onclick("box_mulheres_pareadas", {
        selected_box("mulheres_pareadas")
      })
      
      # Output Número de registros ----
      output$num_registros <- renderValueBox({
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",  # Define o tamanho da fonte aqui
            df_linkada |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco
              ) |>
              nrow()
          ),
          subtitle = "Total de registros processados",
          color = "danger",
          icon = icon("address-card")
        )
      })
      
      output$reg_pareado <- renderValueBox({
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",
            df_linkada |>
              dplyr::filter(
                !is.na(id_pareamento),
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco
              ) |>
              nrow()
          ),
          subtitle = "Registros processados de mulheres identificadas no linkage",
          color = "danger",
          icon = icon("code-compare")
        )
      })
      
      output$num_mulheres <- renderValueBox({
        color <- ifelse(selected_box() == "mulheres", "primary", "danger")
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",
            df_linkada |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco
              ) |>
              distinct(id_pessoa) |>
              nrow()
          ),
          subtitle = "Total de mulheres distintas identificadas",
          icon = icon("venus"),
          color = color
        )
      })
      
      output$mulheres_pareadas <- renderValueBox({
        color <- ifelse(selected_box() == "mulheres_pareadas", "primary", "danger")
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",
            df_linkada |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco
              ) |>
              distinct(id_pareamento) |>
              nrow()
          ),
          subtitle = "Número de mulheres identificadas no linkage",
          icon = icon("link"),
          color = color
        )
      })
      
      # Faixa etaria grafico ----
      output$faixa_etaria_graf <- renderPlotly({
        # Determine if 'Tipo de Violência' filter should be applied
        if (selected_box() == "mulheres_pareadas") {
          filtro_viol <- input$filtro_violencias
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_linkada %>%
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_linkada
          }
          # Apply linkage filter
          df_filtrado <- df_filtrado %>% filter(linkada == 1)
        } else {
          # Ignore 'Tipo de Violência' filter
          df_filtrado <- df_linkada
          # Apply linkage filter
          df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
        }
        
        a <-  df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco,
            #FL_CAD_UNICO_PESSOA %in% input$filtro_cadunico
          ) |>
          distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          tab_1(faixa_etaria_padrao) |>
          filter(faixa_etaria_padrao != "Total") |>
          mutate(
            faixa_etaria_padrao = case_when(faixa_etaria_padrao == "IGNORADA"~"Ignorada",
                                            TRUE ~ faixa_etaria_padrao),
            cor = ifelse(faixa_etaria_padrao == "Ignorada", "#9ba2cb", "#121E87")) |>
          ggplot(aes(
            x = faixa_etaria_padrao, y = `%`, fill = cor,
            text = paste("Faixa etária:", faixa_etaria_padrao, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
          )
          ) +
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Faixa etária", y = "Proporção") +
          theme_minimal() +
          theme(legend.position = "none") +
          theme(axis.text.x = element_text( hjust = 1)) # angle = 45,
        
        ggplotly(a, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
      })
      
      # Faixa etária download
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          if (selected_box() == "mulheres_pareadas") {
            filtro_viol <- input$filtro_violencias
            if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
              df_filtrado <- df_linkada %>%
                filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
            } else {
              df_filtrado <- df_linkada
            }
            df_filtrado <- df_filtrado %>% filter(linkada == 1)
          } else {
            df_filtrado <- df_linkada
            df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
          }
          
          tabela_fxetaria <-  df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco,
              #FL_CAD_UNICO_PESSOA %in% input$filtro_cadunico
            ) |>
            distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          write.csv(tabela_fxetaria, file, row.names = FALSE)
        }
      )
      
      # Raça/cor ----
      output$raca_cor_graf <- renderPlotly({
        if (selected_box() == "mulheres_pareadas") {
          filtro_viol <- input$filtro_violencias
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_linkada %>%
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_linkada
          }
          df_filtrado <- df_filtrado %>% filter(linkada == 1)
        } else {
          df_filtrado <- df_linkada
          df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
        }
        
        # Prepare os dados
        dados_preparados <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco,
            #FL_CAD_UNICO_PESSOA %in% input$filtro_cadunico
          ) |>
          distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          mutate(ds_raca = str_to_title(ds_raca)) |>
          tab_1(ds_raca) |>
          filter(ds_raca != "Total")
        
        # Assegura que "Ignorada" seja um dos níveis e esteja no final
        racas_ordenadas <- unique(dados_preparados$ds_raca)
        racas_ordenadas <- racas_ordenadas[racas_ordenadas != "Ignorada"]
        racas_ordenadas <- c(racas_ordenadas, "Ignorada") # Garante que Ignorada venha por último
        racas_ordenadas <- rev(racas_ordenadas) # Inverte a ordem
        
        dados_preparados$ds_raca <- factor(dados_preparados$ds_raca, levels = racas_ordenadas)
        
        # Cria um vetor de cores
        cores <- setNames(rep("#121E87", length(racas_ordenadas)), racas_ordenadas)
        cores["Ignorada"] <- "#9ba2cb" # Define explicitamente a cor para Ignorada
        
        # Cria o gráfico
        b <- ggplot(dados_preparados, aes(
          x = ds_raca, y = `%`, fill = ds_raca,
          text = paste("Raça/Cor:", ds_raca, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
        )
        )  +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = cores) +
          labs(
            x = "Raça/cor",
            y = "Proporção"
          ) +
          theme_minimal() +
          coord_flip() + # Faz com que as barras fiquem deitadas
          theme(legend.position = "none") # Remove a barra de legenda
        
        # Converter o gráfico ggplot para plotly
        ggplotly(b, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
      })
      
      # Raça/cor download
      output$download_tab_raca_cor <- downloadHandler(
        filename = function() {
          paste("dados-raca-cor-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          if (selected_box() == "mulheres_pareadas") {
            filtro_viol <- input$filtro_violencias
            if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
              df_filtrado <- df_linkada %>%
                filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
            } else {
              df_filtrado <- df_linkada
            }
            df_filtrado <- df_filtrado %>% filter(linkada == 1)
          } else {
            df_filtrado <- df_linkada
            df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
          }
          
          tabela_raca <-  df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco,
              #FL_CAD_UNICO_PESSOA %in% input$filtro_cadunico
            ) |>
            distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(ds_raca)
          
          write.csv(tabela_raca, file, row.names = FALSE)
        }
      )
      
      # Linha da vida gráfico ----
      output$causas_obito_linkage <- renderPlotly({
        
        # Se não houver seleção, retorna o dataframe completo
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          # Cria a lógica do filtro com base nas variáveis selecionadas
          df_filtrado <- df_obitos |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))  # Filtra quando ao menos uma das variáveis for 1
        } else {
          df_filtrado <- df_obitos
        }
        
        
        # Preparando o dataframe 'a'
        a <- df_obitos |>
          filter(banco == "SIM",
                 # faixa_etaria_padrao %in% input$filtro_idade, # Filters removed as per original code logic for this plot
                 # ds_raca %in% input$filtro_raca
          ) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          rename("Óbitos sem notificação" = `%`,
                 "Óbitos sem notificação (n)" = n) |>
          arrange(desc(`Óbitos sem notificação`))  # Ordena por 'Óbitos sem notificação'
        
        # Preparando o dataframe 'b'
        b <- df_obitos |>
          filter(banco == "SIM", FL_SINAN_VIOL == 1,
                 #faixa_etaria_padrao %in% input$filtro_idade, # Filters removed as per original code logic for this plot
                 #ds_raca %in% input$filtro_raca
          ) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          rename("Óbitos com notificação" = `%`,
                 "Óbitos com notificação(n)" = n)
        
        # Combinando 'a' e 'b' com uma junção à esquerda
        c <- left_join(b, a, by = "causa_resumida") |>
          pivot_longer(
            cols = c("Óbitos sem notificação", "Óbitos com notificação"),
            names_to = "nivel",
            values_to = "valor")
        
        # Reordenando o fator 'causa_resumida' baseado nos valores específicos para "Óbitos COM notificação"
        c <- c |>
          group_by(causa_resumida) |>
          summarise(max_valor = max(valor[nivel == "Óbitos com notificação"]), .groups = 'drop') |>
          arrange(desc(-max_valor)) |>
          left_join(c, by = "causa_resumida") |>
          mutate(causa_resumida = factor(causa_resumida, levels = unique(causa_resumida)))
        
        # Agora, criando o gráfico com a ordenação correta
        min_valor <- min(c$valor, na.rm = TRUE)
        max_valor <- max(c$valor, na.rm = TRUE)
        
        bolha_c <- ggplot(c, aes(x = nivel,
                                 y = causa_resumida,
                                 size = valor,
                                 color = nivel,
                                 text = paste("Para", tolower(nivel), ", \n",round(valor, 2), "% foram por", causa_resumida))) +
          geom_point(alpha = 0.9) +
          geom_text(aes(label = paste0(round(valor, 2), "%")),
                    nudge_x = 0.2,  # Nudge para mover o texto para o lado direito
                    hjust = 0,       # Alinha o texto à esquerda do ponto de ancoragem
                    fontface = "bold", # Estilo da fonte
                    size = 3.5) +    # Tamanho da fonte
          scale_size_continuous(range = c(1, 9.2),
                                limits = c(min_valor, max_valor),
                                breaks = pretty(c$valor, n = 5)) +
          labs(size = "Legenda",  # Alterando "valor" para "Legenda"
               x = '',
               y = '') +
          theme_test() +
          theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
          guides(size = guide_legend(override.aes = list(alpha = 1)),
                 color = guide_legend(title = "")) +  # Alterando "nivel" para "Legenda"
          scale_color_manual(values = c("#FF5054", "#121E87"))
        
        plotly_bolha_c <- ggplotly(bolha_c, tooltip = c("text", "label"))
        
        plotly_bolha_c <- plotly_bolha_c %>%
          layout(
            hoverlabel = list(
              bgcolor = "#FAF4F0", # Cor de fundo do tooltip
              font = list(color = "black") # Cor do texto
            )
          )
        
        plotly_bolha_c
      })
    }
  )
}
