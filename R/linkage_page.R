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
library(shinyjs)
library(stringr)

# -----------------
# Bases e pré‑processamento
# -----------------
load("dados/base_linkage4.RData")   # objeto: base_linkage

# Base principal de linkage (para as caixas/filtros)
base_linkage <- base_linkage |>
  select(id_pareamento, id_pessoa, ds_raca, faixa_etaria_padrao,
         banco, cd_causabas,
         fl_viol_fisic, fl_viol_psico, fl_viol_tort, fl_viol_sexu,
         fl_viol_traf, fl_viol_finan, fl_viol_negli, fl_viol_infan,
         fl_viol_legal, fl_viol_outr, FL_ESUS_APS, FL_SINAN_VIOL, FL_SINAN_IEXO,
         sg_sexo, linkada)

# Base de óbitos (SIM)
df_obitos <- base_linkage |> filter(banco == "SIM")

# Dicionário CID
load("dados/icd_map_ufmg.Rdata")      # objeto: icd_map
icd_map <- icd_map |>
  rename(cd_causabas = icd_code_4,
         causa_resumida = CIDBR_RESUMIDO_EXTERNAS) |>
  select(cd_causabas, causa_resumida) |> 
  mutate(
    causa_resumida = case_when(
      causa_resumida == "Ext W40-W49 \tEnvenenamento, intoxicação por ou exposição a substâncias nocivas" ~ "Ext Outras",
      TRUE ~ causa_resumida
    )
  )

df_obitos <- df_obitos |>
  left_join(icd_map, by = "cd_causabas") |>
  mutate(causa_resumida = case_when(
    causa_resumida == "Ignorado" ~ "Ignorado",
    TRUE                         ~ causa_resumida
  ))

#Grafico comparativo entre bancos
viol_iexo <- base_linkage |>
  filter(banco %in% c("SINAN_VIOL", "SINAN_IEXO")) |> 
  select(banco, FL_SINAN_VIOL, FL_SINAN_IEXO, id_pessoa)

so_viol <- viol_iexo |> 
  filter(FL_SINAN_VIOL == 1, FL_SINAN_IEXO != 1) |> 
  select(id_pessoa) |> 
  distinct() |> 
  nrow()

so_iexo <- viol_iexo |> 
  filter(FL_SINAN_VIOL != 1, FL_SINAN_IEXO == 1) |> 
  select(id_pessoa) |> 
  distinct() |> 
  nrow()

ambos <- viol_iexo |> 
  filter(FL_SINAN_VIOL == 1, FL_SINAN_IEXO == 1) |> 
  select(id_pessoa) |> 
  distinct() |> 
  nrow()


categoria <- c("SINAN Violências", "SINAN Intoxicação Exógena", "Ambos")
quantidade <- c(so_viol, so_iexo, ambos)


# Monta o dataframe
df <- data.frame(
  banco = categoria,
  n = quantidade,
  cor = c("#FF5054", 
          "#FFC73B", 
          "#0099D6")
)



# ================================================================
# UI
# ================================================================
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
                       choices = c("<1", "01-04", "05-09", "10-14", "15-19",
                                   "20-29", "30-39", "40-49",
                                   "50-59", "60-69", "70-79", "80+",  "Ignorada"),
                       selected = c("<1", "01-04", "05-09", "10-14", "15-19",
                                    "20-29", "30-39", "40-49",
                                    "50-59", "60-69", "70-79", "80+", "Ignorada")
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
        title = strong("Comparação das proporções das causas de óbito entre mulheres com notificação de violência e sem notificação de violência (2019 - 2022)"),
        width = 12,
        height = "720px", # <--- Added height argument
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("causas_obito_linkage"), height = "95%"), # Consider adding height = "100%" or similar here if needed
        downloadButton(outputId = ns("download_bolha"), label = "Download da Tabela")
      )
    ),
    fluidRow(
      box(
        title = strong("Número de mulheres no SINAN Violências, SINAN Intoxicação Exógena e ambos sistemas (2019 - 2022)"),
        width = 12,
        #height = "720px", # <--- Added height argument
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("graf_viol_iexo"), height = "100%"), # Consider adding height = "100%" or similar here if needed
        downloadButton(outputId = ns("download_viol_iexo"), label = "Download da Tabela")
      )
    )
  )
}


# ================================================================
# SERVER
# ================================================================
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
            base_linkage |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
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
            base_linkage |>
              dplyr::filter(
                !is.na(id_pareamento),
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
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
            base_linkage |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
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
            base_linkage |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
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
            df_filtrado <- base_linkage %>%
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- base_linkage
          }
          # Apply linkage filter
          df_filtrado <- df_filtrado %>% filter(linkada == 1)
        } else {
          # Ignore 'Tipo de Violência' filter
          df_filtrado <- base_linkage
          # Apply linkage filter
          df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
        }
        
        a <-  df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco,
            FL_ESUS_APS %in% input$filtro_esus
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
          paste("dados-faixa-etaria-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          if (selected_box() == "mulheres_pareadas") {
            filtro_viol <- input$filtro_violencias
            if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
              df_filtrado <- base_linkage %>%
                filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
            } else {
              df_filtrado <- base_linkage
            }
            df_filtrado <- df_filtrado %>% filter(linkada == 1)
          } else {
            df_filtrado <- base_linkage
            df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
          }
          
          tabela_fxetaria <-  df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco,
              FL_ESUS_APS %in% input$filtro_esus
            ) |>
            distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          writexl::write_xlsx(tabela_fxetaria, path = file)
        }
      )
      
      # Raça/cor ----
      output$raca_cor_graf <- renderPlotly({
        if (selected_box() == "mulheres_pareadas") {
          filtro_viol <- input$filtro_violencias
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- base_linkage %>%
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- base_linkage
          }
          df_filtrado <- df_filtrado %>% filter(linkada == 1)
        } else {
          df_filtrado <- base_linkage
          df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
        }
        
        # Prepare os dados
        dados_preparados <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco,
            FL_ESUS_APS %in% input$filtro_esus
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
        file = function() {
          paste("dados-raca-cor-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          if (selected_box() == "mulheres_pareadas") {
            filtro_viol <- input$filtro_violencias
            if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
              df_filtrado <- base_linkage %>%
                filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
            } else {
              df_filtrado <- base_linkage
            }
            df_filtrado <- df_filtrado %>% filter(linkada == 1)
          } else {
            df_filtrado <- base_linkage
            df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
          }
          
          tabela_raca <-  df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco,
              FL_ESUS_APS %in% input$filtro_esus
            ) |>
            distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(ds_raca)
          
          writexl::write_xlsx(tabela_raca, path = file)
        }
      )
      
      # ------ GRÁFICO DE BOLHAS (3 colunas) ------
      output$causas_obito_linkage <- renderPlotly({
        
        df_sim <- df_obitos  
        # Filtragem pelo tipo de violência, se existir filtro
        #f_viol <- input$filtro_violencias
        #df_sim <- if (!is.null(f_viol) && length(f_viol) > 0) {
        #  df_obitos |> filter_at(vars(all_of(f_viol)), any_vars(. == 1))
        #} else {
        #  df_obitos
        #}
        
        # ---- 1) Óbitos com notificação (SINAN VIOL) ----
        a <- df_sim |>
          filter(banco == "SIM", FL_SINAN_VIOL == 1) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          transmute(causa_resumida,
                    nivel = "Óbitos com notificação de violência",
                    valor = `%`,
                    n = n)
        
        # ---- 2) Óbitos – SINAN IEXO ----
        b <- df_sim |>
          filter(banco == "SIM", FL_SINAN_IEXO == 1) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          transmute(causa_resumida,
                    nivel = "Óbitos com notificação de intoxicação exógena",
                    valor = `%`,
                    n = n)
        
        # ---- 3) Óbitos sem notificação ----
        c <- df_sim |>
          filter(banco == "SIM", FL_SINAN_VIOL != 1, FL_SINAN_IEXO != 1) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          transmute(causa_resumida,
                    nivel = "Óbitos sem notificação",
                    valor = `%`,
                    n = n)
        
        # ---- Combina tudo em formato longo ----
        df_bolhas <- bind_rows(a, b, c)
        
        # 1. Todas as causas existentes em QUALQUER nível
        todas_causas <- df_bolhas |> distinct(causa_resumida) |> pull(causa_resumida)
        
        # 2. Causas ordenadas do maior para menor na coluna de "Óbitos com notificação de violência"
        ordem_causas_notif <- a |>
          arrange(desc(valor)) |>
          pull(causa_resumida)
        
        # 3. Causas que NÃO estão presentes nos óbitos com notificação
        causas_restantes <- setdiff(todas_causas, ordem_causas_notif)
        
        # 4. Concatena ordenados + restantes
        ordem_final <- c(ordem_causas_notif, causas_restantes)
        
        # >>>> INVERTE OS NÍVEIS PARA FICAR MAIOR NO TOPO <<<<
        df_bolhas <- df_bolhas |>
          mutate(
            causa_resumida = factor(causa_resumida, levels = rev(ordem_final)),
            nivel = factor(nivel,
                           levels = c("Óbitos com notificação de violência",
                                      "Óbitos com notificação de intoxicação exógena",
                                      "Óbitos sem notificação"))
          )
        
        cores <- c("Óbitos com notificação de violência"  = "#FF5054",
                   "Óbitos com notificação de intoxicação exógena"     = "#FFC73B",
                   "Óbitos sem notificação"  = "#121E87")
        
        g <- ggplot(df_bolhas, aes(x = nivel,
                                   y = causa_resumida,
                                   size = valor,
                                   color = nivel,
                                   text = paste0("Para ", tolower(nivel), ": ",
                                                 round(valor, 2), "%\n",
                                                 "Causa: ", causa_resumida,
                                                 "\nRegistros: ", n))) +
          geom_point(alpha = 0.9) +
          geom_text(aes(label = paste0(round(valor, 2), "%")),
                    nudge_x = 0.25,
                    hjust = 0,
                    fontface = "bold",
                    size = 3.5) +
          scale_size_continuous(range = c(1, 9.2),
                                breaks = pretty(df_bolhas$valor, n = 5)) +
          scale_color_manual(values = cores) +
          labs(x = NULL, y = NULL, size = "Proporção") +
          theme_test() +
          theme(axis.text.x = element_text(hjust = 1, vjust = 1),
                legend.position = "none")
        
        ggplotly(g, tooltip = "text") |>
          layout(hoverlabel = list(bgcolor = "#FAF4F0",
                                   font = list(color = "black")))
      })
      
      
      
      output$download_bolha <- downloadHandler(
        filename = function() {
          paste("Comparativos_causa_obito", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          # Código de geração do df_bolhas (mantém tudo que você tem)
          
          df_sim <- df_obitos  
          
          # ---- 1) Óbitos com notificação (SINAN VIOL) ----
          a <- df_sim |>
            filter(banco == "SIM", FL_SINAN_VIOL == 1) |>
            tab_1(causa_resumida) |>
            filter(causa_resumida != "Total") |>
            transmute(causa_resumida,
                      nivel = "Óbitos com notificação de violência",
                      valor = `%`,
                      n = n)
          
          # ---- 2) Óbitos – SINAN IEXO ----
          b <- df_sim |>
            filter(banco == "SIM", FL_SINAN_IEXO == 1) |>
            tab_1(causa_resumida) |>
            filter(causa_resumida != "Total") |>
            transmute(causa_resumida,
                      nivel = "Óbitos com notificação de intoxicação exógena",
                      valor = `%`,
                      n = n)
          
          # ---- 3) Óbitos sem notificação ----
          c <- df_sim |>
            filter(banco == "SIM", FL_SINAN_VIOL != 1, FL_SINAN_IEXO != 1) |>
            tab_1(causa_resumida) |>
            filter(causa_resumida != "Total") |>
            transmute(causa_resumida,
                      nivel = "Óbitos sem notificação",
                      valor = `%`,
                      n = n)
          
          # ---- Combina tudo em formato longo ----
          df_bolhas <- bind_rows(a, b, c)
          
          df_bolhas <- df_bolhas |> pivot_wider(
            names_from = nivel,
            values_from = c(valor, n),
            values_fill = list(valor = 0, n = 0)
          )
          
          # CORREÇÃO: salvar df_bolhas, não tabela_fxetaria
          writexl::write_xlsx(df_bolhas, path = file)
        }
      )
      
      
      
      # ---- Comparação SINAN Violências vs SINAN Intoxicação Exógena ----
      output$graf_viol_iexo <- renderPlotly({
        # Cria o gráfico de barras
        p <- ggplot(df, aes(x = banco,
                            y = n,
                            fill = cor,
                            text = paste0(
                              "Fonte: ", banco, 
                              "\nNúmero de mulheres: ", n
                            ))) +
          geom_bar(stat = "identity") +
          scale_fill_identity() +
          labs(x = "Banco de dados", y = "Número de registros") +
          theme_minimal() +
          theme(legend.position = "none") +
          geom_text(aes(label = n), vjust = -0.5, size = 5) # Adiciona os rótulos
        
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0", # Cor de fundo do tooltip
            font = list(color = "black")
          )
        )
      })
      
      output$download_viol_iexo <- downloadHandler(
        filename = function() {
          paste("mulheres_bancos", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          df <- df |> 
            select(-cor)
          
          # CORREÇÃO: salvar df
          writexl::write_xlsx(df, path = file)
        }
      )
      
    })
  
}
