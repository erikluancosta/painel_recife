library(shiny)
library(shinyWidgets)
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
library(DBI)
library(RPostgres)
library(readr)


# Carregando os dados
#df_sinan <- read_csv2('dados/tela_sinan_viol.csv', col_types = cols(.default = col_character()))

load('dados/sinan_viol.RData')


df_sinan <- df_sinan |>
  mutate(
    rede_enc_sau = case_when((rede_sau == "1" | enc_saude == "1") ~ 1,
                             TRUE ~ 0),
    assit_soc_creas = case_when((assist_soc == "1" | enc_creas == "1") ~ 1,
                                TRUE ~ 0),
    atend_enc_mulh = case_when((atend_mulh == "1" | enc_mulher == "1") ~ 1,
                               TRUE ~ 0),
    cons_enc_tutela = case_when((cons_tutel == "1" | enc_tutela == "1") ~ 1,
                                TRUE ~ 0),
    mpu_enc_mpu = case_when((mpu == "1" | enc_mpu == "1") ~ 1,
                            TRUE ~ 0),
    deleg_enc_cria = case_when((deleg_cria == "1" | enc_dpca == "1") ~ 1,
                               TRUE ~ 0),
    deleg_enc_mulh = case_when((deleg_mulh == "1" | enc_deam == "1") ~ 1,
                               TRUE ~ 0),
    deleg_enc_deleg = case_when((deleg == "1" | enc_deleg == "1") ~ 1,
                                TRUE ~ 0),
    infan_enc_juv = case_when((infan_juv == "1" | enc_vara == "1") ~ 1),
    banco = "SINAN",
    ds_autor_sexo = case_when(
      autor_sexo == "1" ~ "Masculino",
      autor_sexo == "2" ~ "Feminino",
      autor_sexo == "3" ~ "Ambos os sexos",
      TRUE ~ "Ignorado"
    ),
    autor_alco = case_when(
      autor_alco == "1" ~ "Sim",
      autor_alco == "2" ~ "Não",
      TRUE ~ "Ignorado"
    ),
    les_autop = case_when(
      les_autop == "1" ~ "Sim",
      les_autop == "2" ~ "Não",
      les_autop == "9" | is.na(les_autop) ~ "Ignorado",
      TRUE ~ les_autop
    ),
    local_ocor = case_when(
      local_ocor == "01" ~ "Residência",
      local_ocor == "02" ~ "Habitação coletiva",
      local_ocor == "03" ~ "Escola",
      local_ocor == "04" ~ "Local de prática esportiva",
      local_ocor == "05" ~ "Bar ou similar",
      local_ocor == "06" ~ "Via publica",
      local_ocor == "07" ~ "Comércio/Serviços",
      local_ocor == "08" ~ "Industrias/ construção",
      local_ocor == "09" ~ "Outro",
      local_ocor == "99" ~ "Ignorado",
      TRUE ~ "Ignorado"
    ),
    out_vezes = case_when(
      out_vezes == "1" ~'Sim',
      out_vezes == "2" ~'Não',
      out_vezes == "9" ~'Ignorado',
      TRUE ~ "Ignorado"
    )
  ) |> 
  dplyr::mutate(
    nu_idade_anos = as.numeric(nu_idade_anos),
    faixa_etaria_padrao = dplyr::case_when(
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
      TRUE ~  "Ignorada"
    )
  )




# Categoria do sinan para o tipo de agressão
agc <- data.frame(
  categoria = c("ag_forca", "ag_enfor", "ag_objeto", "ag_corte", "ag_quente",
                "ag_enven", "ag_fogo", "ag_ameaca", "ag_outros"),
  ds_tp_ag = c("Força corporal / espancamento",
               "Enforcamento / sufocação",
               "Objeto contundente",
               "Objeto cortante / perfurante",
               "Substância quente",
               "Envenenamento",
               "Arma de fogo",
               "Ameaça",
               "Outros meios")
)

# Deficiêcnias do sinan
defic <- data.frame(
  categoria = c("def_trans", "def_fisica", "def_mental", "def_visual",
                "def_auditi", "def_out", "def_espec"),
  ds_tp_def = c("Transtorno mental ou comportamental",
                "Deficiência física",
                "Deficiência mental / intelectual",
                "Deficiência visual",
                "Deficiência auditiva",
                "Outras deficiências",
                "Deficiência não especificada")
)

# Transtornos 
transt <- data.frame(
  categoria = c("tran_ment", "tran_comp"),
  ds_tp_transtorno = c("Transtorno mental", "Transtorno comportamental")
)

# UI do módulo SINAN
sinan_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Filtros",
        collapsible = FALSE,
        maximizable = FALSE,
        fluidRow(
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_idade"),
                label = strong("Faixa Etária"),
                choices = c("<1", "01-04", "05-09", "10-14", "15-19",
                            "20-29", "30-39", "40-49",
                            "50-59", "60-69", "70-79", "80+", "Ignorada"),
                selected = c("<1", "01-04", "05-09", "10-14", "15-19",
                             "20-29", "30-39", "40-49",
                             "50-59", "60-69", "70-79", "80+",  "Ignorada"),
                options = list(
                  `actions-box` = TRUE,
                  noneSelectedText = "Nenhuma seleção"
                ),
                multiple = TRUE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_raca"),
                label = strong("Raça/cor"),
                choices = c("Branca", "Preta", "Parda",
                            "Indígena", "Amarela", "Ignorada"),
                selected = c("Branca", "Preta", "Parda",
                             "Indígena", "Amarela", "Ignorada"),
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_ano"),
                label = strong("Ano"),
                choices = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
                selected = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          )
        ),
        fluidRow(
          column(
            8,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_violencias"),
                label = "Tipo de Violência",
                choices = c(
                  "Violência física" = "viol_fisic", 
                  "Violência psicológica" = "viol_psico", 
                  "Violência sexual" = "viol_sexu",
                  "Tortura" = "viol_tort",
                  "Negligência" = "viol_negli",
                  "Violência financeira" = "viol_finan",
                  "Trabalho infantil" = "viol_infan",
                  "Intervenção legal" = "viol_legal",
                  "Tráfico de humanos" = "viol_traf",
                  "Outras violências" = "viol_outr"
                ),
                selected = NULL,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("les_autop_fil"),
                label = strong("Lesão autoprovocada"),
                choices = c("Sim", "Não", "Ignorado"),
                selected = c("Sim", "Não", "Ignorado"),
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          )
          
        )
      )
    ),
    
    fluidRow(
      
      box(title=strong("Frequência de notificação por ano"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("freq_ano_graf"))
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
        title = strong("Informações do SINAN"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        # Primeira linha contendo os filtros
        fluidRow(
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("evolution_filter"),
                label = "Tipo de variável do SINAN:",
                choices = c("Encaminhamentos" = "enc", 
                            "Procedimentos" = "proc", 
                            "Relação com o agressor" = "rel", 
                            "Tipo de violência" = "viol",
                            "Meio de agressão" = "agc",
                            "Deficiências" = "defic",
                            "Transtornos" = "transt"),
                selected = "enc",
                options = list(`actions-box` = TRUE),
                multiple = FALSE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("extrato_sinan_filter"),
                label = "Extratificado por:", 
                choices = c("Raça/cor" = 'ds_raca', 
                            "Faixa etária" = 'faixa_etaria_padrao',
                            "Ano da notificação" = 'ano',
                            "Outras vezes" = "out_vezes",
                            "Local de ocorrência" = 'local_ocor'),
                selected = "ds_raca",
                options = list(`actions-box` = TRUE),
                multiple = FALSE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("valor_sinan_filter"),
                label = "Valor:", 
                choices = c("Frequência" = FALSE,
                            "Porcentagem" = TRUE),
                selected = FALSE,
                options = list(`actions-box` = TRUE),
                multiple = FALSE
              )
            )
          )
        ),
        # Segunda linha contendo o gráfico e tabela
        fluidRow(
          column(
            12,
            dataTableOutput(ns("sinan")),
            downloadButton(outputId = ns("download_tab_sinan"), label = "Download da Tabela")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = strong("Sexo do agressor por suspeita do uso de álcool"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("sexo_alcool_graf"))
      )
    ),
    
    # TAB 2
    fluidRow(
      box(
        title = strong("Tabela cruzando informações"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        fluidRow(
          column(3,
                 wellPanel(
                   pickerInput(
                     inputId = ns("var1"),
                     label = "Variável da linha",
                     choices = c("Faixa etária" = 'faixa_etaria_padrao', 
                                 "Raça/cor" = 'ds_raca', 
                                 "Ano da notificação" = 'ano',
                                 "Outras vezes"='out_vezes',
                                 "Local de ocorrência" = 'local_ocor'),
                     selected = "faixa_etaria_padrao",
                     options = list(`actions-box` = TRUE),
                     multiple = FALSE
                   ),
                   pickerInput(
                     inputId = ns("var2"),
                     label = "Variável da coluna",
                     choices = c(#"Faixa etária" = 'faixa_etaria_padrao', 
                                 "Raça/cor" = 'ds_raca', 
                                 "Ano da notificação" = 'ano',
                                 "Outras vezes"='out_vezes'
                                 #"Local de ocorrência" = 'local_ocor'
                                 ),
                     selected = "ds_raca",
                     options = list(`actions-box` = TRUE),
                     multiple = FALSE
                   ),
                   downloadButton(outputId = ns("download_tab2_sinan"), label = "Download da Tabela")
                 )
          ),
          column(9,
                 dataTableOutput(ns("tabela_cruzada"))
          )
        )
      )
    ))
}

# Server do módulo SINAN
sinan_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$freq_ano_graf <- renderPlotly({
        
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        
        a <- df_filtrado |> 
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil
            
          ) |> 
          tab_1(ano) |>
          filter(ano != "Total") |>
          ggplot(aes(
            x = ano, 
            y = `n`, 
            group = 1,
            color = "#9ba2cb",
            text = paste("Ano:", ano, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
          )) +
          geom_line(size = 1) +
          scale_color_identity() +
          labs(x = "Ano", y = "Frequência") +
          theme_minimal() +
          theme(legend.position = "none")
        
        ggplotly(a, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # Gráfico Faixa Etária
      output$faixa_etaria_graf <- renderPlotly({
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        a <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tab_1(faixa_etaria_padrao) |>
          filter(faixa_etaria_padrao != "Total") |>
          mutate(cor = ifelse(faixa_etaria_padrao == "Ignorada", "#9ba2cb", "#121E87")) |>
          ggplot(aes(
            x = faixa_etaria_padrao, y = `%`, fill = cor, 
            text = paste("Faixa etária:", faixa_etaria_padrao, "\nProporção: ", `%`, "%", "\nRegistros: ", n)
          )) + 
          geom_bar(stat = "identity") +
          scale_fill_identity() +
          labs(x = "Faixa etária", y = "Proporção") + 
          theme_minimal() +
          theme(legend.position = "none", axis.text.x = element_text(hjust = 1))
        
        ggplotly(a, tooltip = "text") |> layout(
          hoverlabel = list(bgcolor = "#FAF4F0", font = list(color = "black"))
        )
      })
      
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-sinan-viol", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          tabela_fxetaria <- df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              les_autop %in% input$les_autop_fil,
              ano %in% input$filtro_ano
            ) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          writexl::write_xlsx(tabela_fxetaria, file)
        }
      )
      
      # Gráfico Raça/Cor
      output$raca_cor_graf <- renderPlotly({
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        dados_preparados <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            les_autop %in% input$les_autop_fil,
            ds_raca %in% input$filtro_raca,
            ano %in% input$filtro_ano
          ) |>
          tab_1(ds_raca) |>
          filter(ds_raca != "Total")
        
        racas_ordenadas <- unique(dados_preparados$ds_raca)
        racas_ordenadas <- racas_ordenadas[racas_ordenadas != "Ignorada"]
        racas_ordenadas <- c(racas_ordenadas, "Ignorada")
        racas_ordenadas <- rev(racas_ordenadas)
        dados_preparados$ds_raca <- factor(dados_preparados$ds_raca, levels = racas_ordenadas)
        
        cores <- setNames(rep("#121E87", length(racas_ordenadas)), racas_ordenadas)
        cores["Ignorada"] <- "#9ba2cb"
        
        b <- ggplot(dados_preparados, aes(
          x = ds_raca, y = `%`, fill = ds_raca, 
          text = paste("Raça/Cor:", ds_raca, "\nProporção: ", `%`, "%", "\nRegistros: ", n)
        )) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = cores) +
          labs(x = "Raça/cor", y = "Proporção") +
          theme_minimal() +
          coord_flip() +
          theme(legend.position = "none")
        
        ggplotly(b, tooltip = "text") |> layout(
          hoverlabel = list(bgcolor = "#FAF4F0", font = list(color = "black"))
        )
      })
      
      output$download_tab_raca_cor <- downloadHandler(
        filename = function() {
          paste("dados-raca-cor-sinan-viol", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          tabela_raca <- df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              les_autop %in% input$les_autop_fil,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_raca)
          
          writexl::write_xlsx(tabela_raca, file)
        }
      )
      
      # Tabela SINAN
      output$sinan <- renderDataTable({
        rel <- vitaltable::rel |>
          mutate(ds_tp_rel = ifelse(ds_tp_rel == "Cônjugue", "Cônjuge", ds_tp_rel))
        
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        filtered_df <- get(input$evolution_filter)
        tabela_sinan <- df_filtrado |>
          filter(les_autop %in% input$les_autop_fil,
                 ano %in% input$filtro_ano,
                 ds_raca %in% input$filtro_raca,
                 faixa_etaria_padrao %in% input$filtro_idade) |>
          vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |>
          as.data.frame()
        
        if (input$evolution_filter == "enc") {
          tabela_sinan <- tabela_sinan |>
            rename(Encaminhamentos = tipo_filtered_df) |>
            mutate(Encaminhamentos = ifelse(Encaminhamentos == "tipo_filtered_df", "Nenhum encaminhamento", Encaminhamentos))
        } else if (input$evolution_filter == "proc") {
          tabela_sinan <- tabela_sinan |>
            rename(Procedimentos = tipo_filtered_df) |>
            mutate(Procedimentos = ifelse(Procedimentos == "tipo_filtered_df", "Nenhum procedimento", Procedimentos))
        } else if (input$evolution_filter == "rel") {
          tabela_sinan <- tabela_sinan |>
            rename(`Relacionamento com o agressor` = tipo_filtered_df) |>
            mutate(`Relacionamento com o agressor` = ifelse(`Relacionamento com o agressor` == "tipo_filtered_df", "Nenhum relacionamento registrado", `Relacionamento com o agressor`))
        } else if (input$evolution_filter == "viol") {
          tabela_sinan <- tabela_sinan |>
            rename(`Tipo de violência` = tipo_filtered_df) |>
            mutate(`Tipo de violência` = ifelse(`Tipo de violência` == "tipo_filtered_df", "Nenhum tipo de violência registrado", `Tipo de violência`))
        } else if (input$evolution_filter == "agc") {
          tabela_sinan <- tabela_sinan |>
            rename(`Meio de agressão` = tipo_filtered_df) |>
            mutate(`Meio de agressão` = ifelse(`Meio de agressão` == "tipo_filtered_df", "Nenhum tipo de meio de agressão registrado", `Meio de agressão`))
        } else if (input$evolution_filter == "defic") {
          tabela_sinan <- tabela_sinan |>
            rename(`Deficiência` = tipo_filtered_df) |>
            mutate(`Deficiência` = ifelse(`Deficiência` == "tipo_filtered_df", "Nenhuma deficiência registrada", `Deficiência`))
        } else if (input$evolution_filter == "transt") {
          tabela_sinan <- tabela_sinan |>
            rename(`Transtorno` = tipo_filtered_df) |>
            mutate(`Transtorno` = ifelse(`Transtorno` == "tipo_filtered_df", "Nenhum transtorno registrado", `Transtorno`))
        }
        
        datatable(tabela_sinan, options = list(
          pageLength = 20,
          rowCallback = JS(
            "function(row, data, index) {",
            "  if(data[0] === 'Total') {",
            "    $('td', row).css('font-weight', 'bold');",
            "  }",
            "}"
          )
        )) %>%
          formatStyle("Total", fontWeight = "bold")
      })
      
      output$download_tab_sinan <- downloadHandler(
        filename = function() {
          paste("dados-sinan-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          rel <- vitaltable::rel |>
            mutate(ds_tp_rel = ifelse(ds_tp_rel == "Cônjugue", "Cônjuge", ds_tp_rel))
          
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          filtered_df <- get(input$evolution_filter)
          tabela_sinan <- df_filtrado |>
            filter(les_autop %in% input$les_autop_fil,
                   ano %in% input$filtro_ano,
                   ds_raca %in% input$filtro_raca,
                   faixa_etaria_padrao %in% input$filtro_idade) |>
            vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |>
            as.data.frame()
          
          if (input$evolution_filter == "enc") {
            tabela_sinan <- tabela_sinan |>
              rename(Encaminhamentos = tipo_filtered_df) |>
              mutate(Encaminhamentos = ifelse(Encaminhamentos == "tipo_filtered_df", "Nenhum encaminhamento", Encaminhamentos))
          } else if (input$evolution_filter == "proc") {
            tabela_sinan <- tabela_sinan |>
              rename(Procedimentos = tipo_filtered_df) |>
              mutate(Procedimentos = ifelse(Procedimentos == "tipo_filtered_df", "Nenhum procedimento", Procedimentos))
          } else if (input$evolution_filter == "rel") {
            tabela_sinan <- tabela_sinan |>
              rename(`Relacionamento com o agressor` = tipo_filtered_df) |>
              mutate(`Relacionamento com o agressor` = ifelse(`Relacionamento com o agressor` == "tipo_filtered_df", "Nenhum relacionamento registrado", `Relacionamento com o agressor`))
          } else if (input$evolution_filter == "viol") {
            tabela_sinan <- tabela_sinan |>
              rename(`Tipo de violência` = tipo_filtered_df) |>
              mutate(`Tipo de violência` = ifelse(`Tipo de violência` == "tipo_filtered_df", "Nenhum tipo de violência registrado", `Tipo de violência`))
          } else if (input$evolution_filter == "agc") {
            tabela_sinan <- tabela_sinan |>
              rename(`Meio de agressão` = tipo_filtered_df) |>
              mutate(`Meio de agressão` = ifelse(`Meio de agressão` == "tipo_filtered_df", "Nenhum tipo de meio de agressão registrado", `Meio de agressão`))
          } else if (input$evolution_filter == "defic") {
            tabela_sinan <- tabela_sinan |>
              rename(`Deficiência` = tipo_filtered_df) |>
              mutate(`Deficiência` = ifelse(`Deficiência` == "tipo_filtered_df", "Nenhuma deficiência registrada", `Deficiência`))
          } else if (input$evolution_filter == "transt") {
            tabela_sinan <- tabela_sinan |>
              rename(`Transtorno` = tipo_filtered_df) |>
              mutate(`Transtorno` = ifelse(`Transtorno` == "tipo_filtered_df", "Nenhum transtorno registrado", `Transtorno`))
          }
          writexl::write_xlsx(tabela_sinan, file)
        }
      )
      
      # Gráfico: Sexo do agressor por suspeita do uso de álcool
      output$sexo_alcool_graf <- renderPlotly({
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        a <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tab_2(ds_autor_sexo, autor_alco) |>
          filter(ds_autor_sexo != "Total") |>
          select(-Total)
        
        a <- a |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),
                               names_to = "autor_alco",
                               values_to = "n")
        
        b <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tab_2(ds_autor_sexo, autor_alco, pct_row = TRUE) |>
          filter(ds_autor_sexo != "Total") |>
          select(-Total)
        
        b <- b |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),
                               names_to = "autor_alco",
                               values_to = "%")
        
        c <- merge(a, b, by = c('ds_autor_sexo', 'autor_alco')) |>
          mutate(text = paste("Sexo do agressor:", ds_autor_sexo, "\nProporção: ", round(`%`, 1), "%", "\nRegistros: ", n))
        
        c$ds_autor_sexo <- factor(c$ds_autor_sexo, levels = c("Masculino", "Feminino", "Ambos os sexos", "Ignorado"))
        d <- c |> 
          ggplot(aes(x = ds_autor_sexo, y = n, fill = autor_alco, text = text)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = c("Sim" = "#121E87", "Ignorado" = "#9ba2cb", "Não" = "#FF5054")) +
          geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour = "#FAF4F0", size = 3) +
          labs(x = "", y = "Frequência entre as categorias", fill = "Suspeita de uso de álcool") +
          theme_minimal() +
          ggtitle("")
        
        ggplotly(d, tooltip = "text") |> layout(
          hoverlabel = list(bgcolor = "#FAF4F0", font = list(color = "black"))
        )
      })
      
      
      
      ## FUNCAO DA TABELA 2
      tabela_2 <- function(df, var_row, var_col, freq_var = NULL, pct = FALSE, pct_row = FALSE, dec = 1) {
        var_row_sym <- rlang::sym(var_row)
        var_col_sym <- rlang::sym(var_col)
        
        if (!is.null(freq_var)) {
          freq_var_sym <- rlang::sym(freq_var)
        }
        
        if (is.null(freq_var)) {
          # Caso freq_var seja NULL, aplica-se a lógica de contagem simples
          df <- df %>%
            group_by(!!var_row_sym, !!var_col_sym) %>%
            summarise(
              contagem = n(),
              .groups = 'drop'
            )
        } else {
          df <- df %>%
            group_by(!!var_row_sym, !!var_col_sym) %>%
            summarise(
              contagem = as.numeric(sum(!!freq_var_sym, na.rm = TRUE)),
              .groups = 'drop'
            )
        }
        
        df <- df %>%
          pivot_wider(
            names_from = !!var_col_sym,
            values_from = contagem,
            values_fill = 0
          ) %>%
          adorn_totals("col") %>%
          arrange(-Total)
        
        if (pct) {
          df <- df %>%
            filter(
              !!var_row_sym != 'Total'
            ) %>%
            mutate(
              across(
                where(is.numeric),
                ~round((. / sum(.)) * 100, dec)
              )
            )
        }
        
        if (pct_row) {
          df <- df %>%
            filter(
              !!var_row_sym != 'Total'
            ) %>%
            mutate(
              across(
                where(is.numeric),
                ~round((. / Total) * 100, dec)
              )
            )
        }
        
        df <- df %>%
          adorn_totals("row", name = "Total")
        
        return(as.data.frame(df))
      }
      
      
      output$tabela_cruzada <- renderDataTable({
        req(input$var1, input$var2)
        
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        dados <- df_sinan |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tabela_2(
            var_row = input$var1, 
            var_col = input$var2,
            freq_var = NULL
            )
        
        datatable(dados, options = list(
          pageLength = 20,
          rowCallback = JS(
            "function(row, data, index) {",
            "  if(data[0] === 'Total') {",
            "    $('td', row).css('font-weight', 'bold');",
            "  }",
            "}"
          )))
      })
      
      output$download_tab2_sinan<- downloadHandler(
        filename = function() {
          paste("dados-sinan-cruzados", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          req(input$var1, input$var2)
          
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          dados <- df_sinan |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              les_autop %in% input$les_autop_fil,
              ano %in% input$filtro_ano
            ) |>
            tabela_2(
              var_row = input$var1, 
              var_col = input$var2,
              freq_var = NULL
            )
          
          writexl::write_xlsx(dados, file)
        }
      )
      
      # Outros gráficos (como o gráfico de "sexo_agressor_graf" e "uso_alc_graf") permanecem inalterados
      
    }
  )
}



