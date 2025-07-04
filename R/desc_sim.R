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
library(RPostgres)
library(DBI)

load('dados/df_sim.RData')


df_sim <- df_sim |> 
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
      TRUE ~ "Ignorada"
    )
  )

sim_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Filtros",
        collapsible = FALSE,
        maximizable = FALSE,
        fluidRow(
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
                 )
          ),
          column(4,
                 wellPanel(
                   pickerInput(
                     inputId = ns("filtro_raca"),
                     label = strong("Raça/cor"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       noneSelectedText = "Nenhuma seleção"
                     ),
                     choices = c(
                       "Branca", 
                       "Preta", 
                       "Parda",
                       "Indígena",
                       "Amarela",
                       "Ignorada"
                     ),
                     selected = c(
                       "Branca", 
                       "Preta", 
                       "Parda",
                       "Indígena",
                       "Amarela",
                       "Ignorada"
                     )
                   )
                 )
          ),
          column(4,
                 wellPanel(
                   pickerInput(
                     inputId = ns("filtro_ano"),
                     label = strong("Ano"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       noneSelectedText = "Nenhuma seleção"
                     ),
                     choices = c(2016, 2017, 2018,2019, 2020, 2021, 2022),
                     selected = c(2016, 2017, 2018,2019, 2020, 2021, 2022)
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
        title = strong("Faixa etária"),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("faixa_etaria_graf"), height = "400px"),  # Aumenta a altura do gráfico
        downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")
      ),
      
      box(
        title = strong("Raça/cor"),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("raca_cor_graf"), height = "400px"),  # Aumenta a altura do gráfico
        downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela")
      )
    ),
    
    fluidRow(
      box(
        title = strong("Proporção de óbitos por causas externas (capítulo XIX e XX) da CID-10, 2016-2022"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        div(
          style = "overflow-y: scroll;",
          plotlyOutput(ns("graf_obito"), height = "900px")  # Ajusta a altura conforme necessário
        )
      )
    ),
    
    fluidRow(
      column(12,
             dataTableOutput(ns("sim"))
      )
    )
  )
}

sim_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$freq_ano_graf <- renderPlotly({
        
        a <- df_sim |> 
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca
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
          scale_y_continuous(limits = c(5000, 9000)) +
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
      
      # Gráfico Faixa etária
      output$faixa_etaria_graf <- renderPlotly({
        
        a <-  df_sim |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ano %in% input$filtro_ano
          ) |> 
          tab_1(faixa_etaria_padrao) |>
          filter(faixa_etaria_padrao != "Total") |> 
          mutate(cor = ifelse(faixa_etaria_padrao == "Ignorada", "#9ba2cb", "#121E87")) |>
          ggplot(aes(
            x = faixa_etaria_padrao, y = `%`, fill = cor, 
            text = paste("Faixa etária:", faixa_etaria_padrao, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
          )) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Faixa etária", y = "Proporção") + 
          theme_minimal() +
          theme(legend.position = "none",
                axis.text.x = element_text(hjust = 1))
        
        ggplotly(a, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # Download da tabela Faixa etária
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          tabela_fxetaria <-  df_sim |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(faixa_etaria_padrao) |>
            filter(faixa_etaria_padrao != "Total") |>
            arrange(faixa_etaria_padrao)
          write_xlsx(tabela_fxetaria, file)
        }
      )
      
      # Gráfico Raça/cor
      output$raca_cor_graf <- renderPlotly({
        dados_preparados <- df_sim |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
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
          text = paste("Raça/Cor:", ds_raca, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
        ))  +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = cores) +
          labs(
            x = "Raça/cor",
            y = "Proporção"
          ) +
          theme_minimal() +
          coord_flip() +
          theme(legend.position = "none")
        
        ggplotly(b, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
        
      })
      
      # Raça/cor download
      output$download_tab_raca_cor <- downloadHandler(
        filename = function() {
          paste("dados-raca-cor-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          tabela_raca <-  df_sim |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_raca) 
          write_xlsx(tabela_raca, file)
        })
      
      
      
      
      # Gráfico de Óbitos
      output$graf_obito <- renderPlotly({
        ## Colocando a descricao dos SIH
        cid_10 <- read.csv2('dados/cid_10.csv')
        
        cid_10 <- cid_10 |> unique()
        # cid_10 <- openxlsx::read.xlsx('dados/cid10_tratado.xlsx')
        
        df_sim_mod <- df_sim |> 
          filter(
            ano %in% input$filtro_ano
          ) |> 
          left_join(cid_10, by = c("cd_causabas" = "SUBCAT")) |> 
          filter(
            CAPITULO %in% c(
              "Capítulo XIX - Lesões, envenenamento e algumas outras conseqüências de causas externas",
              "Capítulo XX - Causas externas de morbidade e de mortalidade"
            )
          ) |> 
          mutate(
            CAPITULO = ifelse(is.na(CAPITULO), "Sem registro", CAPITULO)
          ) |> 
          vitaltable::tab_1(DESCRICAO_CAT) |> 
          filter(DESCRICAO_CAT != "Total") 
        
        # Ajusta os níveis do fator e quebra o texto das categorias
        df_sim_mod$DESCRICAO_CAT <- stringr::str_wrap(df_sim_mod$DESCRICAO_CAT, width = 60)
        df_sim_mod <- df_sim_mod |> mutate(DESCRICAO_CAT = forcats::fct_reorder(DESCRICAO_CAT, n))
        
        gr_ob <- df_sim_mod |> 
          ggplot(aes(
            x = `%`, y = DESCRICAO_CAT,
            text = paste("Capítulo da CID-10:", DESCRICAO_CAT, "\nProporção:", `%`, "%", "\nRegistros:", n)
          )) +
          geom_bar(stat = "identity", width = 0.8, fill = "#121E87") +
          scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
          theme_minimal() +
          theme(
            axis.text.y = element_text(size = 10, lineheight = 1.5),  # Aumenta o lineheight
            axis.text.x = element_text(size = 11),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(size = 8)
          ) +
          labs(
            x = NULL,
            y = "Proporção"
          )
        
        # Ajusta a altura do gráfico de acordo com o número de categorias
        plot_height <- nrow(df_sim_mod) * 50  # Ajuste o multiplicador conforme necessário
        
        ggplotly(gr_ob, tooltip = "text", height = plot_height) |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
    }
  )
}




