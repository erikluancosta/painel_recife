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
library(DBI)
library(RPostgres)
library(readr)

load("dados/df_iexo.RData")

#df_iexo <- read_csv2('dados/tela_sinan_iexo.csv', col_types = cols(.default = col_character()))

#df_iexo <- vitallinkage::faixa_etaria(df_iexo |> mutate(nu_idade_anos = as.numeric(nu_idade_anos)))

#df_iexo <- df_iexo |> rename("faixa_etaria" = faixa_etaria)

iexo_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(box
             (width = 12,
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
                            choices = c("<1", "01-04", "05-09", "10-19",
                                        "20-29", "30-39", "40-49",
                                        "50-59", "60-69", "70-79", "80+"),
                            selected = c("<1", "01-04", "05-09", "10-19",
                                         "20-29", "30-39", "40-49",
                                         "50-59", "60-69", "70-79", "80+"),
                          options = list(
                            `actions-box` = TRUE,
                            noneSelectedText = "Nenhuma seleção"
                          )
                        ))),
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
                              "Ignorada"))
                        )),
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
                 
               ),
               
               
               fluidRow(
                 column(12,
                        wellPanel(
                          pickerInput(
                            inputId = ns("filtro_circuns"),
                            label = "Circunstância",
                            multiple = TRUE,
                            options = list(
                              `actions-box` = TRUE,
                              noneSelectedText = "Nenhuma seleção"
                            ),
                            choices = (df_iexo$ds_circunstan) |> unique() |> sort(),
                            selected = df_iexo$ds_circunstan)
                        ))
                 
                 
               )
             )),
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
      
      box(title = strong('Faixa etária'),
          width = 6,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("faixa_etaria_graf")),
          downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")),
      
      box(title = strong('Raça/cor'),
          width = 6,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("raca_cor_graf")),
          downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      
      box(title=strong("Proporção por Circunstância"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("circunstancia_graf")),
          downloadButton(outputId = ns("download_tab_circunstancia"), label = "Download da Tabela"))
      ),
    
    fluidRow(
      
      box(title=strong("Proporção por agente intoxicante"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("ag_intox_graf")),
          downloadButton(outputId = ns("download_tab_ag_intox"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      
      box(title=strong("Proporção de tipo de atendimento por hospitalização"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("atend_hospit_graf")),
          downloadButton(outputId = ns("download_atend_hospit_graf"), label = "Download da Tabela"))
    )
    )
    
}


iexo_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$freq_ano_graf <- renderPlotly({
        a <- df_iexo |> 
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns
            
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
      
      # Gráfico de Faixa Etária
      output$faixa_etaria_graf <- renderPlotly({
        a <-  df_iexo |>
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ano %in% input$filtro_ano
          ) |>
          tab_1(faixa_etaria) |>
          filter(faixa_etaria != "Total") |> 
          mutate(cor = ifelse(faixa_etaria == "Ignorada", "#9ba2cb", "#121E87")) |>
          ggplot(aes(
            x = faixa_etaria, y = `%`, fill = cor, 
            text = paste("Faixa etária:", faixa_etaria, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
          )) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Faixa etária", y = "Proporção") + 
          theme_minimal() +
          theme(legend.position = "none") +
          theme(axis.text.x = element_text( hjust = 1))
        
        ggplotly(a, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # Faixa etária download
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          tabela_fxetaria <-  df_iexo |>
            filter(
              faixa_etaria %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(faixa_etaria) |>
            arrange(faixa_etaria)
          write.csv2(tabela_fxetaria, file, row.names = FALSE, fileEncoding = "latin1")
          
        }
      )
      
      # Gráfico de Raça/Cor
      output$raca_cor_graf <- renderPlotly({
        dados_preparados <- df_iexo  |>
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_circunstan %in% input$filtro_circuns,
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
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = cores) +
          labs(x = "Raça/cor", y = "Proporção") +
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
          paste("dados-raca-cor-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          tabela_raca <-  df_iexo |>
            filter(
              faixa_etaria %in% input$filtro_idade,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_raca) |>
            filter(ds_raca != "Total") |>
            arrange(ds_raca)
          write.csv2(tabela_raca, file, row.names = FALSE, fileEncoding = "latin1")

          
        })
      
      # Gráfico de Circunstâncias
      output$circunstancia_graf <- renderPlotly({
        data_filtered <- df_iexo |>
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |>
          tab_1(ds_circunstan) |>
          filter(ds_circunstan != "Total")
        
        data_filtered$ds_circunstan <- with(data_filtered, reorder(ds_circunstan, n))
        
        p <- ggplot(data_filtered, aes(
          x = ds_circunstan, y = `%`, fill = "#121E87",
          text = paste("Circunstância:", ds_circunstan,  "\nProporção: ", `%`,"%", "\nRegistros: ", n)
        )) +
          geom_bar(stat = "identity") +
          labs(x = "Circunstância", y = "Proporção") +
          theme_minimal() +
          theme(legend.position = "none") +
          coord_flip()
        
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # tabela circunstancia
      output$download_tab_circunstancia <- downloadHandler(
        filename = function() {
          paste("dados-circunstancia-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          tabela_raca <-  df_iexo |>
            filter(
              faixa_etaria %in% input$filtro_idade,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_circunstan) |>
            filter(ds_circunstan != "Total") |>
            arrange(ds_circunstan)
          write.csv2(tabela_raca, file, row.names = FALSE, fileEncoding = "latin1")
          
        })
      
      
      output$ag_intox_graf <- renderPlotly({
        data_filtered <- df_iexo |>
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |>
          tab_1(ds_agente_tox) |>
          filter(ds_agente_tox != "Total")
        
        data_filtered$ds_agente_tox <- with(data_filtered, reorder(ds_agente_tox, n))
        
        p <- ggplot(data_filtered, aes(
          x = ds_agente_tox, y = `%`, fill = "#121E87",
          text = paste("Agente intoxicante:", ds_agente_tox,  "\nProporção: ", `%`,"%", "\nRegistros: ", n)
        )) +
          geom_bar(stat = "identity") +
          labs(x = "Agente intoxicante", y = "Proporção") +
          theme_minimal() +
          theme(legend.position = "none") +
          coord_flip()
        
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # Download handler for Agente Intoxicante
      output$download_tab_ag_intox <- downloadHandler(
        filename = function() {
          paste("dados-agente-intoxicante-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          tabela_ag_intox <- df_iexo |>
            filter(
              faixa_etaria %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_agente_tox) |>
            filter(ds_agente_tox != "Total") |>
            arrange(ds_agente_tox)
          write.csv2(tabela_ag_intox, file, row.names = FALSE, fileEncoding = "latin1")
          
        }
      )
      
      
      
      output$atend_hospit_graf <- renderPlotly({
        
        # Se não houver seleção, retorna o dataframe completo
        filtro_viol <- input$filtro_violencias
        
    
        a <- df_iexo |>
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |> 
          tab_2(ds_tpatend, ds_hospital) |> 
          filter(ds_tpatend != "Total") |> 
          select(-Total)
        
        a <- a |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                               names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                               values_to = "n")
        
        b <- df_iexo |>
          filter(
            faixa_etaria %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |> 
          tab_2(ds_tpatend, ds_hospital, pct_row=T) |> 
          filter(ds_tpatend != "Total") |>
          select(-Total)
        
        b <- b |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                               names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                               values_to = "%")
        
        
        c <- merge(a,b, by=c('ds_tpatend', 'ds_hospital')) |> mutate(text = paste("Tipo de atendimento:", ds_tpatend, "\nProporção: ", round(`%`, 1), "%", "\nRegistros: ", n))
        
        c$ds_tpatend <- factor(c$ds_tpatend, levels = c("Ambulatorial", "Domiciliar", "Hospitalar", "Ignorado", "Nenhum"))
        d <- c |> 
          ggplot(aes(x = ds_tpatend, y = n, fill = ds_hospital, text = text)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = c("Sim" = "#121E87", 
                                       "Ignorado" = "#9ba2cb", 
                                       "Não" = "#FF5054")) +
          geom_text(aes(
            label = n), 
            position = position_stack(vjust = 0.5),
            colour ="#FAF4F0",
            size = 3
          ) +
          labs(x = "", y = "Frequência entre as categorias", fill = "Hospitalização") +
          theme_minimal() +
          ggtitle("")
        
        # Convertendo o ggplot para plotly e ajustando os tooltips
        ggplotly(d, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
        
      })
      
      
      # tabela hospitalizacoes
      output$download_atend_hospit_graf <- downloadHandler(
        filename = function() {
          paste("dados-atendimento_hospitalizacao-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          
          a <- df_iexo |>
            filter(
              faixa_etaria %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |> 
            tab_2(ds_tpatend, ds_hospital) |> 
            filter(ds_tpatend != "Total") |> 
            select(-Total)
          
          a <- a |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                                 names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                                 values_to = "n")
          
          b <- df_iexo |>
            filter(
              faixa_etaria %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |> 
            tab_2(ds_tpatend, ds_hospital, pct_row=T) |> 
            filter(ds_tpatend != "Total") |>
            select(-Total)
          
          b <- b |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                                 names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                                 values_to = "%")
          
          
          c <- merge(a,b, by=c('ds_tpatend', 'ds_hospital')) |> mutate(text = paste("Tipo de atendimento:", ds_tpatend, "\nProporção: ", round(`%`, 1), "%", "\nRegistros: ", n))
          
          
          write.csv2(c, file, row.names = FALSE, fileEncoding = "latin1")
        })
      
    
    }
  )
}


