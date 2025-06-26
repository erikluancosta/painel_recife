library(tidyverse)
source("conectar/conectar.R")

# Estabelecendo conexão
con <- conectar('linkage')

df_sim <- dbGetQuery(con, 
                  "SELECT ds_raca, nu_idade_anos, banco, cd_causabas, ano
        FROM tratado_sim
          WHERE ano::NUMERIC BETWEEN 2016 AND 2022
            AND sg_sexo = 'F'
        ")

# Adicionando faixa-etária no SIM
df_sim <- df_sim |> 
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      nu_idade_anos < 1 ~ "<1",
      nu_idade_anos >= 1 & nu_idade_anos <= 4 ~ "01-04",
      nu_idade_anos >= 5 & nu_idade_anos <= 9 ~ "05-09", 
      nu_idade_anos >= 10 & nu_idade_anos <= 19 ~ "10-19", 
      nu_idade_anos >= 20 & nu_idade_anos <= 29 ~ "20-29", 
      nu_idade_anos >= 30 & nu_idade_anos <= 39 ~ "30-39", 
      nu_idade_anos >= 40 & nu_idade_anos <= 49 ~ "40-49", 
      nu_idade_anos >= 50 & nu_idade_anos <= 59 ~ "50-59", 
      nu_idade_anos >= 60 & nu_idade_anos <= 69 ~ "60-69", 
      nu_idade_anos >= 70 & nu_idade_anos <= 79 ~ "70-79", 
      nu_idade_anos >= 80 ~ "80+", 
      TRUE ~ as.character(nu_idade_anos)
    )
  )

save(df_sim, file="dados/df_sim.RData")
