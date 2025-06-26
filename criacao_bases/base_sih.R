library(tidyverse)
library(writexl)
source("conectar/conectar.R")

# Estabelecendo conexão
con <- conectar('linkage')


#Carregar seus dados
df_sih <- dbGetQuery(con, "
        SELECT id_unico,ano, ds_raca, nu_idade_anos, banco,cd_diag_pri, cd_procedimento::NUMERIC
        FROM tratado_sih
          WHERE ano::NUMERIC BETWEEN 2016 AND 2022
            AND sg_sexo = 'F'")

df_sih <- df_sih |> 
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


# Carregando dicionário de procedimentos
dic_procedimentos_sih <- dbGetQuery(
  con, "SELECT * FROM dic_proc_sih"
)

# Garantindo que estará como numérico
dic_procedimentos_sih <- dic_procedimentos_sih |> 
  mutate(
   cd_procedimento = as.numeric(cd_procedimento)
  )

# Juntando as categorias de procedimento
df_sih <- df_sih |> left_join(dic_procedimentos_sih, by = "cd_procedimento")

df_sih <- df_sih |> 
  select(-cd_grupo_proc, -cd_subgrupo, cd_forma_org )


# salvando
save(df_sih, file = "dados/df_sih.RData")



