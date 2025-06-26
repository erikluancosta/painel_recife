library(codetools)

# Diretório com os scripts do seu projeto
scripts_dir <- "C:\\vitalstrategies\\data_science\\paineis\\RECIFE_16_22\\painel_recife\\R"

# Lista de todos os arquivos R no diretório
r_files <- list.files(scripts_dir, pattern = "\\.R$", full.names = TRUE)

# Função para verificar pacotes carregados
find_packages <- function(file) {
  script <- parse(file)
  codetools::findGlobals(script, merge = FALSE)$packages
}

# Aplicar a função em todos os arquivos e buscar "vitallinkage"
used_packages <- lapply(r_files, find_packages)
names(used_packages) <- r_files

# Filtrar os scripts que usam "vitallinkage"
scripts_with_vitallinkage <- names(Filter(function(pkgs) "vitallinkage" %in% pkgs, used_packages))
print(scripts_with_vitallinkage)





ruas <- read.csv2("C:\\vitalstrategies\\data_science\\paineis\\RECIFE_16_22\\linha_da_vida_recife\\dados\\2611606_RECIFE\\2611606_RECIFE.csv")


# Reduzindo o dataframe
ruas <- ruas |> filter(DSC_ESTABELECIMENTO == "")

# Exemplo hipotético do data frame com o número de endereços desejado por bairro
df_bairros_amostra <-pontos_viol |> 
  vitaltable::tab_1(ds_bairro_res) %>%
  select(ds_bairro_res, n) |> 
  rename(DSC_LOCALIDADE = ds_bairro_res)

filter_bairros <- ruas |> 
  filter(DSC_LOCALIDADE %in% df_bairros_amostra$DSC_LOCALIDADE) 



library(dplyr)

# Exemplo de data frame com número de endereços desejado por bairro
library(dplyr)
library(purrr)

# Divide df_bairros_amostra por DSC_LOCALIDADE
lista_bairros <- df_bairros_amostra %>% group_split(DSC_LOCALIDADE)

# Para cada "pedaço", faz o filtro e amostragem apropriada e junta com map_dfr()
amostra_enderecos <- map_dfr(
  lista_bairros,
  ~ {
    # Pega o nome do bairro atual
    bairro_atual <- unique(.x$DSC_LOCALIDADE)
    # Pega o tamanho da amostra desejado
    n_amostra <- unique(.x$n)
    
    filter(filter_bairros, DSC_LOCALIDADE == bairro_atual) %>%
      slice_sample(n = n_amostra)
  }
)


amostra_enderecos <- amostra_enderecos |> 
  select(DSC_LOCALIDADE, LATITUDE, LONGITUDE)





















# 1) Filtrar apenas as linhas de pontos_viol que tenham ds_bairro_res não vazio 
#    (e que esteja entre os bairros que você quer parear, se desejar).
pontos_viol_valid <- pontos_viol %>%
  filter(!is.na(ds_bairro_res)) 
# Se quiser, inclua: filter(ds_bairro_res %in% unique(amostra_enderecos$DSC_LOCALIDADE))

# 2) Dentro de cada bairro (ds_bairro_res), criar um índice aleatório 1..n
pontos_viol_valid <- pontos_viol_valid %>%
  group_by(ds_bairro_res) %>%
  mutate(rand_id = sample(row_number())) %>%
  ungroup()

# 3) Fazer o mesmo em amostra_enderecos, agrupando por DSC_LOCALIDADE
amostra_enderecos2 <- amostra_enderecos %>%
  group_by(DSC_LOCALIDADE) %>%
  mutate(rand_id = sample(row_number())) %>%
  ungroup()

# 4) Agora fazemos o join pela dupla (bairro, rand_id).
#    - Se você quiser manter todas as linhas de pontos_viol_valid,
#      use left_join(); as linhas que não encontrarem match
#      ficarão com LAT/ LONG em NA.
#    - Se você quiser apenas as linhas que de fato casaram 1 a 1,
#      use inner_join().
final_df <- pontos_viol_valid %>%
  left_join(amostra_enderecos2, 
            by = c("ds_bairro_res" = "DSC_LOCALIDADE", "rand_id" = "rand_id"))


finalizar_latlong <- final_df |> 
  select(id_unico, LATITUDE, LONGITUDE)


pontos_viol_valid <- pontos_viol_valid |> 
  select(id_unico, ds_bairro_res, LATITUDE, LONGITUDE)


pontos_viol <- pontos_viol |> 
  left_join(finalizar_latlong, by = "id_unico")


pontos_viol <- pontos_viol |>
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE)) |> 
 # select(-latitude_final, -longitude_final) |> 
  rename("latitude_final" = LATITUDE, "longitude_final" = LONGITUDE)

save(pontos_viol, file = "dados\\pontos_viol.RData")

