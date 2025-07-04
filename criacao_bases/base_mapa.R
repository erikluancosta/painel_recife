library(tidyverse)
library(writexl)
source("conectar/conectar.R")

# Estabelecendo conexão
con <- conectar('linkage')



pontos_viol <- dbGetQuery(con, '
                        SELECT id_unico, latitude_final, longitude_final
                        FROM coord_ibge_linkage;
                        ')

#write_xlsx(endereco, 'dados/tabela_de_enderecos.xlsx')
# RODEI NO GOOGLE SHEETS OS LAT LONG

google <- readxl::read_excel('dados/tabela_de_enderecos.xlsx')

save(pontos_viol, file = "data/pontos_viol_real.RData")

load("dados/pontos_viol_real.RData")


a <- pontos_viol |> 
  left_join(google, by = c("id_unico"))

pontos_viol <- a
save(pontos_viol, file = "dados/pontos_viol_real.RData")





dbWriteTable(con, "geoloc_viol22", pontos_viol, overwrite = TRUE, row.names = FALSE)
