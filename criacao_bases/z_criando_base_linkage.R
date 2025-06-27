library(reshape2)
library(vitaltable)
library(data.table)
library(dplyr)
library(tidyverse)
library(janitor)
source("conectar/conectar.R")

# Configurar a conexão ao banco de dados PostgreSQL
con <- conectar("linkage")

# Todo banco do sinan_viol
sinan_viol <- dbGetQuery(con, "SELECT * FROM tratado_sinan_viol")

# Todo banco do sinan_iexo
sinan_iexo <- dbGetQuery(con, "SELECT * FROM tratado_sinan_iexo")

# Todo banco do sim
sim <- dbGetQuery(con, "SELECT * FROM tratado_sim")

# Todo banco do SIH
sih <- dbGetQuery(con, "SELECT * FROM tratado_sih")

# TRansformando em texto para concatenar sem problemas
sinan_iexo <- sinan_iexo |> vitallinkage::as_char()

# base para analse de linkage
base_linkage <- bind_rows(sinan_iexo, sinan_viol, sim, sih)#, esus)

rm(sih, sim, sinan_iexo)#, sinan_viol)
# Tabela de Id
id <- dbGetQuery(con, "SELECT * FROM registro_linkage")



# Criando a base de linkage+-, seleção de variáveis e tratamento
base_linkage2 <- base_linkage |>
  select(
    id_unico,
    ds_raca, sg_sexo, banco, ano, # descritivas faixa_etaria
    
    # SINAN
    ## ENCAMINHAMENTOS
    rede_educa, cons_ido, deleg_idos,  dir_human, defen_publ, rede_sau, enc_saude,assist_soc,enc_creas, atend_mulh, enc_mulher, cons_tutel, enc_tutela, mpu,
    enc_mpu, deleg_cria, enc_dpca, deleg_mulh, enc_deam, deleg, enc_deleg, infan_juv, enc_vara, # Enc
    
    les_autop, ds_bairro_res, local_ocor, out_vezes,
    
    ## TIPO DE VIOLÊNCIA
    viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr,
    
    ## TIPO DE PROCEDIMENTO
    proc_dst, proc_hiv, proc_hepb, proc_sang, proc_semen, proc_vagin, proc_contr, proc_abort, # proc
    
    ## RELACIONAMENTO COM O AGRESSOR
    rel_pai, rel_mae,  rel_pad, rel_mad, rel_conj, rel_excon, rel_namo,rel_exnam, rel_filho, rel_irmao,
    rel_conhec, rel_desco, rel_cuida, rel_patrao, rel_inst, rel_pol, rel_propri, rel_outros,
    
    ## Meio de agressão
    ag_forca, ag_enfor, ag_objeto, ag_corte, ag_quente, ag_enven, ag_fogo, ag_ameaca, ag_outros,
    
    ## Deficiências
    def_trans, def_fisica, def_mental, def_visual, def_auditi, def_out, def_espec,
    
    ## Outros
    autor_sexo, autor_alco, cd_causabas, lococor, cd_idade, nu_idade_anos, ds_agente_tox, ds_loc_expo, ds_cd_via1, ds_via_2,
    ds_circunstan, ds_tpexp, ds_tpatend, ds_hospital, ds_classi_fin, ds_evolucao,
    
    
    # hospitalizacoes 
    cd_diag_pri, cd_procedimento
  ) |> 
  filter(ano > 2015 & ano < 2023) |>
  mutate(par_1 = 1,
         
         rede_enc_sau = case_when((rede_sau=="1" | enc_saude=="1")~1,
                                  T~0),
         assit_soc_creas = case_when((assist_soc=="1" |enc_creas=="1")~1,
                                     T~0), # abrigo
         atend_enc_mulh = case_when((atend_mulh=="1" | enc_mulher=="1")~1,
                                    T~0),
         cons_enc_tutela = case_when((cons_tutel=="1" | enc_tutela=="1")~1,
                                     T~0),
         mpu_enc_mpu = case_when((mpu=="1" | enc_mpu=="1")~1,
                                 T~0),
         deleg_enc_cria = case_when((deleg_cria=="1"|enc_dpca=="1")~1,
                                    T~0),
         deleg_enc_mulh = case_when((deleg_mulh=="1"| enc_deam=="1")~1,
                                    T~0),
         deleg_enc_deleg = case_when((deleg=="1"|enc_deleg=="1")~1,
                                     T~0),
         infan_enc_juv = case_when((infan_juv=="1"|enc_vara=="1")~1),
         
         ds_les_autop = case_when(
           les_autop==1~"Sim",
           les_autop==2~"Não",
           les_autop==9~"Ignorado",
           TRUE ~ as.character(les_autop)),
         
         ds_autor_sexo = case_when(
           autor_sexo == "1" ~ "Masculino",
           autor_sexo == "2" ~ "Feminino",
           autor_sexo == "3" ~ "Ambos os sexos",
           TRUE ~ autor_sexo
         ),
         
         ds_autor_alco = case_when(
           autor_alco  == "1" ~ "Sim",
           autor_alco == "2" ~ "Não",
           TRUE ~ autor_alco
         ),
         
         lococor = case_when(
           lococor == "1" ~ "Hospital",
           lococor == "2" ~ "Outros estabelecimentos de saúde",
           lococor == "3" ~ "Domicílio",
           lococor == "4" ~ "Via pública",
           lococor == "5" ~ "Outros",
           lococor == "5" ~ "Aldeia indígena",
           lococor == "9" ~ "Ignorado",
           TRUE ~ lococor
         ),
         
         les_autop = case_when(
           les_autop == "1" ~ "Sim",
           les_autop == "2" ~ "Não",
           les_autop == "9" ~ "Ignorado",
           TRUE ~ les_autop
         ),
         
         out_vezes = case_when(
           out_vezes == "1" ~'Sim',
           out_vezes == "2" ~'Não',
           out_vezes == "9" ~'Ignorado',
           TRUE ~ out_vezes
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
           TRUE ~ local_ocor
         ),
         
         cd_diag_pri = gsub(" ", "", cd_diag_pri)
  ) |> 
  mutate(
    cd_diag_pri = if_else(nchar(cd_diag_pri) == 3,
                          paste0(cd_diag_pri, "0"),
                          cd_diag_pri),
    cd_procedimento = as.numeric(cd_procedimento),
    cd_idade = as.numeric(cd_idade), 
    nu_idade_anos = dplyr::case_when(
      banco == "SINAN_IEXO" & cd_idade < 4001 ~ 0, 
      banco == "SINAN_IEXO" & cd_idade > 4000 & cd_idade < 6000 ~ cd_idade - 4000, 
      TRUE ~ as.numeric(nu_idade_anos)
    )) |> 
  mutate(
    across(c(viol_fisic, viol_psico, viol_tort, viol_sexu, 
             viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), 
           ~ if_else(is.na(.) | . != 1, 0, as.numeric(.)))
  )

# Adicionando os códigos
base_linkage2 <- base_linkage2 |> left_join(id, by=c("id_unico", "banco"))


remover <- base_linkage2 |> group_by(id_pareamento) |> summarise(contagem = n()) |> filter(contagem==1)

# removendo
base_linkage2 <- base_linkage2 |> 
  mutate(id_pareamento = if_else(id_pareamento %in% remover$id_pareamento, NA, id_pareamento))




## CRIANDO FLAGS POR BANCO
# Criando flag de 1 e 0 caso a pessoa contenha o banco do SIM
base_linkage2 <- base_linkage2 |>  group_by(id_pessoa) |> 
  mutate(FL_SIM = sum(banco == "SIM")) |> 
  ungroup() |> 
  mutate(FL_SIM = if_else(FL_SIM > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# Criando flag de 1 e 0 caso a pessoa contenha o banco do SIH
base_linkage2 <- base_linkage2 |>  group_by(id_pessoa) |> 
  mutate(FL_SIH = sum(banco == "SIH")) |> 
  ungroup() |> 
  mutate(FL_SIH = if_else(FL_SIH > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# Criando flag de 1 e 0 caso a pessoa contenha o banco do SINAN_IEXO
base_linkage2 <- base_linkage2 |>  group_by(id_pessoa) |> 
  mutate(FL_SINAN_IEXO = sum(banco == "SINAN_IEXO")) |> 
  ungroup() |> 
  mutate(FL_SINAN_IEXO = if_else(FL_SINAN_IEXO > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# Criando flag de 1 e 0 caso a pessoa contenha o banco do SINAN_VIOL
base_linkage2 <- base_linkage2 |>  group_by(id_pessoa) |> 
  mutate(FL_SINAN_VIOL = sum(banco == "SINAN_VIOL")) |> 
  ungroup() |> 
  mutate(FL_SINAN_VIOL = if_else(FL_SINAN_VIOL > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# FLAG DE ESUS
esus <- id |> 
  filter(
    banco == "ESUS_APS"
  ) |> 
  select(banco, id_pareamento) |> 
  filter(!is.na(id_pareamento))

base_linkage2 <- base_linkage2 |>
  mutate(
    FL_ESUS_APS = if_else(id_pareamento %in% esus$id_pareamento, 1, 0)
  )

# Filtrando apenas sexo feminino
base_linkage2 <- base_linkage2 |> 
  filter(
    sg_sexo == "F"
  )



## COLOCANDO A DESCRICAO DOS CODIGOS DA CID10
# Configurar a conexão ao banco de dados PostgreSQL
con2 <- dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  port = 5432,          # Porta padrão do PostgreSQL
  user = "postgres",
  password = "123",
  dbname = "erik"
)

causa_base <-  dbGetQuery(con2, "
SELECT 
  \"SUBCAT\" as cd_causabas,
  \"DESCRABREV_SUB\" as ds_causabas
FROM tb_cid10")

causa_base<-causa_base |> unique()

base_linkage2 <- base_linkage2 |> left_join(causa_base, by="cd_causabas")


## Colocando a descricao dos SIH
diag_pri<-  dbGetQuery(con2, "
SELECT 
  \"SUBCAT\" as cd_diag_pri,
  \"DESCRABREV_SUB\" as ds_diag_pri
FROM tb_cid10")

diag_pri <- diag_pri |> unique()

diag_pri <- diag_pri |> mutate(
  cd_diag_pri = if_else(nchar(cd_diag_pri) == 3,
                        paste0(cd_diag_pri, "0"),
                        cd_diag_pri))

base_linkage2 <- base_linkage2 |> left_join(diag_pri, by="cd_diag_pri")




# Removendo variáveis desnecessárias
rm(remover, id, diag_pri, con, con2, sinan_viol, causa_base)

base_linkage <- base_linkage2 |> 
  group_by(id_pessoa) |> 
  mutate(
    nu_idade_anos_inicial = min(nu_idade_anos)
  ) |> 
  ungroup() |> 
  group_by(id_pessoa) |> 
  mutate(
    fl_viol_fisic = max(viol_fisic),
    fl_viol_psico = max(viol_psico),
    fl_viol_tort = max(viol_tort),
    fl_viol_sexu = max(viol_sexu), 
    fl_viol_traf = max(viol_traf), 
    fl_viol_finan = max(viol_finan), 
    fl_viol_negli = max(viol_negli),
    fl_viol_infan = max(viol_infan),
    fl_viol_legal = max(viol_legal),
    fl_viol_outr = max(viol_outr)
  ) |> 
  ungroup()


save(base_linkage, file='dados/base_linkage.RData')
