library(tidyverse)
library(tidyverse)
source("conectar/conectar.R")

# Estabelecendo conexão
con <- conectar('linkage')

# Configurar conexão ao banco de dados PostgreSQL

### SINAN VIOLENCIAS
df_sinan <- dbGetQuery(con, "
        SELECT id_unico, ds_raca, nu_idade_anos, autor_alco,autor_sexo,out_vezes, local_ocor, ano,
               viol_fisic, viol_psico, viol_sexu, viol_tort, viol_negli,
               viol_finan, viol_infan, viol_legal,viol_traf,viol_outr, les_autop,
               proc_dst, proc_hiv, proc_hepb, proc_sang, proc_semen, proc_vagin, proc_contr, proc_abort,
               rel_pai, rel_mae,  rel_pad, rel_mad, rel_conj, rel_excon, rel_namo,rel_exnam, rel_filho, rel_irmao,
               rel_conhec, rel_desco, rel_cuida, rel_patrao, rel_inst, rel_pol, rel_propri, rel_outros,
               rede_educa, cons_ido, deleg_idos,  dir_human, defen_publ, rede_sau, enc_saude,assist_soc,enc_creas, atend_mulh, enc_mulher, cons_tutel, enc_tutela, mpu,
               enc_mpu, deleg_cria, enc_dpca, deleg_mulh, enc_deam, deleg, enc_deleg, infan_juv, enc_vara,
               ag_forca, ag_enfor, ag_objeto, ag_corte, ag_quente, ag_enven,  ag_fogo, ag_ameaca, ag_outros,
               def_trans, def_fisica, def_mental, def_visual, def_auditi, def_out, def_espec,
               tran_ment, tran_comp
        FROM tratado_sinan_viol 
          WHERE ano::NUMERIC BETWEEN 2016 AND 2022
            AND sg_sexo = 'F'
                       ")
# save rdata

# write.csv2(df_sinan, "dados/tela_sinan_viol.csv")

df_sinan <- df_sinan |> mutate(banco = "SINAN")


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
    out_vezes = case_when(
      out_vezes == "1" ~'Sim',
      out_vezes == "2" ~'Não',
      out_vezes == "9" ~'Ignorado',
      TRUE ~ "Ignorado"
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
    )
  )


# Adicionando faixa-etária no SIM
df_sinan <- df_sinan |> 
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

save(df_sinan, file = "dados/df_sinan_viol.RData")


# Tabela para o meio de agressão
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

# Criando o dataframe com as variáveis de deficiência do SINAN Violências
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


