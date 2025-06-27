library(reshape2)
library(vitaltable)
#library(data.table)
library(dplyr)
library(tidyverse)
library(janitor)

source("conectar/conectar.R")

con <- conectar("linkage")

# Todo banco do sinan_viol
sinan_viol <- dbGetQuery(con, "SELECT * FROM tratado_sinan_viol")

# Todo banco do sinan_iexo
sinan_iexo <- dbGetQuery(con, "SELECT * FROM tratado_sinan_iexo")

# Todo banco do sim
sim <- dbGetQuery(con, "SELECT * FROM tratado_sim")

# Todo banco do SIH
sih <- dbGetQuery(con, "SELECT * FROM tratado_sih")


sinan_iexo <- sinan_iexo |> vitallinkage::as_char()

# base para teste da linah da vida
linha_vida <- bind_rows(sinan_iexo |> vitallinkage::as_char(),
                        sinan_viol|> vitallinkage::as_char(), 
                        sim |> vitallinkage::as_char(), 
                        sih |> vitallinkage::as_char())#, esus)

rm(sih, sim, sinan_iexo)#, sinan_viol)

# Tabela de Id
id <- dbGetQuery(con, "SELECT * FROM registro_linkage")


# Base de Id tratadas
#id <- id |> 
#  filter(banco != "ESUS_APS") |> 
#  group_by(id_pessoa) |>
#  mutate(contagem = n()) |> 
#  mutate(
#    id_pareamento = case_when(
#      contagem == 1 ~ NA,
#      TRUE ~ id_pareamento  # mantém o valor original se contagem > 1
#    )
#  ) |>
#  ungroup() 




#rm(linha_vida2)


# Criando a base de linha da vida, seleção de variáveis e tratamento
linha_vida2 <- linha_vida |>
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



linha_vida2 <- linha_vida2 |> left_join(id, by=c("id_unico", "banco"))


## REMOVENDO PAREAMENTOS COM APENAS UM REGISTRO DEVIDO O RECORTE NO TEMPO
# para remover o id_pareamento unico
remover <- linha_vida2 |> group_by(id_pareamento) |> summarise(contagem = n()) |> filter(contagem==1)
# removendo
linha_vida2 <- linha_vida2 |> 
  mutate(id_pareamento = if_else(id_pareamento %in% remover$id_pareamento, NA, id_pareamento))

## CRIANDO FLAGS POR BANCO
# Criando flag de 1 e 0 caso a pessoa contenha o banco do SIM
linha_vida2 <- linha_vida2 |>  group_by(id_pessoa) |> 
  mutate(FL_SIM = sum(banco == "SIM")) |> 
  ungroup() |> 
  mutate(FL_SIM = if_else(FL_SIM > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# Criando flag de 1 e 0 caso a pessoa contenha o banco do SIH
linha_vida2 <- linha_vida2 |>  group_by(id_pessoa) |> 
  mutate(FL_SIH = sum(banco == "SIH")) |> 
  ungroup() |> 
  mutate(FL_SIH = if_else(FL_SIH > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# Criando flag de 1 e 0 caso a pessoa contenha o banco do SINAN_IEXO
linha_vida2 <- linha_vida2 |>  group_by(id_pessoa) |> 
  mutate(FL_SINAN_IEXO = sum(banco == "SINAN_IEXO")) |> 
  ungroup() |> 
  mutate(FL_SINAN_IEXO = if_else(FL_SINAN_IEXO > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())

# Criando flag de 1 e 0 caso a pessoa contenha o banco do SINAN_VIOL
linha_vida2 <- linha_vida2 |>  group_by(id_pessoa) |> 
  mutate(FL_SINAN_VIOL = sum(banco == "SINAN_VIOL")) |> 
  ungroup() |> 
  mutate(FL_SINAN_VIOL = if_else(FL_SINAN_VIOL > 0, 1, 0)) |> 
  select(id_unico, id_pessoa, id_pareamento, starts_with('FL_'), banco, everything())


# Supondo que seu dataframe se chama linha_vida2
linha_vida2 <- linha_vida2 %>%
  group_by(id_pessoa) %>%
  mutate(sexo_observado = case_when(
    any(sg_sexo == "F") ~ "F",
    any(sg_sexo == "M") ~ "M",
    TRUE ~ "I"
  )) %>%
  ungroup()



# Filtrando apenas sexo feminino
linha_vida2 <- linha_vida2 |> 
  filter(
    sexo_observado == "F"
  )


## COLOCANDO A DESCRICAO DOS CODIGOS DA CID10
# Configurar a conexão ao banco de dados PostgreSQL

causa_base <-  dbGetQuery(con, "
                        SELECT 
                          \"SUBCAT\" as cd_causabas,
                          \"DESCRABREV_SUB\" as ds_causabas
                        FROM cid10")

causa_base<-causa_base |> unique()

linha_vida2 <- linha_vida2 |> left_join(causa_base, by="cd_causabas")


## Colocando a descricao dos SIH
diag_pri<-  dbGetQuery(con, "
                          SELECT 
                            \"SUBCAT\" as cd_diag_pri,
                            \"DESCRABREV_SUB\" as ds_diag_pri
                          FROM cid10")

diag_pri <- diag_pri |> unique()

diag_pri <- diag_pri |> mutate(
  cd_diag_pri = if_else(nchar(cd_diag_pri) == 3,
                        paste0(cd_diag_pri, "0"),
                        cd_diag_pri))

linha_vida2 <- linha_vida2 |> left_join(diag_pri, by="cd_diag_pri")

# Removendo variáveis desnecessárias
rm(remover, id, diag_pri, sinan_viol, causa_base)

# Adicionando tipo de procedimento
dic_procedimentos_sih<-  dbGetQuery(con, "
                            SELECT 
                              *
                            FROM dic_proc_sih")

dic_procedimentos_sih <- dic_procedimentos_sih |> 
  mutate(
    cd_procedimento = as.numeric(cd_procedimento)
  ) |> 
  select(cd_procedimento, ds_procedimento)

linha_vida2 <- linha_vida2 |> left_join(dic_procedimentos_sih, by = "cd_procedimento")


### BASE DE TEXTOS
# Criando base de dados com os textos para a linha da vida
linha_vida3 <- linha_vida2 |> 
  mutate(banco = case_when(
    banco == "SIM" ~ "SIM",
    banco == "SIH" ~ "SIH",
    banco == "SINAN_IEXO" ~ "SINAN intox exog",
    banco == "SINAN_VIOL" ~ "SINAN Violências"
  
  )) |> 
  filter(FL_SINAN_VIOL == 1) |> 
  mutate(
    # Violências
    viol_sexu_txt = ifelse(viol_sexu == 1, "Violência sexual", NA),
    viol_fisic_txt = ifelse(viol_fisic == 1, "Violência física", NA),
    viol_psico_txt = ifelse(viol_psico == 1, "Violência psicológica", NA),
    viol_tort_txt = ifelse(viol_tort == 1, "Tortura", NA),
    viol_traf_txt = ifelse(viol_traf == 1, "Tráfico humano", NA),
    viol_finan_txt = ifelse(viol_finan == 1, "Violência financeira", NA), 
    viol_negli_txt = ifelse(viol_negli == 1, "Negligência", NA),
    viol_infan_txt = ifelse(viol_infan == 1, "Violência infantil", NA),
    viol_legal_txt = ifelse(viol_legal == 1, "Violência legal", NA),
    viol_outr_txt = ifelse(viol_outr == 1, "Outro tipo de violência", NA),
    dt_evento_inicio = as.Date(dt_evento_inicio), # Transformando dt_comum em formato de data
    # Procedimentos
    proc_dst_txt = ifelse(proc_dst == 1, "IST", NA),
    proc_hiv_txt = ifelse(proc_hiv == 1, "HIV", NA),
    proc_hepb_txt = ifelse(proc_hepb == 1, "Hepatite B", NA),
    proc_sang_txt = ifelse(proc_sang == 1, "Coleta de sangue", NA),
    proc_semen_txt = ifelse(proc_semen == 1, "Coleta de sêmen", NA),
    proc_vagin_txt = ifelse(proc_vagin == 1, "Coleta de secreção vaginal", NA),
    proc_contr_txt = ifelse(proc_contr == 1, "Contracepção de emergência", NA),
    proc_abort_txt = ifelse(proc_abort == 1, "Aborto previsto em lei", NA),
    # Encaminhamentos
    rede_enc_sau_txt = ifelse(rede_enc_sau == 1, 'Rede da saúde', NA),
    assit_soc_creas_txt = ifelse(assit_soc_creas == 1, 'Rede assistência social', NA),
    rede_educa_txt = ifelse(rede_educa == 1, "Rede educação", NA),
    atend_enc_mulh_txt = ifelse(atend_enc_mulh == 1, "Rede atendimento à mulher", NA),
    cons_enc_tutela_txt = ifelse(cons_enc_tutela == 1, "Conselho Tutelar", NA),
    cons_ido_txt = ifelse(cons_ido == 1, "Conselho do Idoso", NA),
    deleg_idos_txt = ifelse(deleg_idos == 1, "Delegacia de atendimento ao idoso", NA),
    dir_human_txt = ifelse(dir_human == 1, "centro de referência dos direitos Humanos", NA),
    mpu_enc_mpu_txt = ifelse(mpu_enc_mpu == 1, "Ministério público", NA),
    deleg_enc_cria_txt = ifelse(deleg_enc_cria == 1, "Delegacia Especializada de Proteção à Criança e Adolescente", NA),
    deleg_enc_mulh_txt = ifelse(deleg_enc_mulh == 1, "Delegacia de atendimento à mulher", NA),
    deleg_enc_deleg_txt = ifelse(deleg_enc_deleg == 1, "Outras delegacias", NA),
    infan_enc_juv_txt = ifelse(infan_enc_juv == 1, "Justiça da infância e da juventude", NA),
    defen_publ_txt = ifelse(defen_publ == 1, "Defensoria pública", NA),
    # Relação com agressor
    rel_pai_txt = ifelse(rel_pai == 1, 'Pai', NA),
    rel_mae_txt = ifelse(rel_mae == 1, 'Mãe', NA),
    rel_pad_txt = ifelse(rel_pad == 1, 'Padrasto', NA),
    rel_mad_txt = ifelse(rel_mad == 1, 'Madrasta', NA),
    rel_conj_txt = ifelse(rel_conj == 1, 'Cônjuge', NA),
    rel_excon_txt = ifelse(rel_excon == 1, 'Ex-cônjuge', NA),
    rel_namo_txt = ifelse(rel_namo == 1, 'Namorado(a)', NA),
    rel_exnam_txt = ifelse(rel_exnam == 1, 'Ex-Namorado(a)', NA),
    rel_filho_txt = ifelse(rel_filho == 1, 'Filho ou filha', NA),
    rel_irmao_txt = ifelse(rel_irmao == 1, 'Irmão ou irmã', NA),
    rel_conhec_txt = ifelse(rel_conhec == 1, 'Amigo(s)/Conhecido(s)', NA),
    rel_desco_txt = ifelse(rel_desco == 1, 'Desconhecido(s)', NA),
    rel_cuida_txt = ifelse(rel_cuida == 1, 'Cuidador', NA),
    rel_patrao_txt = ifelse(rel_patrao == 1, 'Patrão/chefe', NA),
    rel_inst_txt = ifelse(rel_inst == 1, 'Pessoa com relação institucional', NA),
    rel_pol_txt = ifelse(rel_pol == 1, 'Policial/Agente da lei', NA),
    rel_propri_txt = ifelse(rel_propri == 1, 'Própria pessoa', NA),
    rel_outros_txt = ifelse(rel_outros == 1, 'Outro tipo de relação', NA),
    # Forma de agressão
    ag_forca_txt = ifelse(ag_forca == 1, 'Força corporal / espancamento', NA),
    ag_enfor_txt = ifelse(ag_enfor == 1, 'Enforcamento / sufocação', NA),
    ag_objeto_txt = ifelse(ag_objeto == 1, 'Objeto contundente', NA),
    ag_corte_txt = ifelse(ag_corte == 1, 'Objeto cortante / perfurante', NA),
    ag_quente_txt = ifelse(ag_quente == 1, 'Substância quente', NA),
    ag_enven_txt = ifelse(ag_enven == 1, 'Envenenamento', NA),
    ag_fogo_txt = ifelse(ag_fogo == 1, 'Arma de fogo', NA),
    ag_ameaca_txt = ifelse(ag_ameaca == 1, 'Ameaça', NA),
    ag_outros_txt = ifelse(ag_outros == 1, 'Outros meios', NA),
    # Deficiência
    def_trans_txt = ifelse(def_trans == 1, 'Transtorno mental ou comportamental', NA),
    def_fisica_txt = ifelse(def_fisica == 1, 'Deficiência física', NA),
    def_mental_txt = ifelse(def_mental == 1, 'Deficiência mental / intelectual', NA),
    def_visual_txt = ifelse(def_visual == 1, 'Deficiência visual', NA),
    def_auditi_txt = ifelse(def_auditi == 1, 'Deficiência auditiva', NA),
    def_out_txt = ifelse(def_out == 1, 'Outras deficiências', NA),
    def_espec_txt = ifelse(def_espec == 1, 'Deficiência não especificada', NA)
    
  ) |>
  group_by(id_pessoa) |>
  summarise(
    violencias_sofridas = paste(na.omit(unique(c(
      viol_sexu_txt, viol_fisic_txt, viol_psico_txt, viol_tort_txt, viol_traf_txt, viol_finan_txt, 
      viol_negli_txt, viol_infan_txt, viol_legal_txt, viol_outr_txt
    ))), collapse = ", "),
    
    procedimentos = paste(na.omit(unique(c(
      proc_dst_txt, proc_hiv_txt, proc_hepb_txt, proc_sang_txt, proc_semen_txt, proc_vagin_txt,
      proc_contr_txt, proc_abort_txt
    ))), collapse = ", "),
    
    encaminhamentos = paste(na.omit(unique(c(
      rede_enc_sau_txt, assit_soc_creas_txt, rede_educa_txt, atend_enc_mulh_txt, cons_enc_tutela_txt,
      cons_ido_txt, deleg_idos_txt, dir_human_txt, mpu_enc_mpu_txt, deleg_enc_cria_txt, deleg_enc_mulh_txt,
      deleg_enc_deleg_txt, infan_enc_juv_txt, defen_publ_txt
    ))), collapse = ", "),
    
    rel_agressor = paste(na.omit(unique(c(
      rel_pai_txt, rel_mae_txt, rel_pad_txt, rel_mad_txt, rel_conj_txt, rel_excon_txt, rel_namo_txt, rel_exnam_txt,
      rel_filho_txt, rel_irmao_txt, rel_conhec_txt, rel_desco_txt, rel_cuida_txt, rel_patrao_txt, rel_inst_txt,
      rel_pol_txt, rel_propri_txt, rel_outros_txt
    ))), collapse = ", "),
    
    meio_agressao = paste(na.omit(unique(c(
      ag_forca_txt, ag_enfor_txt, ag_objeto_txt, ag_corte_txt, ag_quente_txt, ag_enven_txt, ag_fogo_txt, ag_ameaca_txt, ag_outros_txt
    ))), collapse = ", "),
    
    deficiencias = paste(na.omit(unique(c(
               def_trans_txt, def_fisica_txt, def_mental_txt, def_visual_txt, def_auditi_txt, def_out_txt, def_espec_txt                                   
    ))), collapse = ", "),
    
    hospitalizacoes = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_diag_pri)), collapse = ", ")), side = "both"),
    procedimentos_sih = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_procedimento)), collapse = ", ")), side = "both"),
    
    lesao_autop = str_trim(gsub("^,|,$", "", paste(na.omit(unique(les_autop)), collapse = ", ")), side = "both"),
    outras_vezes = str_trim(gsub("^,|,$", "", paste(na.omit(unique(out_vezes)), collapse = ", ")), side = "both"),
    local_ocorrencia = str_trim(gsub("^,|,$", "", paste(na.omit(unique(local_ocor)), collapse = ", ")), side = "both"),
    
    repet_banco = paste(names(table(banco)), table(banco), sep = ": ", collapse = "<br>"),
    causa_obito = paste(na.omit(unique(ds_causabas)), collapse = ""),
    local_ocorrencia_obito = paste(na.omit(unique(sort(lococor[banco %in% c("SIM")]))), collapse = ", "),
    bair_residencia = paste(na.omit(unique(ds_bairro_res)), collapse = ", "),
    data_obito = paste(na.omit(unique(format(dt_evento_inicio[banco %in% c('SIM')], "%d/%m/%Y"))), collapse = ", "),
    data_viols = paste(na.omit(unique(format(sort(dt_evento_inicio[banco %in% c('SINAN Violências')]), "%d/%m/%Y"))), collapse = ", "),
    data_hosp = paste(na.omit(unique(format(sort(dt_evento_inicio[banco %in% c('SIH')]), "%d/%m/%Y"))), collapse = ", "),
    data_iexo = paste(na.omit(unique(format(sort(dt_evento_inicio[banco %in% c('SINAN intox exog')]), "%d/%m/%Y"))), collapse = ", "),
    tipos_agente_tox = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_agente_tox)), collapse = ", ")), side = "both"),
    
    ds_loc_expo_iexo = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_loc_expo)), collapse = ", "))),
    ds_classi_fin_iexo = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_classi_fin)), collapse = ", "))),
    ds_evolucao_iexo = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_evolucao)), collapse = ", "))),
    ds_tp_atendimento_iexo = str_trim(gsub("^,|,$", "", paste(na.omit(unique(ds_tpatend)), collapse = ", ")))#,
    
    
    
    # tipos_penais_ob = paste(na.omit(unique(sort(ds_tp_obito[banco %in% c('Boletins de Ocorrência Letais', "Boletins de Ocorrência")]))), collapse = ", "),
    # meio_empregado = paste(na.omit(unique(sort(ds_meio_emp[banco %in% c('Boletins de Ocorrência Letais', "Boletins de Ocorrência")]))), collapse = ", ")
    
  ) |>
  ungroup() |>
  mutate(
    violencias_sofridas = ifelse(violencias_sofridas == "", NA, violencias_sofridas),
    procedimentos = ifelse(procedimentos == "", NA, procedimentos),
    encaminhamentos = ifelse(encaminhamentos == "", NA, encaminhamentos),
    rel_agressor = ifelse(rel_agressor == "", NA, rel_agressor),
    meio_agressao = ifelse(meio_agressao == "", NA, meio_agressao),
    deficiencias = ifelse(deficiencias == "", NA, deficiencias),
    lesao_autop = ifelse(lesao_autop == "", NA, lesao_autop),
    outras_vezes = ifelse(outras_vezes == "", NA, outras_vezes),
    local_ocorrencia = ifelse(local_ocorrencia == "", NA, local_ocorrencia),
    hospitalizacoes = ifelse(hospitalizacoes == "", NA, hospitalizacoes),
    procedimentos_sih = ifelse(procedimentos_sih == "", NA, procedimentos_sih),
    causa_obito = ifelse(causa_obito == "", NA, causa_obito),
    local_ocorrencia_obito = ifelse(local_ocorrencia_obito == "", NA, local_ocorrencia_obito),
    data_obito = ifelse(data_obito == "", NA, data_obito),
    data_viols = ifelse(data_viols == "", NA, data_viols),
    data_hosp = ifelse(data_hosp == "", NA, data_hosp),
    data_iexo = ifelse(data_iexo == "", NA, data_iexo),
    tipos_agente_tox = ifelse(tipos_agente_tox == "", NA, str_to_title(tipos_agente_tox)),
    ds_loc_expo_iexo = ifelse(ds_loc_expo_iexo == "", NA, str_to_title(ds_loc_expo_iexo)),
    ds_classi_fin_iexo = ifelse(ds_classi_fin_iexo == "", NA, str_to_title(ds_classi_fin_iexo)),
    ds_evolucao_iexo = ifelse(ds_classi_fin_iexo == "", NA, str_to_title(ds_evolucao_iexo)),
    ds_tp_atendimento_iexo = ifelse(ds_tp_atendimento_iexo == "", NA, str_to_title(ds_tp_atendimento_iexo)),
    
    # tipos_penais_ob = ifelse(tipos_penais_ob == "", NA, str_to_title(tipos_penais_ob)),
    # meio_empregado  = ifelse(meio_empregado  == "", NA, str_to_title(meio_empregado )),
    
    # Texto
    texto_final = paste(
      ifelse(!is.na(id_pessoa), paste("<b>Código identificador:</b>", id_pessoa, "<br>"), ""),
      ifelse(!is.na(bair_residencia), paste("<b>Bairro(s) de residência:</b>", bair_residencia, "<br>"), ""),
      ifelse(!is.na(repet_banco), paste("<b><br>Número de vezes no sistema:</b><br>", repet_banco, "<br>"), ""),
      ifelse(!is.na(data_viols), paste("<b><br>Datas de ocorrência (SINAN Violências):</b>", data_viols, "<br>"), ""),
      ifelse(!is.na(violencias_sofridas), paste("<b>Violências sofridas (SINAN Violências):</b>", violencias_sofridas, "<br>"), ""),
      ifelse(!is.na(procedimentos), paste("<b>Procedimentos realizados (SINAN Violências):</b>", procedimentos, "<br>"), ""),
      ifelse(!is.na(encaminhamentos), paste("<b>Encaminhamentos (SINAN Violências):</b>", encaminhamentos, "<br>"), ""),
      ifelse(!is.na(rel_agressor), paste("<b>Relação com o agressor (SINAN Violências):</b>", rel_agressor, "<br>"), ""),
      ifelse(!is.na(meio_agressao), paste("<b>Meio da agressão (SINAN Violências):</b>", meio_agressao, "<br>"), ""),
      ifelse(!is.na(deficiencias), paste("<b>Deficiência (SINAN Violências):</b>", deficiencias, "<br>"), ""),
      ifelse(!is.na(lesao_autop), paste("<b>Lesão autoprovocada (SINAN Violências):</b>", lesao_autop, "<br>"), ""),
      ifelse(!is.na(outras_vezes), paste("<b>Outras vezes (SINAN Violências):</b>", outras_vezes, "<br>"), ""),
      ifelse(!is.na(local_ocorrencia), paste("<b>Local de ocorrência (SINAN Violências):</b>", local_ocorrencia, "<br>"), ""),
      
      ifelse(!is.na(data_iexo), paste("<b><br>Datas de ocorrência (SINAN Intoxicação Exogena):</b>", data_iexo, "<br>"), ""),
      ifelse(!is.na(ds_loc_expo_iexo), paste("<b>Local de ocorrência da exposição (SINAN Intoxicação Exogena):</b>", ds_loc_expo_iexo, "<br>"), ""),
      ifelse(!is.na(tipos_agente_tox), paste("<b>Tipo de agente intoxicante (SINAN Intoxicaação Exogena):</b>", tipos_agente_tox, "<br>"), ""),
      ifelse(!is.na(ds_tp_atendimento_iexo), paste("<b>Tipo de atendimento (SINAN Intoxicaação Exogena):</b>", ds_tp_atendimento_iexo, "<br>"), ""),
      ifelse(!is.na(ds_classi_fin_iexo), paste("<b>Classificação final (SINAN Intoxicaação Exogena):</b>", ds_classi_fin_iexo, "<br>"), ""),
      ifelse(!is.na(ds_evolucao_iexo), paste("<b>Evolução do caso (SINAN Intoxicaação Exogena):</b>", ds_evolucao_iexo, "<br>"), ""),
      
      ifelse(!is.na(data_hosp), paste("<b><br>Hospitalizações (SIH):</b>", data_hosp, "<br>"), ""),
      ifelse(!is.na(hospitalizacoes), paste("<b>Diagnóstico primário (SIH):</b>", hospitalizacoes, "<br>"), ""),
      ifelse(!is.na(procedimentos_sih), paste("<b>Procedimentos realizados (SIH):</b>", procedimentos_sih, "<br>"), ""),
      
      ifelse(!is.na(data_obito), paste("<b><br>Data de óbito (SIM):</b>", data_obito, "<br>"), ""),
      ifelse(!is.na(causa_obito), paste("<b>Causa do óbito (SIM):</b>", causa_obito, "<br>"), ""),
      ifelse(!is.na(local_ocorrencia_obito), paste("<b>Local de ocorrência do óbito (SIM):</b>", local_ocorrencia_obito, "<br>"), ""),
      
      #ifelse(!is.na(tipos_penais_ob), paste("<b>Tipo Penal (BO letal):</b>", tipos_penais_ob, "<br>"), ""),
      #ifelse(!is.na(meio_empregado), paste("<b>Meio Empregado (BO letal):</b>", meio_empregado, "<br><br>"), ""),
      
      sep = ""
    ) |> str_trim()
  )


# Criando a base de linha da vida
df_linha_vida_final <- linha_vida2 |> 
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
    fl_viol_outr = max(viol_outr),
    
  ) |> 
  ungroup() |> 
  select(-viol_fisic, -viol_psico, -viol_tort, -viol_sexu, 
         -viol_traf, -viol_finan, -viol_negli, -viol_infan, -viol_legal, -viol_outr
  )

# Selecionando as variáveis finais
df_linha_vida_final <- df_linha_vida_final |> 
  select( id_unico,
          id_pareamento, id_pessoa, ds_raca, sg_sexo, nu_idade_anos, banco, ano, les_autop,
          FL_SIH, FL_SINAN_IEXO, FL_SINAN_VIOL, FL_SIM, ds_bairro_res,
          banco,  ## TIPO DE VIOLÊNCIA 
          fl_viol_fisic, fl_viol_psico, fl_viol_tort, fl_viol_sexu, fl_viol_traf, ,
          fl_viol_finan, fl_viol_negli, 
          fl_viol_infan, fl_viol_legal, fl_viol_outr, 
          dt_evento_inicio, dt_evento_fim, dt_registro
  )


linha_vida3 <- linha_vida3 |>
  select(id_pessoa, texto_final) |> 
  left_join(df_linha_vida_final, by="id_pessoa")

# Padronizando a raça
linha_vida3 <- linha_vida3 |> 
  group_by(id_pessoa) |> 
  mutate(
    ds_raca_padronizada = case_when(
      any(ds_raca == "Indígena") ~ "Indígena",
      any(ds_raca == "Amarela") ~ "Amarela",
      any(ds_raca == "Preta") ~ "Preta",
      any(ds_raca == "Parda") ~ "Parda",
      any(ds_raca == "Branca") ~ "Branca",
      any(ds_raca == "Ignorada") ~ "Ignorada",
      TRUE ~ NA_character_
    )
  ) |> 
  ungroup()

linha_vida3 <- linha_vida3 |> 
  filter(id_pessoa %notin% c(
    850612, 210156, 282772, 334744, 344109, 373583, 
    944845, 1020323, 1021868, 1024157, 1042568, 1044833,
    1691194, 14326740, 14326820, 14326932, 14326944,
    14326987, 14327253, 14327300, 14327853, 14327855, 
    14327865, 14976559, 14978405, 15000785, 15003413, 
    15006035, 15006469, 15006561, 15702224, 15718935,
    15718943, 15719025, 15720078, 17397073)) |> 
  mutate(
    sg_sexo = "F"
  )

#load("dados/linha_vida3.RData")

df_linha_vida <- linha_vida3 |> 
  mutate(dt_comum = ymd(dt_registro))



df_linha_vida <- df_linha_vida |> 
  group_by(id_pessoa) |>
  mutate(
    fl_les_autop = ifelse(any(les_autop == "Sim"), 1, 0)
  ) |> 
  ungroup() |> 
  mutate(
    fl_les_autop = ifelse(is.na(fl_les_autop), 0, fl_les_autop)
  )

save(df_linha_vida, file = "dados/linha_vida3.RData")


# Verificar se a conexão foi estabelecida com sucesso
if (dbIsValid(con)) {
  print("Conexão estabelecida com sucesso!")
} else {
  print("Falha na conexão ao banco de dados.")
}

# Para desconectar quando terminar de usar o banco de dados
# dbDisconnect(con)


# Escrever o dataframe df no banco de dados como uma tabela chamada "tratado_sih"
dbWriteTable(con, "base_linha_vida_sem_esus", linha_vida3, overwrite = TRUE, row.names = FALSE)



sih <- dbGetQuery(con, "SELECT id_unico, ds_nome_pac, dt_nasc, ds_nome_mae,sg_sexo, nu_cns FROM tratado_sih")
sinan <- dbGetQuery(con, "SELECT id_unico, ds_nome_pac, dt_nasc, ds_nome_mae, sg_sexo,nu_cns FROM tratado_sinan_viol")
sinan_iexo <- dbGetQuery(con, "SELECT id_unico, ds_nome_pac, dt_nasc, ds_nome_mae, sg_sexo,nu_cns FROM tratado_sinan_iexo")
sim <- dbGetQuery(con, "SELECT id_unico, ds_nome_pac, dt_nasc, ds_nome_mae, sg_sexo,nu_cns FROM tratado_sim")
