# tabulacao_dados

###############################################
# OBJETIVO                                    #
# COLETA DE DADOS E EXPORTACAO EM PLANILHA    #
#                                             #
###############################################

## tabulacao inicial - 15/03/2024
## tabulacao inicial - 21/03/2024
## revisão de código - 26/06/2024

require(dplyr)
require(openxlsx)
require(tidyr)
require(stringr)


diretorio_trabalho = "//srjn4/atlas/executivo_estadual/"
setwd(diretorio_trabalho)

# tabela fonte 1: vinculos_v6_resumos.brasil_v12_esfera_x_poder
# tabela fonte 2: vinculos_v6_resumos.brasil_v12_corxsexoxpoderxesfera
# tabela fonte 3: public.brasil_ee_sexo_85_03
# tabela fonte 4: public.brasil_ee_decis
# tabela fonte 5: vinculos_v6_resumos.uf_v11_genero_poder_esfera
# tabela fonte 6: vinculos_v6_resumos.brasil_v12_publico_rem_decil_ee

# tabela fonte 7: adeb_resultados.adeb_499_publico_media_quintos_ee_todos

# tabela fonte 8 : adeb_resultados.adeb_499_publico_media_decimos_ee

# tabela fonte 9 : vinculos_v6_resumos.uf_v12_publico_ee_sexo_remuneracao_razao

# tabela fonte 10 : vinculos_v6_resumos.uf_v12_publico_raca_remuneracao


################################################################################
# conectar e coletar
################################################################################

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "rais_2019",
                      host = "psql10-df",
                      port = 5432,
                      user = keyring::key_get("id_ipea"),
                      password = keyring::key_get("pw_ipea_psql10-df"))

identical(DBI::dbGetInfo(con)$host, "psql10-df")

query1 <- paste(
  "SELECT ano,",
  "vinculos_executivo_estadual,",                     
  "vinculos_executivo_estadual_controlado,",               
  "rem_soma_vinculos_executivo_estadual_controlado,",      
  "rem_mediana_vinculos_executivo_estadual_controlado ",
  "FROM vinculos_v6_resumos.brasil_v12_esfera_x_poder")

df1 <- DBI::dbGetQuery(con, query1)

query2 <- paste(
  "SELECT ano,codigo,uf,",
  "executivo_estadual_masculino, executivo_estadual_feminino",                     
  "FROM vinculos_v6_resumos.uf_v11_genero_poder_esfera")

df2 <- DBI::dbGetQuery(con, query2)


query3 <- paste(
  "SELECT *",
  "FROM vinculos_v6_resumos.brasil_v12_corxpoderxesfera",
  "WHERE poder = 'E' and esfera = 'E'")

df3 <- DBI::dbGetQuery(con, query3)

query4 <- paste(
  "SELECT *",
  "FROM vinculos_v6_resumos.uf_v12_corxpoderxesfera",
  "WHERE poder = 'E' and esfera = 'E'")

df4 <- DBI::dbGetQuery(con, query4)


query5 <- paste(
  "SELECT *",
  "FROM vinculos_v6_resumos.uf_v12_publico_rem_decil_ee")

df5 <- DBI::dbGetQuery(con, query5)


query6 <- paste(
  "SELECT *",
  "FROM vinculos_v6_resumos.brasil_v12_publico_rem_decil_ee")

df6 <- DBI::dbGetQuery(con, query6)

query7 <- paste(
  "SELECT *",
  "FROM adeb_resultados.adeb_499_publico_media_quintos_ee_todos")

df7 <- DBI::dbGetQuery(con, query7)


query9 <- paste(
  "SELECT *",
  "FROM vinculos_v6_resumos.uf_v12_publico_ee_sexo_remuneracao_razao")

df9 <- DBI::dbGetQuery(con, query9)

query10 <- paste(
  "SELECT *",
  "FROM vinculos_v6_resumos.uf_v12_publico_raca_remuneracao")

df10 <- DBI::dbGetQuery(con, query10)



################################################################################
# tabulacao
################################################################################

tabela <- list()

## BRASIL

### total brasil
tabela$total_brasil = tibble(
  ano = df1$ano,
  vinculos_executivo_estadual = df1$vinculos_executivo_estadual
) %>% arrange(ano)


### total brasil sexo
tabela$total_brasil_sexo = df2 %>% 
  group_by(ano) %>% 
  summarise(
    vinculos_executivo_estadual_feminino = sum(executivo_estadual_feminino),
    vinculos_executivo_estadual_masculino = sum(executivo_estadual_masculino)
  ) %>% 
  ungroup() %>% 
  as_tibble()


### total brasil cor
tabela$total_brasil_cor = df3 %>% 
  filter(!is.na(raca_script_r_resultado)) %>%
  rename(cor = raca_script_r_resultado,vinculos_executivo_estadual = total_vinculos_publicos) %>% 
  mutate(
    cor = factor(cor),
    cor_descricao = factor(cor,
                           levels = c(1,2,4,6,8),
                           labels = c('Indígena','Branca','Preta','Amarela','Parda')
    )) %>% 
  select(ano,cor,cor_descricao,vinculos_executivo_estadual) %>% 
  arrange(ano) %>% 
  as_tibble()


### rem media e mediana brasil
tabela$rem_media_brasil = tibble(
  ano = df1$ano,
  remuneracao_media_executivo_estadual = round( 
    df1$rem_soma_vinculos_executivo_estadual_controlado/df1$vinculos_executivo_estadual_controlado,4),
  remuneracao_mediana_executivo_estadual = df1$rem_mediana_vinculos_executivo_estadual_controlado
) %>% arrange(ano)


### decis brasil
tabela$rem_decil_brasil = tibble(
  ano = df6$ano,

  remuneracao_decil_1_executivo_estadual = df6$rem_decil1_vinculos_publicos_controlado,
  remuneracao_decil_2_executivo_estadual = df6$rem_decil2_vinculos_publicos_controlado,
  remuneracao_decil_3_executivo_estadual = df6$rem_decil3_vinculos_publicos_controlado,
  remuneracao_decil_4_executivo_estadual = df6$rem_decil4_vinculos_publicos_controlado,
  
  remuneracao_decil_5_executivo_estadual = df6$rem_mediana_vinculos_publicos_controlado,
  
  remuneracao_decil_6_executivo_estadual = df6$rem_decil6_vinculos_publicos_controlado,
  remuneracao_decil_7_executivo_estadual = df6$rem_decil7_vinculos_publicos_controlado,
  remuneracao_decil_8_executivo_estadual = df6$rem_decil8_vinculos_publicos_controlado,
  remuneracao_decil_9_executivo_estadual = df6$rem_decil9_vinculos_publicos_controlado,
  
  remuneracao_razao_decil_9_1_executivo_estadual = round( 
    df6$rem_decil9_vinculos_publicos_controlado/df6$rem_decil1_vinculos_publicos_controlado,4),
  remuneracao_diferenca_decil_9_1_executivo_estadual = round( 
    df6$rem_decil9_vinculos_publicos_controlado - df6$rem_decil1_vinculos_publicos_controlado,4)
) %>% 
  arrange(ano)


### BRASIL REM MEDIA NO DECIL 
tabela$rem_media_quintos_brasil = tibble(
  ano = df7$ano,
  
  remuneracao_media_quintos_1_executivo_estadual = df7$rem_media_vinculos_publicos_ate20,
  remuneracao_media_quintos_2_executivo_estadual = df7$rem_media_vinculos_publicos_20ate40,
  remuneracao_media_quintos_3_executivo_estadual = df7$rem_media_vinculos_publicos_40ate60,
  remuneracao_media_quintos_4_executivo_estadual = df7$rem_media_vinculos_publicos_60ate80,
  remuneracao_media_quintos_5_executivo_estadual = df7$rem_media_vinculos_publicos_apos80,
  
  remuneracao_razao_media_quintos_5_1_executivo_estadual = round( 
    df7$rem_media_vinculos_publicos_apos80/df7$rem_media_vinculos_publicos_ate20,4),
  remuneracao_diferenca_media_quintos_5_1_executivo_estadual = round( 
    df7$rem_media_vinculos_publicos_apos80 - df7$rem_media_vinculos_publicos_ate20,4)
) %>% 
  arrange(ano)



### rem media sexo brasil
tabela$rem_media_brasil_sexo = df9 %>% 
  group_by(ano, sexo, sexo_nome) %>% 
  summarise(total_vinculos = sum(total_vinculos_publicos_controlado),
            rem_soma = sum(rem_soma_vinculos_publicos_controlado)) %>% 
  ungroup() %>% 
  mutate(rem_media = round(rem_soma/total_vinculos,4)) %>% 
  select(ano, sexo_nome, rem_media) %>% 
  pivot_wider(names_from = sexo_nome, values_from = rem_media, 
              names_prefix = "rem_media_") %>% 
  mutate(razao_rem_media = round(rem_media_masculino/rem_media_feminino,4))



### rem media raca brasil
# limpar nao identificado, amarela e indigena
# criar nao_brancos = pretos e pardos

tabela$rem_media_brasil_cor = df10 %>% 
  rename(cor = raca_script_r_resultado) %>% 
  filter(cor %in% c(2,4,8)) %>%
  mutate(cor_branca = case_when(
    cor == 2 ~ 1,
    cor %in% c(4,8) ~ 0,
    .default = NA_integer_
  ),
  cor_descricao = factor(cor_branca,
                           levels = c(0,1),
                           labels = c('negros','brancos'))
  ) %>% 
  group_by(ano, cor_branca, cor_descricao) %>% 
  summarise(total_vinculos = sum(total_vinculos_publicos_controlado),
            rem_soma = sum(rem_soma_vinculos_publicos_controlado)) %>% 
  ungroup() %>% 
  mutate(rem_media = round(rem_soma/total_vinculos,4)) %>% 
  select(ano, cor_descricao, rem_media) %>% 
  pivot_wider(names_from = cor_descricao, values_from = rem_media, 
              names_prefix = "rem_media_") %>% 
  mutate(razao_rem_media = round(rem_media_brancos/rem_media_negros,4))



## UF



### total sexo uf
tabela$total_uf_sexo = tibble(
  ano = df2$ano,
  codigo_uf = df2$codigo, 
  sigla_uf = df2$uf,
  vinculos_executivo_estadual_feminino = df2$executivo_estadual_feminino,
  vinculos_executivo_estadual_masculino = df2$executivo_estadual_masculino
) %>% arrange(ano,codigo_uf)


### total cor uf
tabela$total_uf_cor = df4 %>% 
  filter(!is.na(raca_script_r_resultado)) %>%
  rename(cor = raca_script_r_resultado,vinculos_executivo_estadual = total_vinculos_publicos) %>% 
  mutate(
    cor = factor(cor),
    cor_descricao = factor(cor,
                           levels = c(1,2,4,6,8),
                           labels = c('Indígena','Branca','Preta','Amarela','Parda')
    )) %>% 
  select(ano, codigo_uf = codigo, sigla_uf = uf,cor,cor_descricao,vinculos_executivo_estadual) %>% 
  arrange(ano,codigo_uf)

### rem media e mediana uf
tabela$rem_media_uf = tibble(
  ano = df5$ano,
  codigo_uf = df5$codigo, 
  sigla_uf = df5$uf,
  remuneracao_media_executivo_estadual = round( 
    df5$rem_soma_vinculos_publicos_controlado/df5$total_vinculos_publicos_controlado,4),
  remuneracao_mediana_executivo_estadual = df5$rem_mediana_vinculos_publicos_controlado
) %>% arrange(ano,codigo_uf)


### decis uf
tabela$rem_decil_uf = tibble(
  ano = df5$ano,
  codigo_uf = df5$codigo, 
  sigla_uf = df5$uf,
  remuneracao_decil_1_executivo_estadual = df5$rem_decil1_vinculos_publicos_controlado,
  remuneracao_decil_2_executivo_estadual = df5$rem_decil2_vinculos_publicos_controlado,
  remuneracao_decil_3_executivo_estadual = df5$rem_decil3_vinculos_publicos_controlado,
  remuneracao_decil_4_executivo_estadual = df5$rem_decil4_vinculos_publicos_controlado,
  
  remuneracao_decil_5_executivo_estadual = df5$rem_mediana_vinculos_publicos_controlado,
  
  remuneracao_decil_6_executivo_estadual = df5$rem_decil6_vinculos_publicos_controlado,
  remuneracao_decil_7_executivo_estadual = df5$rem_decil7_vinculos_publicos_controlado,
  remuneracao_decil_8_executivo_estadual = df5$rem_decil8_vinculos_publicos_controlado,
  remuneracao_decil_9_executivo_estadual = df5$rem_decil9_vinculos_publicos_controlado,
  
  remuneracao_razao_decil_9_1_executivo_estadual = round( 
    df5$rem_decil9_vinculos_publicos_controlado/df5$rem_decil1_vinculos_publicos_controlado,4)
) %>% 
  arrange(ano,codigo_uf)


### rem media sexo UF
tabela$rem_media_brasil_sexo_uf = df9 %>% 
  mutate(rem_media = round(rem_soma_vinculos_publicos_controlado/total_vinculos_publicos_controlado,4)) %>% 
  select(ano, uf=uf_ipea, sexo_nome, rem_media) %>% 
  pivot_wider(names_from = sexo_nome, values_from = rem_media, 
              names_prefix = "rem_media_") %>% 
  mutate(razao_rem_media = round(rem_media_masculino/rem_media_feminino,4))


### rem media raca uf
# limpar nao identificado, amarela e indigena
# criar nao_brancos = pretos e pardos

tabela$rem_media_brasil_cor_uf = df10 %>% 
  rename(cor = raca_script_r_resultado) %>% 
  filter(cor %in% c(2,4,8)) %>%
  mutate(cor_branca = case_when(
    cor == 2 ~ 1,
    cor %in% c(4,8) ~ 0,
    .default = NA_integer_
  ),
  cor_descricao = factor(cor_branca,
                         levels = c(0,1),
                         labels = c('negros','brancos'))
  ) %>% 
  group_by(ano, uf=uf_ipea, cor_branca, cor_descricao) %>% 
  summarise(total_vinculos = sum(total_vinculos_publicos_controlado),
            rem_soma = sum(rem_soma_vinculos_publicos_controlado)) %>% 
  ungroup() %>% 
  mutate(rem_media = round(rem_soma/total_vinculos,4)) %>% 
  select(ano, uf, cor_descricao, rem_media) %>% 
  pivot_wider(names_from = cor_descricao, values_from = rem_media, 
              names_prefix = "rem_media_") %>% 
  mutate(razao_rem_media = round(rem_media_brancos/rem_media_negros,4))


#View(tabela$rem_decil_uf)


## dados: ( atualizar lista )

# Panorama Brasil:
# Total de vínculos anuais no Brasil entre 1985 e 2021
# Total de vínculos anuais no Brasil, por sexo, entre 1985 e 2021
# Total de vínculos anuais no Brasil, por cor, entre 2004 e 2021
# Remuneração média mensal no ano no Brasil entre 1985 e 2021
# Decis da remuneração média mensal no ano dos vínculos no Brasil entre 1985 e 2021
#
# 
# Por Unidade da Federação:
# Total de vínculos anuais por sexo e UF entre 1985 e 2021
# Total de vínculos anuais por cor e UF entre 2004 e 2021
# Remuneração média mensal no ano, por UF entre 1985 e 2021
# Decis da remuneração média mensal no ano dos vínculos, por UF entre 1985 e 2021


################################################################################
# planilha de dados para exportar
################################################################################


## Create a new workbook
wb <- createWorkbook()


for(tab in names(tabela)){
  addWorksheet(wb, tab)
  writeData(wb, tab, tabela[[tab]], rowNames = TRUE)
}


## Salvar arquivo em planilha no servidor
saveWorkbook(wb, file = "./dados/planilha_dados.xlsx", overwrite = TRUE)
# C:\Users\b15048941705\Documents\projeto\
#saveWorkbook(wb, file = "C:/Users/b15048941705/Documents/projeto/planilha_dados.xlsx", overwrite = TRUE)

## Atualizar no Google Drive
### verificar autorização

googledrive::drive_put(
  path = googledrive::as_dribble("Burocracias subnacionais"),
  media = "./dados/planilha_dados.xlsx"
)


