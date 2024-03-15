

# tabulacao inicial - 15/03/2024

require(dplyr)
require(openxlsx)
require(tidyr)
require(stringr)
require(dplyr)
require(dplyr)

diretorio_trabalho = "//srjn4/atlas/vinculos_estaduais/"
setwd(diretorio_trabalho)

# tabela fonte 1: vinculos_v6_resumos.brasil_v12_esfera_x_poder
# tabela fonte 2: vinculos_v6_resumos.brasil_v12_corxsexoxpoderxesfera
# tabela fonte 3: public.brasil_ee_sexo_85_03
# tabela fonte 4: public.brasil_ee_decis
# tabela fonte 5: vinculos_v6_resumos.uf_v11_genero_poder_esfera
# 

# gerar tabelas e colocar numa planilha


# conectar

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "rais_2019",
                      host = "psql10-df",
                      port = 5432,
                      user = keyring::key_get("id_ipea"),
                      password = keyring::key_get("pw_ipea_psql10-df"))

DBI::dbGetInfo(con)$host == "psql10-df"

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

# tabular

tabela <- list()

tabela$total_brasil = tibble(
  ano = df1$ano,
  vinculos_executivo_estadual = df1$vinculos_executivo_estadual
)

tabela$total_brasil_sexo = df2 %>% 
  group_by(ano) %>% 
  summarise(
    executivo_estadual_feminino = sum(executivo_estadual_feminino),
    executivo_estadual_masculino = sum(executivo_estadual_masculino)
  ) %>% 
  ungroup()

tabela$total_brasil_cor = df3 %>% 
  filter(!is.na(raca_script_r_resultado)) %>% 
  select(ano,cor = raca_script_r_resultado,total_vinculos_publicos)

# rem brasil

# decis brasil

# total sexo

tabela$total_uf_sexo = df2

# total cor

# rem uf

# decis uf


# dados:

# Panorama Brasil:
# Total de vínculos anuais no Brasil entre 1985 e 2021
# Total de vínculos anuais no Brasil, por sexo, entre 1985 e 2021
# Total de vínculos anuais no Brasil, por cor, entre 2004 e 2021
# Remuneração média mensal no ano no Brasil entre 1985 e 2021
# Decis da remuneração média mensal no ano dos vínculos no Brasil entre 1985 e 2021
# 
# Por Unidade da Federação:
# Total de vínculos anuais por sexo e UF entre 1985 e 2021
# Total de vínculos anuais por cor e UF entre 2004 e 2021
# Remuneração média mensal no ano, por UF entre 1985 e 2021
# Decis da remuneração média mensal no ano dos vínculos, por UF entre 1985 e 2021


# planilha de dados

## Create a new workbook
wb <- createWorkbook()


for(tab in names(tabela)){
  addWorksheet(wb, tab)
  writeData(wb, tab, tabela[[tab]], rowNames = TRUE)
}


# exportar
## Save workbook to working directory
saveWorkbook(wb, file = "planilha_dados.xlsx", overwrite = TRUE)


##  dados em construção - 

# uf total cor - EE - vinculos_v6_resumos.uf_v12_corxpoderxesfera
# uf rem decis - EE - vinculos_v6_resumos.uf_v12_decis_poderxesfera
# uf rem media - EE - vinculos_v6_resumos.uf_v12_poderxesfera
