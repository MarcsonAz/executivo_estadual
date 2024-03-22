

diretorio_trabalho = "//srjn4/atlas/vinculos_estaduais/"
setwd(diretorio_trabalho)

# Pacotes ----

library(dplyr)
library(plotly)

# Dados ----

# Cria a lista que irá receber os data frames
df <- list()

## Conexão ----

# Faz a conexão com o banco de dados
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "rais_2019",
                      host = "psql10-df",
                      port = 5432,
                      user = keyring::key_get("id_ipea"),
                      password = keyring::key_get("pw_ipea_psql10-df"))

query <- paste(
  "SELECT ano, codigo, uf,",
  "estadual_total, vinculos_legislativo_estadual, vinculos_executivo_estadual, vinculos_judiciario_estadual",
  "FROM vinculos_v6_resumos.uf_v12_esfera_e_poder")

df$original <- DBI::dbGetQuery(con, query)
rm(con, query)

## Tratamento ----

View(df$original)
dim(df$original)

sum(is.na(df$original$uf))




## gráfico de linha


### cores 

vetor_uf = df$original %>% 
  select(codigo) %>% 
  pull() %>% 
  unique() %>% 
  sort()

vetor_cores = viridis::magma(length(vetor_uf))


df$cores <- tibble(codigo = vetor_uf, vetor_cores = vetor_cores)

df$tratado = left_join(df$original,df$cores) %>% 
  dplyr::filter(!is.na(codigo))


### 




df$tratado %>% 
  
  plot_ly(x=~ano, y=~estadual_total, group=~uf,
          type="scatter",color=~uf,
          colors = ,
          mode="lines+markers")



query <- paste(
  "SELECT ano, trunc(codemun_6/10000) as codigo, sum(populacao) as populacao",
  "FROM ibge_populacao.tb_populacao",
  "GROUP BY ano, trunc(codemun_6/10000)")

df$populacao <- DBI::dbGetQuery(con, query)
rm(con, query)

## Tratamento ----

sort(unique(df$populacao$codigo))


df$populacao <- df$populacao %>% 
  dplyr::filter(ano >= 1985 & ano <=2021)
  
  
dim(df$populacao)
  