## consultas para segregacao

# 
library(segregation)
library(dplyr)
library(DBI)

source('../atlas_estado/ATLAS_2024/conectar.R')

query1 <- paste(
  "SELECT ano, sexo, sexo_descricao, substring(cbo_base,1,4) as cbo_familia,
count(*) as total_vinculos
FROM vinculos_v6_auxiliar.v_publico
where ano in (1985,1995,2003,2010,2015,2021)
group by ano, sexo, sexo_descricao, substring(cbo_base,1,4) as cbo_familia")


df1 <- DBI::dbGetQuery(con, query1)