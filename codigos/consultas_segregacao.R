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
where ano in (1985,1995)
group by ano, sexo, sexo_descricao, substring(cbo_base,1,4)")


df1 <- DBI::dbGetQuery(con, query1)

df_saida <- rbind(
  df1,
  df2 %>% 
    group_by(ano,sexo,sexo_descricao,cbo_familia) %>% 
    summarise(total_vinculos = sum(total_vinculos)) %>% 
    ungroup()
)
  

arrow::write_parquet(df_saida,'./dados/consulta_segregacao_familia_sexo_05052025.parquet')



query2 <- paste(
  "SELECT ano, sexo, sexo_descricao, cor_raca, cor_raca_descricao, substring(cbo_base,1,4) as cbo_familia,
count(*) as total_vinculos
FROM vinculos_v6_auxiliar.v_publico
where ano in (2004,2015,2021)
group by ano, sexo, sexo_descricao, cor_raca, cor_raca_descricao, substring(cbo_base,1,4)")

df2 <- DBI::dbGetQuery(con, query2)

arrow::write_parquet(df2,'./dados/consulta_segregacao_familia_sexo_cor_05052025.parquet')


# federal
query3 <- paste(
  "SELECT ano, sexo, sexo_descricao, substring(cbo_base,1,4) as cbo_familia,
count(*) as total_vinculos
FROM vinculos_v6_auxiliar.v_publico
where ano in (1985,1995,2004,2010,2015,2021) and nivel_federativo = 'Federal'
group by ano, sexo, sexo_descricao, substring(cbo_base,1,4)")

df3 <- DBI::dbGetQuery(con, query3)

arrow::write_parquet(df3,'./dados/consulta_segregacao_familia_sexo_federal_05052025.parquet')




rm(list=ls())
gc()


