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


# publico - total - anos selecionados
query4 <- paste(
  "SELECT ano, sexo, sexo_descricao, cor_raca, cor_raca_descricao, substring(cbo_base,1,4) as cbo_familia,
count(*) as total_vinculos
FROM vinculos_v6_auxiliar.v_publico
where ano in (1985,1995,2005,2010,2015,2021) 
group by ano, sexo, sexo_descricao, substring(cbo_base,1,4)")

df4 <- DBI::dbGetQuery(con, query4)

arrow::write_parquet(df4,'./dados/consulta_segregacao_familia_sexo_cor_06052025.parquet')

## por Executivo Estadual - UF - ocupacao - sexo - cor - 2021
query13 <- paste(
  "SELECT v.ano,
    v.uf_ipea,
    v.cbo2002,
    v.genero,
    raca.raca_script_r_resultado,
    count(1) AS total_vinculos_publicos
    FROM vinculos_v6.tb_vinculos_2021 v
    JOIN rfb.tb_ipea_rfb_publicos r ON r.cnpj_texto::text = v.id_estab::text
    LEFT JOIN site_adeb_v3.tb_cpf_publico_raca_cor_v2_2004_2021 raca ON raca.cpf = v.cpf::text
    WHERE r.poder::text = 'E'::text AND r.esfera::text = 'E'::text
    GROUP BY v.ano,
    v.uf_ipea,
    v.cbo2002,
    v.genero,
    raca.raca_script_r_resultado
    ORDER BY v.ano DESC")

df13 <- DBI::dbGetQuery(con, query13)

arrow::write_parquet(df13,'./dados/consulta_segregacao_2021_uf_ocupacao_sexo_cor_06052025.parquet')

query15 <- 
  "select codigo, titulo
  from cbo.\"05_ocupacao\""
cbo <- DBI::dbGetQuery(con, query15)


rm(list=ls())
gc()


