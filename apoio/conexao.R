

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