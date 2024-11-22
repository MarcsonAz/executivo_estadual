#conexao
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "rais_2019",
                      host = "psql10-df",
                      port = 5432,
                      user = keyring::key_get("id_ipea"),
                      password = keyring::key_get("pw_ipea_psql10-df"))

if(DBI::dbGetInfo(con)$host == "psql10-df")cat("Conectado! Acesso ao banco ",DBI::dbGetInfo(con)$dbname)