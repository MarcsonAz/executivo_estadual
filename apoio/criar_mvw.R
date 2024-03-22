"-- vinculos_v6_resumos.brasil_v12_corxpoderxesfera source

CREATE MATERIALIZED VIEW vinculos_v6_resumos.brasil_v12_corxpoderxesfera
TABLESPACE pg_default
AS SELECT v.ano,
    r.poder,
    r.esfera,
    cor.raca_script_r_resultado,
    count(v.*) AS total_vinculos_publicos
   FROM vinculos_v6.tb_vinculos v
     JOIN rfb.tb_ipea_rfb_publicos r ON r.cnpj_texto::text = v.id_estab::text
     LEFT JOIN site_adeb_v3.tb_cpf_publico_raca_cor_v2_2004_2021 cor ON v.cpf::text = cor.cpf
  WHERE v.ano > 2003
  GROUP BY v.ano, r.poder, r.esfera, cor.raca_script_r_resultado
  ORDER BY v.ano DESC, r.poder, r.esfera, cor.raca_script_r_resultado
WITH DATA;

COMMENT ON MATERIALIZED VIEW vinculos_v6_resumos.brasil_v12_corxpoderxesfera IS '22022024 - dados de cor junto com poder e nivel federativo';"

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "rais_2019",
                      host = "psql10-df",
                      port = 5432,
                      user = keyring::key_get("id_ipea"),
                      password = keyring::key_get("pw_ipea_psql10-df"))

DBI::dbGetInfo(con)$host == "psql10-df"

# vinculos_v6_resumos.uf_v12_poderxesfera source
qq <- paste(
  "CREATE MATERIALIZED VIEW vinculos_v6_resumos.uf_v12_poderxesfera 
TABLESPACE pg_default
AS SELECT v.ano,
    e.id AS codigo,
    e.descricao_estado AS uf,
    r.poder,
    r.esfera,
    count(v.*) AS total_vinculos_publicos,
    count(
        CASE
            WHEN r.controle_tabela_ipea::text = 'publicos'::text AND v.rem_ipea < corte.corte_media_4sd_rem_ipea AND v.rem_media_sm > 0::numeric THEN 1
            ELSE NULL::integer
        END) AS total_vinculos_publicos_controlado,
    sum(
        CASE
            WHEN r.controle_tabela_ipea::text = 'publicos'::text AND v.rem_ipea < corte.corte_media_4sd_rem_ipea AND v.rem_media_sm > 0::numeric THEN v.rem_ipea
            ELSE NULL::numeric
        END) AS rem_soma_vinculos_publicos_controlado
        
   FROM vinculos_v6.tb_vinculos v
     JOIN rfb.tb_ipea_rfb_publicos r ON r.cnpj_texto::text = v.id_estab::text
     LEFT JOIN mvw_rendimento_publico_corte corte ON corte.ano = v.ano AND corte.poder::text = r.poder::text AND corte.esfera::text = r.esfera::text
     LEFT JOIN ( VALUES (11,'RO'::text), (12,'AC'::text), (13,'AM'::text), (14,'RR'::text), (15,'PA'::text), (16,'AP'::text), (17,'TO'::text), (21,'MA'::text), (22,'PI'::text), (23,'CE'::text), (24,'RN'::text), (25,'PB'::text), (26,'PE'::text), (27,'AL'::text), (28,'SE'::text), (29,'BA'::text), (31,'MG'::text), (32,'ES'::text), (33,'RJ'::text), (35,'SP'::text), (41,'PR'::text), (42,'SC'::text), (43,'RS'::text), (50,'MS'::text), (51,'MT'::text), (52,'GO'::text), (53,'DF'::text)) e(id, descricao_estado) ON e.id = (v.codemun / 10000)
  WHERE r.poder = 'E' and r.esfera = 'E'
  GROUP BY v.ano, e.id, e.descricao_estado, r.poder, r.esfera
  ORDER BY v.ano DESC, e.id, e.descricao_estado, r.poder, r.esfera
WITH DATA;")
  

DBI::dbSendQuery(con, qq)



# vinculos_v6_resumos.uf_v12_corxpoderxesfera source
qq2 <- paste(
  "
CREATE MATERIALIZED VIEW vinculos_v6_resumos.uf_v12_corxpoderxesfera
TABLESPACE pg_default
AS SELECT v.ano,
    e.id AS codigo,
    e.descricao_estado AS uf,
    r.poder,
    r.esfera,
    cor.raca_script_r_resultado,
    count(v.*) AS total_vinculos_publicos
   FROM vinculos_v6.tb_vinculos v
     JOIN rfb.tb_ipea_rfb_publicos r ON r.cnpj_texto::text = v.id_estab::text
     LEFT JOIN site_adeb_v3.tb_cpf_publico_raca_cor_v2_2004_2021 cor ON v.cpf::text = cor.cpf
     LEFT JOIN ( VALUES (11,'RO'::text), (12,'AC'::text), (13,'AM'::text), (14,'RR'::text), (15,'PA'::text), (16,'AP'::text), (17,'TO'::text), (21,'MA'::text), (22,'PI'::text), (23,'CE'::text), (24,'RN'::text), (25,'PB'::text), (26,'PE'::text), (27,'AL'::text), (28,'SE'::text), (29,'BA'::text), (31,'MG'::text), (32,'ES'::text), (33,'RJ'::text), (35,'SP'::text), (41,'PR'::text), (42,'SC'::text), (43,'RS'::text), (50,'MS'::text), (51,'MT'::text), (52,'GO'::text), (53,'DF'::text)) e(id, descricao_estado) ON e.id = (v.codemun / 10000)
  WHERE v.ano > 2003
  GROUP BY v.ano, e.id, e.descricao_estado, r.poder, r.esfera, cor.raca_script_r_resultado
  ORDER BY v.ano DESC, e.id, e.descricao_estado, r.poder, r.esfera, cor.raca_script_r_resultado
WITH DATA;")


DBI::dbSendQuery(con, qq2)

