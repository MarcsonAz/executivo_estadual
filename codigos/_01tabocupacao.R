# tabular ocupacao

pacman::p_load(char = c("dplyr","openxlsx","tidyr","stringr"))


diretorio_trabalho = "//srjn4/atlas/executivo_estadual/"
setwd(diretorio_trabalho)

source('../atlas_estado/ATLAS_2024/conectar.R')

# consultas e carregar em memoria
{
  query1 <- paste(
  "SELECT v.ano,
    v.uf_ipea,
    raca.raca_script_r_resultado,
    v.cbo2002,
    count(1) AS total_vinculos_publicos,
    count(case
          WHEN v.rem_ipea < corte.corte_media_4sd_rem_ipea AND v.rem_ipea > 0::numeric then 1
          ELSE NULL::numeric
          END::double precision) AS total_vinculos_publicos_controlado,
    sum(case
        WHEN v.rem_ipea < corte.corte_media_4sd_rem_ipea AND v.rem_ipea > 0::numeric THEN v.rem_ipea
        ELSE NULL::numeric
        END::double precision) AS rem_soma_vinculos_publicos_controlado
    FROM vinculos_v6.tb_vinculos_2021 v
    JOIN rfb.tb_ipea_rfb_publicos r ON r.cnpj_texto::text = v.id_estab::text
    LEFT JOIN site_adeb_v3.tb_cpf_publico_raca_cor_v2_2004_2021 raca ON raca.cpf = v.cpf::text
    LEFT JOIN mvw_rendimento_publico_corte corte ON corte.ano = v.ano AND corte.poder::text = r.poder::text AND corte.esfera::text = r.esfera::text
    WHERE r.poder::text = 'E'::text AND r.esfera::text = 'E'::text
    GROUP BY v.ano, v.uf_ipea, raca.raca_script_r_resultado,v.cbo2002
    ORDER BY v.ano DESC")
  
  df1 <- DBI::dbGetQuery(con, query1)
  
  query2 <- 
    "select codigo, titulo
  from cbo.\"05_ocupacao\""
  cbo <- DBI::dbGetQuery(con, query2)
}



## totais

sum(df1$total_vinculos_publicos) ## ok 2.990.720

## teste

tab_01 = df1 %>% 
  group_by(cbo2002) %>% 
  summarise(total_vinculos = sum(total_vinculos_publicos),
            rem_media = round(
              sum(rem_soma_vinculos_publicos_controlado,na.rm = TRUE)/sum(total_vinculos_publicos_controlado),
              4)) %>% 
  left_join(cbo,by = join_by(cbo2002 == codigo)) %>% 
  select(cbo2002, titulo,total_vinculos, rem_media) %>% 
  filter(total_vinculos>=100)


openxlsx2::write_xlsx(tab_01, "./dados/planilha_dados_ocupados.xlsx")





# no brasil, dentro do E E

# top ocupacaoes em vinculos

df_top_v <- df1 %>% 
  group_by(cbo2002) %>% 
  summarise(s = sum(total_vinculos_publicos)) %>% 
  slice_max(order_by=s,n=20)
  
 
df_top_v <- left_join(df_top_v,cbo,by = join_by(cbo2002 == codigo)) %>% 
  select(cbo2002, titulo, total_vinculos = s)



# top ocupacaoes em rem media (entre ocupacoes com mais de 10 vínculos)

df_top_rem <- df1 %>% 
  group_by(cbo2002) %>% 
  summarise(s = round(sum(rem_soma_vinculos_publicos_controlado)/sum(total_vinculos_publicos),4)) %>% 
  slice_max(order_by=s,n=20)


df_top_rem <- left_join(df_top_rem,cbo,by = join_by(cbo2002 == codigo)) %>% 
  select(cbo2002, titulo, rem_media = s)

d = df_top_rem %>%  left_join(df1 %>% 
  group_by(cbo2002) %>% 
  summarise(s = sum(total_vinculos_publicos))
)


### proporcao por ocupacao

# cor por ocupacao
tab_02 = df1 %>% 
  mutate(cor = case_when(
    raca_script_r_resultado == 1 ~ "Índigena",
    raca_script_r_resultado == 2 ~ "Branca",
    raca_script_r_resultado == 4 ~ "Preta",
    raca_script_r_resultado == 6 ~ "Amarela",
    raca_script_r_resultado == 8 ~ "Parda",
    .default = NA_character_
  ),
  cor_pre_par = case_when(
    raca_script_r_resultado %in% c(1,2,6) ~ "Não Preta ou Parda",
    raca_script_r_resultado %in% c(4,8) ~ "Preta ou Parda",
    .default = NA_character_
  )) %>%
  filter(!is.na(cor_pre_par)) %>% 
  group_by(cbo2002,cor_pre_par) %>% 
  summarise(total_cbo_cor = sum(total_vinculos_publicos)) %>% 
  mutate(total_cbo = sum(total_cbo_cor)) %>%
  ungroup() %>% 
  filter(cor_pre_par == "Preta ou Parda") %>% 
  mutate(prop_cor_pre_par = round(100*total_cbo_cor/total_cbo,4))



#### colocar a proporcao dentro da tabela de ocupacoes com o filtro de 
# minimo de 100 vinculos

tab_03 = left_join(tab_01, tab_02) %>% 
  select(cbo2002,nome_ocupacao = titulo, total_vinculos,rem_media,prop_cor_pre_par )


openxlsx2::write_xlsx(tab_03, "./dados/planilha_dados_ocupados_2021.xlsx")



