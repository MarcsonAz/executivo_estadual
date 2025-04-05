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
  
  query2 <- paste(
    "SELECT v.ano,
    v.genero as sexo,
    case
    	when v.genero = 1 then 'Homem'
    	when v.genero = 2 then 'Mulher'
    end as sexo_descricao,    
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
    LEFT JOIN mvw_rendimento_publico_corte corte ON corte.ano = v.ano AND corte.poder::text = r.poder::text AND corte.esfera::text = r.esfera::text
    WHERE r.poder::text = 'E'::text AND r.esfera::text = 'E'::text
    GROUP BY v.ano, v.genero ,v.cbo2002
    ORDER BY v.ano DESC")
  
  df2 <- DBI::dbGetQuery(con, query2)
  
  query3 <- 
    "select codigo, titulo
  from cbo.\"05_ocupacao\""
  cbo <- DBI::dbGetQuery(con, query3)
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
  filter(total_vinculos>=100) %>% 
  na.omit()


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

# cor e sexo por ocupacao
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

tab_03 = df2 %>%  group_by(cbo2002, sexo_descricao) %>% 
  summarise(total_cbo_sexo = sum(total_vinculos_publicos)) %>% 
  mutate(total_cbo = sum(total_cbo_sexo)) %>%
  ungroup() %>% 
  filter(sexo_descricao == "Mulher") %>% 
  mutate(prop_sexo_mulher = round(100*total_cbo_sexo/total_cbo,4))

tab_03 = tab_03 %>% filter(total_cbo>=100)
  
  
  
#### colocar a proporcao dentro da tabela de ocupacoes com o filtro de 
# minimo de 100 vinculos

tab_saida = left_join(tab_01, tab_02) %>% 
  left_join(tab_03 %>% select(cbo2002, prop_sexo_mulher)) %>% 
  select(cbo2002,nome_ocupacao = titulo, total_vinculos,rem_media,
         prop_cor_pre_par,prop_sexo_mulher)


openxlsx2::write_xlsx(tab_saida, "./dados/planilha_dados_ocupados_2021.xlsx")


### dados de populacao e cor ou raca

library(sidrar)
library(tidyr)



dados_origem = sidrar::get_sidra(6402,
                       variable = 4090,
                       geo = c("State"),period = c("202204"))
df = dados_origem
# tratamento

df = df %>% 
  janitor::clean_names() %>% 
  select(
    unidade_da_federacao_codigo,unidade_da_federacao,
    cor_ou_raca,
    total_ocupados = valor
  )

df = df %>% 
  pivot_wider(names_from = cor_ou_raca, values_from = total_ocupados) %>% 
  mutate(cor_prop_prepar = round((Preta+Parda)/Total,4))



openxlsx2::write_xlsx(df, "./dados/planilha_dados_populacao_cor_2022.xlsx")





#### proporcao cor no funcionalismo publico EE


query4 <- paste(
  "SELECT v.ano,
    s.edterritorios_codigo as codigo_uf,
    s.edterritorios_sigla as sigla_uf,
    count(v.*) AS total_vinculos_publicos,
    count(case 
    	when raca.raca_script_r_resultado in (4,8) then 1
    	else null
    end) as total_vinculos_publicos_prepar
    FROM vinculos_v6.tb_vinculos_2021 v
    JOIN rfb.tb_ipea_rfb_publicos r ON r.cnpj_texto::text = v.id_estab::text
    LEFT JOIN site_adeb_v3.tb_cpf_publico_raca_cor_v2_2004_2021 raca ON raca.cpf = v.cpf::text
    left join spat.ed_territorios_uf_geral s on s.edterritorios_codigo = v.uf_ipea
    WHERE r.poder::text = 'E'::text AND r.esfera::text = 'E'::text
    GROUP BY v.ano,
    s.edterritorios_codigo,
    s.edterritorios_sigla
    ORDER BY v.ano DESC")

df_vp_2021_cor <- DBI::dbGetQuery(con, query4)


openxlsx2::write_xlsx(df_vp_2021_cor, "./dados/planilha_dados_uf_cor_2021.xlsx")



#### grafico barra - representação de cor ou raça por UF 2021

## tratamento

df_vp_2021_cor2 = df_vp_2021_cor %>% 
  mutate(uf = as.character(codigo_uf),
    vp_prop_prepar = round(
    total_vinculos_publicos_prepar/total_vinculos_publicos,4)) %>% 
  select(uf, sigla_uf, vp_prop_prepar)

# populacao
df2 = df %>% 
  select(uf=unidade_da_federacao_codigo, nome_uf = unidade_da_federacao,
         pop_prop_prepar = cor_prop_prepar)

dados <- left_join(df2,df_vp_2021_cor2) %>% 
  mutate(diff = vp_prop_prepar - pop_prop_prepar,
         sigla_uf = forcats::as_factor(sigla_uf)
  ) %>% 
  mutate(sigla_uf_factor = forcats::fct_reorder(nome_uf,diff))

ggplot(dados, aes(x = sigla_uf_factor, y = diff, fill = ifelse(diff < 0, "negativo", "positivo"))) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("negativo" = "red", "positivo" = "blue"),
                    guide = "none") +
  scale_y_continuous(limits = c(-0.55,0.1), breaks = seq(-0.5,0.5,0.1)) +
  labs(title = element_blank(),
       x = "Unidades da Federação",
       y = "Variação") #+
  theme(
    
  )



ggsave(filename = "./graficos/uf_proporcao_prepar_representacao.png", device = "png",
       width = 12, height = 6, units = "cm")






queryx <- paste(
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

dfx <- DBI::dbGetQuery(con, queryx)









