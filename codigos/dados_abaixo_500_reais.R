# ocup - pessoal abaixo de 500 reais em 2021

q = "select v.ano, v.rem_ipea, v.cbo2002, ocup.titulo, e.razao_social, raca.raca_script_r_resultado, v.genero, d.descricao, v.horas_contr
FROM vinculos_v6.tb_vinculos_2021 v
     JOIN rfb_2024.empresas e ON e.cnpj_basico = v.cnpj_raiz::text
     JOIN ibge.dicionario_natureza_juridica d ON d.natureza_juridica::text = e.natureza_juridica
     LEFT JOIN vinculos_v6_auxiliar.mvw_rendimento_publico_corte corte ON corte.ano::text = v.ano::text AND corte.poder::text = d.poder::text AND corte.nivel_federativo::text = d.nivel::text
     LEFT JOIN raca_cor.raca_cor_adm_publica_rfb_parte_1 raca ON raca.cpf::text = v.cpf::text
     LEFT JOIN cbo.\"05_ocupacao\" ocup on ocup.codigo = v.cbo2002 
  WHERE d.setor::text = 'publico'::text AND v.rem_ipea IS NOT NULL AND v.rem_ipea > 0::numeric and raca.raca_script_r_resultado is not null and v.rem_ipea < 500::numeric"

df = DBI::dbGetQuery(con,q)

glimpse(df)

boxplot(df$rem_ipea)

library(ggplot2)

df %>% 
  group_by(cbo2002,titulo) %>% 
  summarise(t = n(),
    m = mean(rem_ipea)) %>%
  ungroup() %>% 
  mutate(prop_t = round(t/sum(t)*100,2)) %>% 
  arrange(desc(t))

# A tibble: 459 × 5
#   cbo2002             titulo                                                        total   rem_media  prop_total
# 
# 1  232115  Professor de disciplinas pedagógicas no ensino médio                        2772  468.       13.0 
# 2  411010  Assistente administrativo                                                   2598  448.       12.1 
# 3  111410  Dirigente do serviço público estadual e distrital                           2379  441.       11.1 
# 4  331205  Professor de nível médio no ensino fundamental                              2227  434.       10.4 
# 5  111415  Dirigente do serviço público municipal                                      1544  425.       7.21
# 6  411005  Auxiliar de escritório                                                       555  429.       2.59
# 7  231210  Professor de nível superior do ensino fundamental (primeira a quarta séri…   393  439.       1.84
# 8  410105  Supervisor administrativo                                                    382  429.       1.78
# 9  515215  Auxiliar de laboratório de análises clínicas                                 379  457.       1.77
# 10 514225  Trabalhador de serviços de limpeza e conservação de áreas públicas           362  434.       1.69

df %>% 
  group_by(genero) %>% 
  summarise(t = n(),
            m = mean(rem_ipea)) %>%
  ungroup() %>% 
  mutate(prop_t = round(t/sum(t)*100,2)) %>% 
  arrange(desc(t))



df %>% 
  group_by(raca_script_r_resultado) %>% 
  summarise(t = n(),
            m = mean(rem_ipea)) %>%
  ungroup() %>% 
  mutate(prop_t = round(t/sum(t)*100,2)) %>% 
  arrange(desc(t))


df %>% 
  group_by(descricao) %>% 
  summarise(t = n(),
            m = mean(rem_ipea)) %>%
  ungroup() %>% 
  mutate(prop_t = round(t/sum(t)*100,2)) %>% 
  arrange(desc(t))


df %>% 
  mutate(horas_contr_cat = case_when(
    horas_contr < 10 ~ 'até 10 horas',
    horas_contr < 20 & horas_contr >= 10   ~ 'de 10 a 20 horas',
    horas_contr < 30 & horas_contr >= 20   ~ 'de 20 a 30 horas',
    horas_contr >= 30  ~ 'de 30 horas ou mais',
    .default = NA_character_
  ) %>% as.factor()) %>% 
  group_by(horas_contr_cat) %>% 
  summarise(t = n(),
            m = mean(rem_ipea)) %>%
  ungroup() %>% 
  mutate(prop_t = round(t/sum(t)*100,2)) %>% 
  arrange(desc(t))

