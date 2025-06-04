#tab_cor_representatividade

pacman::p_load(char = c("dplyr","openxlsx2","tidyr","sidrar","stringr","segregation"))


### dados de populacao e cor ou raca

### Tabela 6402 - Pessoas de 14 anos ou mais de idade, total, na força de trabalho, 
### ocupadas, desocupadas, fora da força de trabalho, em situação de informalidade 
### e respectivas taxas e níveis, por cor ou raça 
### https://sidra.ibge.gov.br/Tabela/6402

# tabalho está em 2021, mas dados do IBGE estão 
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

openxlsx2::write_xlsx(df, "./novo_dados/planilha_dados_ibge_populacao_cor_2022.xlsx")

#### proporcao cor no funcionalismo publico EE

source("../atlas_estado/ATLAS_2024/conectar.R")

query1 <- paste(
  "SELECT v.ano,
    s.edterritorios_codigo as codigo_uf,
    s.edterritorios_sigla as sigla_uf,
    count(v.*) AS total_vinculos_publicos,
    count(case 
    	when raca.raca_script_r_resultado = 2 then 1
    	else null
    end) as total_vinculos_publicos_branca,
    count(case 
    	when raca.raca_script_r_resultado = 4 then 1
    	else null
    end) as total_vinculos_publicos_preta,
    count(case 
    	when raca.raca_script_r_resultado = 8 then 1
    	else null
    end) as total_vinculos_publicos_parda,
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

df_vp_2021_cor <- DBI::dbGetQuery(con, query1)


openxlsx2::write_xlsx(df_vp_2021_cor, "./novo_dados/planilha_dados_novo_uf_cor_2021.xlsx")



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



