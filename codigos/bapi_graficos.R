# codigo para o bapi


###############################################################
# OBJETIVO                                                    #
# GRAFICOS E TABELAS PARA O TEXTO                             #
# Trajetórias das desigualdades nos funcionalismos estaduais  #
###############################################################


# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ipeaplot','scales','ggtext','viridis',
    'ggrepel'),
  character.only = TRUE)


## gráficos no BAPI

#- BAPI 1 - Total de vínculos públicos no poder Executivo e nível estadual (1985 - 2021)
#- BAPI 2 - Remuneração média de vínculos públicos nos níveis federal, estadual e municipal (1985 - 2021)
#- BAPI 3 - Remuneração média entre os quintos de remuneração de vínculos públicos no poder Executivo e nível estadual (1985 - 2021)
#- BAPI 4 - Razão entre o mais alto (nono) e o menor (primeiro) decil de remuneração de vínculos públicos no poder Executivo e nível estadual (1985 - 2021)
#- BAPI 5 - Variação relativa de vínculos públicos no poder Executivo e nível estadual entre 2004 e 2021 por cor para cada Unidade da Federação
#- BAPI 6 - Razão de vínculos de brancos para cada vínculo de negro, por UF (2004 - 2021)
#- BAPI 7 - Índice de representatividade burocrática, por UF (2021)
#- BAPI 8 - Razão da remuneração média de vínculos públicos de pessoas brancas, pardas e pretas do poder Executivo estadual, por Unidade da Federação (2004 - 2021)
#- BAPI 9 - Variação relativa de vínculos públicos no poder Executivo e nível estadual entre 1985 e 2021 por sexo para cada Unidade da Federação
#- BAPI 10- Razão da remuneração média de vínculos públicos no poder Executivo e nível estadual entre homens e mulheres, por Unidade da Federação (1985 - 2021)


# dados

# ARQUIVO ZIP - CÓDIGOS E DADOS

# funcao de leitura de multiplas planilhas
multiplesheets <- function(fname) { 
  
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  names(data_frame) <- sheets 
  return(data_frame)
} 

multiplesheets
source('./apoio/dfs_para_relatorio.R')

##################
##################


# REVISAR - caminhos no source e nos ggsaves - tabela 1,2,3 - carregar irb em dados


##################
##################


# Gráfico 1 #########################################################

# G1.1 - tratamento
data = base_completa$total_uf_sexo

siglas = tibble::tibble(
  codigo = data$codigo_uf,
  sigla = data$sigla_uf) %>% 
  distinct() %>% 
  na.omit() %>% 
  arrange(codigo)

data = data %>% 
  mutate(total_mulher = as.numeric(vinculos_executivo_estadual_feminino),
         total_homem = as.numeric(vinculos_executivo_estadual_masculino),
         total = total_mulher + total_homem,
         codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  select(!...1) %>% 
  na.omit()

data$total[data$ano==2021 & data$codigo_uf=="RO"] = NA_integer_


# G1.2 - grafico 
ggplot(data) +
  geom_line(aes(x=ano, y=total/1000)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf, scales = "free_y", ncol=4) +
  
  labs(caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    legend.position = "none",
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.text.x = element_text(size = 10, color="#141414"),
    axis.title.x = element_blank(),
    
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.title.y = element_text(size = 12,vjust = -2),
    
    strip.text = element_text(size = 12, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414"))


ggsave(filename = "./graficos/uf_total.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)





# Gráfico 2 - #####################################################################

### G2.1 - tratamento
data <- base_completa$rem_media_brasil_nf %>% 
  rename(Federal = remuneracao_media_federal,
         Estadual = remuneracao_media_estadual,
         Municipal = remuneracao_media_municipal) %>% 
  select(-`...1`) %>% 
  pivot_longer(cols = Federal:Municipal,
               names_to = 'categoria') %>% 
  mutate(categoria = factor(categoria,
                            levels = c("Federal", "Estadual","Municipal"),
                            labels = c("Federal", "Estadual","Municipal")))

# 
# f - BDB542
# e - 36809A
# m - 9A3636
cores_nivel_federativo = c('#BDB542',"#36809A","#9A3636")

### G2.2 - grafico
ggplot() +
  geom_line(data = data, aes(x=ano, y=value, color=categoria),
            linewidth = 0.9) +
  scale_color_manual(values=c("Federal" = cores_nivel_federativo[1],
                              "Estadual" = cores_nivel_federativo[2] ,
                              "Municipal" = cores_nivel_federativo[3]),
                     labels = c("Federal", "Estadual", "Municipal")) +
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(
    limits = c(0,14000),
    breaks = seq(0,14000,2000)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, margin = margin(t=0, b=0, unit="pt"), color="#141414"),
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color="#141414"),
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.y = element_text(size = 10,vjust = -2),
    axis.text.y = element_text(size = 10, margin = margin(l = 0), color="#141414"),
    axis.ticks.y = element_blank(),
    
    plot.caption = element_text(size = 10,vjust = 0, color="#141414")
  )

ggsave(filename = "./graficos/brasil_remuneracao_media_nivelfederativo.png",
       plot = last_plot(), width = 7.44, height = 4.68, units = "in", dpi = 300)


# Gráfico 3 - #####################################################################

### G3.1 - tratamento
data = base_completa$rem_media_quintos_brasil

# para grafico 14
data = data %>%  
  select(!...1) %>% 
  pivot_longer(!ano,
               names_to = 'categoria',
               values_to = 'remuneracao'
  ) %>% 
  filter(
    categoria != 'remuneracao_razao_media_quintos_5_1_executivo_estadual') %>% 
  mutate(categoria = factor(categoria,
                            levels= c(
                              'remuneracao_media_quintos_1_executivo_estadual',
                              'remuneracao_media_quintos_2_executivo_estadual',
                              'remuneracao_media_quintos_3_executivo_estadual',
                              'remuneracao_media_quintos_4_executivo_estadual',
                              'remuneracao_media_quintos_5_executivo_estadual'
                            ),
                            labels = c(
                              'Média até percentil 20',
                              'Média entre 20 e 40',
                              'Média entre 40 e 60',
                              'Média entre 60 e 80',
                              'Média acima do 80'
                            ))) %>% 
  na.omit()

# G3.2 - grafico
ggplot(data, aes(ano,remuneracao , color= categoria)) + 
  geom_line() +
  scale_color_viridis(discrete=TRUE, option = "D",end = 0.8, direction = -1) +
  
  theme_ipea() +
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023') +
  ylab('Remuneração em reais') +
  scale_y_continuous(limits = c(0,15e3), breaks = seq(0,15e3,2e3)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, margin = margin(t=0, b=0, unit="pt"), color="#141414"),
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color="#141414"),
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.y = element_text(size = 10,vjust = -2),
    axis.text.y = element_text(size = 10, margin = margin(l = 0), color="#141414"),
    axis.ticks.y = element_blank(),
    
    #strip.text = element_text(size = 6, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414")
  )


#ggsave(filename = "./graficos/brasil_remuneracao_media_quintos_linha.png", device = "png",
#      width = 10, height = 6, units = "cm")

ggsave(filename = "./graficos/brasil_remuneracao_media_quintos_linha.png",
       plot = last_plot(), width = 7.44, height = 4.68, units = "in", dpi = 300)


# G3.3-  tabela auxiliar - # variacao nos dados

df_cv_rem <- data %>% 
  group_by(categoria) %>% 
  summarise(desvio_padrao = sd(remuneracao),
            media = mean(remuneracao)) %>% 
  ungroup() %>% 
  mutate(cv = round(desvio_padrao/media,3))


# Tabela 1 - #####################################################################

# dados na planilha excel: tabela_1.xlsx

# Gráfico 4 - #####################################################################

### G4.1 - tratamento
data = base_completa$rem_decil_brasil

data = data %>% 
  select(!...1) %>% 
  pivot_longer(!ano,
               names_to = 'categoria',
               values_to = 'remuneracao'
  ) %>% 
  filter(
    categoria == 'remuneracao_razao_decil_9_1_executivo_estadual')

# G4.2 - grafico
ggplot(data, aes(ano,remuneracao)) + 
  geom_line() +
  #scale_color_viridis(discrete=TRUE, option = "D",end = 0.8, direction = -1) +
  
  theme_ipea() +
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023') +
  ylab('Razão entre decis de remuneração') +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, margin = margin(t=0, b=0, unit="pt"), color="#141414"),
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color="#141414"),
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.text.y = element_text(size = 10, margin = margin(l = 0), color="#141414"),
    axis.ticks.y = element_blank(),
    
    #strip.text = element_text(size = 6, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414")
  )


ggsave(filename = "./graficos/brasil_remuneracao_decis_razao_9_1.png",
       plot = last_plot(), width = 7.44, height = 4.68, units = "in", dpi = 300)


# Gráfico 5 #########################################################


# G5.1 - tratamento
data = base_completa$total_uf_cor

data = data %>% 
  filter(!(codigo_uf == 11 & ano == 2021), ) %>% 
  mutate(ano = ifelse(
    codigo_uf == 11 & ano == 2020, 2021, ano))

data = data %>%  
  mutate(total = as.numeric(vinculos_executivo_estadual),
         cor_descricao = as.factor(cor_descricao),
         codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  select(ano, codigo_uf, cor, cor_descricao, total) %>% 
  filter(ano %in% c(2004,2021)) %>% 
  arrange(codigo_uf,cor,ano) %>% 
  mutate(
    lag = lag(total)) %>% 
  filter(ano == 2021) %>% 
  mutate(
    variacao_absoluta = total - lag,
    descricao_periodo = rep("2004-2021",5*27),
    variacao_relativa = round(((total/lag)-1)*100,1)
  ) 

# G5.2 grafico
ggplot(data %>% 
         filter(
           cor_descricao %in% c('Branca','Preta','Parda'),
           codigo_uf != 'RR'),
       aes(x=codigo_uf, y=variacao_relativa,fill=cor_descricao)
) + 
  geom_col() +
  facet_wrap(~cor_descricao,ncol=1,scales="free") +
  scale_fill_manual(values = c('#7AD151','#2A788E','#414487')) +
  
  ################# VARIACAO RELATIVA NO NIVEL BRASIL #################
# NOTA DO GRAFICO
labs(caption = 'Fonte: Rais. Notas: Escala livre no eixo vertical. Sem dados de Roraima. Dados de Rondônia são de 2020 em relação a 2004. \nA variação no nível Brasil para cor ou raça Branca é de -5%, para Parda 1% e para Preta 27%.') +
  ################# VARIACAO RELATIVA NO NIVEL BRASIL #################


ylab('Variação de vínculos') +
  theme_ipea() +
  theme(
    legend.position = "none",
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color="#141414"),
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.y = element_text(size = 10, margin = margin(l = 0), color="#141414"),
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.ticks.y = element_blank(),
    
    strip.text = element_text(face= 'bold',size = 10, color="#141414"),
    plot.caption = element_text(size = 7,vjust = 0, color="#141414")
  )

ggsave(filename = "./graficos/uf_total_cor_variacao_relativa.png",
       plot = last_plot(), width = 7.44, height = 7.01, units = "in", dpi = 300)

# exportar data para tabela de ANEXO

write.csv2(data,"./dados/uf_total_cor_variacao_relativa.csv")

# Gráfico 6 #########################################################

# G6.1 - tratamento
data = base_completa$total_uf_cor

data = data %>% 
  mutate(codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla),
         cor_descricao = factor(cor_descricao),
         total= as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano,codigo_uf,cor_descricao,total) %>% 
  na.omit()

data = data %>% 
  pivot_wider(
    names_from = 'cor_descricao',
    values_from = 'total') %>%
  mutate_at(c("Indígena","Branca","Preta","Amarela","Parda") , ~ ifelse(is.na(.),0,.)) %>% 
  mutate(
    razao_b_pp = round(Branca/(Preta+Parda),1)
  ) %>% 
  select(ano,codigo_uf,razao_b_pp)


# G6.2 - grafico
ggplot(data) +
  geom_line(aes(x=ano, y=razao_b_pp)) +
  facet_wrap(~codigo_uf,ncol=4) + #,scales="free_y") +
  
  labs(caption = 'Fonte: Rais') +
  ylab('Razão de vínculos') +
  theme_ipea() +
  scale_y_continuous(limits = c(0,32),
                     breaks = seq(0,31,5)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-5))) +
  theme(
    legend.position = "none",
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.text.x = element_text(size = 10, color="#141414"),
    axis.title.x = element_blank(),
    
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.title.y = element_text(size = 12,vjust = -2),
    
    strip.text = element_text(size = 12, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414"))


ggsave(filename = "./graficos/uf_total_cor_razao.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)



# Gráfico 7 #########################################################

df11 = readxl::read_xlsx("dados/planilha_dados_irb.xlsx")

library(ipeaplot)

ggplot(dados, aes(
  x = sigla_uf_factor,
  y = diff,
  fill = ifelse(diff <= 1, "negativo", "positivo")
)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +
  coord_flip() +
  scale_fill_manual(
    values = c("negativo" = "red", "positivo" = "blue"),
    guide = "none"
  ) +
  labs(
    # title = "Variação por Unidade da Federação",
    x = "Unidades da Federação",
    y = "Variação"
  ) +
  theme_ipea(base_size = 13) +             # <- define o tema principal
  theme_ipea(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # centraliza o título
    axis.text.y = element_text(size = 11, color = "gray20"), # ajusta eixo y
    axis.text.x = element_text(size = 11, color = "gray20"), # ajusta eixo x
    axis.title = element_text(face = "bold"),                # títulos dos eixos
    panel.grid.major.y = element_blank(),                    # remove grades verticais
    panel.grid.minor = element_blank()
  )



# Gráfico 8 #########################################################

# G8.1 - tratamento
apoio =  base_completa$total_uf_sexo %>% 
  select(codigo_uf,sigla_uf) %>%
  distinct() %>% 
  na.omit()

data = base_completa$rem_media_brasil_cor_uf

data = left_join(data,apoio, by = join_by(uf == codigo_uf)) %>% 
  na.omit() %>% 
  mutate(codigo_uf = factor(uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla))

ggplot(data) +
  geom_line(aes(x=ano, y=razao_rem_media)) +
  facet_wrap(~codigo_uf,ncol=4) +
  
  
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre brancos e não brancos.') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(limits = c(0,2),breaks = seq(0,2,0.5)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-5))) +
  theme(
    legend.position = "none",
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.text.x = element_text(size = 10, color="#141414"),
    axis.title.x = element_blank(),
    
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.title.y = element_text(size = 12,vjust = -2),
    
    strip.text = element_text(size = 12, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414"))

ggsave(filename = "./graficos/uf_razao_rem_media_cor.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)


# Gráfico 9 #########################################################


# G9.1 - tratamento
data = base_completa$total_uf_sexo

data = data %>% 
  filter(!(codigo_uf == 11 & ano == 2021),!is.na(codigo_uf)) %>% 
  mutate(ano = ifelse(codigo_uf == 11 & ano == 2020, 2021, ano)) %>% 
  mutate(ano = ifelse(codigo_uf == 17 & ano == 1989, 1985,ano))

data = data %>% 
  filter(!is.na(codigo_uf)) %>% 
  select(ano,codigo_uf,sigla_uf,
         Mulher = vinculos_executivo_estadual_feminino,
         Homem = vinculos_executivo_estadual_masculino) %>% 
  pivot_longer(cols=Mulher:Homem,
               names_to = 'sexo',
               values_to = 'total') %>% 
  
  mutate(total = as.numeric(total),
         sexo = forcats::fct(sexo),
         codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  select(ano, codigo_uf, sexo, total) %>% 
  filter(ano %in% c(1985,2021)) %>% 
  arrange(codigo_uf,sexo,ano) %>% 
  mutate(
    lag = lag(total)) %>% 
  filter(ano == 2021) %>% 
  mutate(
    variacao_absoluta = total - lag,
    descricao_periodo = rep("1985-2021",2*27),
    variacao_relativa = round(((total/lag)-1)*100,1)
  ) %>% 
  select(
    descricao_periodo,codigo_uf,sexo,variacao_absoluta,variacao_relativa)

# G9.2 - grafico
ggplot(data %>% filter(codigo_uf != 'RR'),
       aes(x=codigo_uf, y=variacao_relativa,fill=sexo)) + 
  geom_col() +
  facet_wrap(~sexo,ncol=1,scales="free") +
  scale_fill_manual(values = cores_sexo) +
  labs(caption = 'Fonte: Rais. Notas: Escala livre no eixo vertical. Sem dados de Roraima. \nDados de Rondônia são de 2020 em relação a 1985. \nDados de Tocantins são de 2021 em relação a 1989.') +
  
  ################# VARIACAO RELATIVA NO NIVEL BRASIL #################
# NOTA DO GRAFICO
labs(caption = 'Fonte: Rais. Notas: Escala livre no eixo vertical. Sem dados de Roraima. \nDados de Rondônia são de 2020 em relação a 1985. Dados de Tocantins são de 2021 em relação a 1989.\nA variação relativa para as mulheres é de 28%; para os homens, de 30%.') +
  ################# VARIACAO RELATIVA NO NIVEL BRASIL #################


ylab('Variação de vínculos') +
  theme_ipea() +
  theme(
    legend.position = "none",
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color="#141414"),
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.y = element_text(size = 10, margin = margin(l = 0), color="#141414"),
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.ticks.y = element_blank(),
    
    strip.text = element_text(face= 'bold',size = 10, color="#141414"),
    plot.caption = element_text(size = 7,vjust = 0, color="#141414")
  )

ggsave(filename = "./graficos/uf_total_sexo_variacao_relativa.png",
       plot = last_plot(), width = 7.44, height = 7.01, units = "in", dpi = 300)

# exportar data para tabela de ANEXO

write.csv2(data,"./dados/uf_total_sexo_variacao_relativa.csv")

# Gráfico 10 #########################################################

# G10.1 - tratamento
apoio =  base_completa$total_uf_sexo %>% 
  select(codigo_uf,sigla_uf) %>%
  distinct() %>% 
  na.omit()

data = base_completa$rem_media_brasil_sexo_uf

data = left_join(data,apoio, by = join_by(uf == codigo_uf)) %>% 
  na.omit() %>% 
  mutate(codigo_uf = factor(uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla))

# G10.2 - grafico
ggplot(data) +
  geom_line(aes(x=ano, y=razao_rem_media)) +
  facet_wrap(~codigo_uf,ncol=4) +
  
  
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre homens e mulheres.') +
  ylab('Razão de remuneração') +
  theme_ipea() +
  # scale_y_continuous(limits = c(0,2.3),breaks = seq(0,2,0.5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    legend.position = "none",
    
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.text.x = element_text(size = 10, color="#141414"),
    axis.title.x = element_blank(),
    
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.title.y = element_text(size = 12,vjust = -2),
    
    strip.text = element_text(size = 12, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414"))

ggsave(filename = "./graficos/uf_razao_rem_media_sexo.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)


# tabela 2 

# tabela 3