# graficos uf

###############################################
# OBJETIVO                                    #
# GRAFICOS UF PARA O TEXTO                #
#                                             #
###############################################

 
# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ipeaplot','scales','ggtext','viridis',
    'ggrepel'),
  character.only = TRUE)

## SERIES UF ###############################################################
## 

# dados
source('./apoio/dfs_para_relatorio.R')

{
 

data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano, total)


variacao = data %>% filter(
  ano %in% c(1985,1995,2005,2015,2021)
) %>% mutate(
  lag = lag(total),
  variacao_absoluta = total - lag,
  descricao_periodo = c("","1985-1995","1995-2005","2005-2015","2015-2021"),
  variacao_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,variacao_absoluta,variacao_relativa)

variacao2 = data %>% filter(
  ano %in% c(1985,2021)
) %>% mutate(
  lag = lag(total),
  variacao_absoluta = total - lag,
  descricao_periodo = c("","1985-2021"),
  variacao_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,variacao_absoluta,variacao_relativa)

variacao = rbind(
  variacao, variacao2
) %>% mutate(descricao_periodo = factor(
  descricao_periodo, levels = descricao_periodo, labels=descricao_periodo
))

texto_caption = paste0(
    'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
    round(variacao$variacao_relativa[1]),"%, ",
    round(variacao$variacao_relativa[2]),"%, ",
    round(variacao$variacao_relativa[3]),"%, ",
    round(variacao$variacao_relativa[4]),"% e ",
    round(variacao$variacao_relativa[5]),"%.")

}

data = base_completa$total_uf_sexo

siglas = tibble::tibble(
  codigo = data$codigo_uf,
  sigla = data$sigla_uf
) %>% 
  distinct() %>% 
  na.omit() %>% 
  arrange(codigo)

####
#### 

# Gráfico 1 - Total de vínculos públicos no poder Executivo e nível estadual (1985 - 2021)

# Gráfico 3 - Remuneração média de vínculos públicos nos níveis municipal, estadual e federal por Unidade da Federação (1985 - 2021)

# Gráfico 4 - Remuneração média de vínculos públicos no poder Executivo e nível estadual por Unidade da Federação (2021)

# Gráfico 7 - Variação de vínculos públicos no poder Executivo e nível estadual entre 2004 e 2021 por cor para cada Unidade da Federação

# Gráfico 8 - Razão de vínculos de Brancos para cada vínculo de Preto ou Pardo, por Unidade da Federação (2004 - 2021)

# Gráfico 10 - Razão da remuneração média de vínculos públicos no poder Executivo e nível estadual entre vínculos de Brancos e de Pretos ou Pardos, por Unidade da Federação (2004 - 2021)

# Gráfico 11 - Total de vínculos públicos no poder Executivo e nível estadual por sexo (1985 - 2021)

# Gráfico 12 - Variação de vínculos públicos no poder Executivo e nível estadual entre 1985 e 2021 por sexo para cada Unidade da Federação

# Gráfico 13 - Razão da remuneração média de vínculos públicos no poder Executivo e nível estadual entre homens para cada Mulher, por Unidade da Federação (1985 - 2021)

# Gráfico A2 - Painel do total anual de vínculos no Executivo estadual por cor e UF (2004-2021)


####
#### 


# Gráfico 1 #########################################################

# 1 - tratamento
data = base_completa$total_uf_sexo

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


# 2 - grafico 
ggplot(data) +
  geom_line(aes(x=ano, y=total/1000)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf, scales = "free_y", ncol=4) +
  
  labs(caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    # legend.title = element_blank(),
    legend.position = "none",
    #legend.text = element_text(size = 14, margin = margin(t=0, b=0, unit="pt"), color="#141414"),
    
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





# Gráfico 3 #########################################################


# 1 - tratamento
data = df11_2 ## codigo: 01_tabulacao_dados.R - linha 129

data = data %>%
  rename(Federal = rem_media_federal_total,
         Estadual = rem_media_estadual_total,
         Municipal = rem_media_municipal_total) %>% 
  pivot_longer(!c(ano,codigo,uf), names_to = "categoria", values_to= "valor") %>% 
  mutate(categoria = factor(categoria,
                            levels = c('Federal','Estadual','Municipal'),
                            labels = c('Federal','Estadual','Municipal')),
         uf2 = factor(codigo,
                      levels = data$codigo,
                      labels = data$uf)) %>% 
  filter(!(categoria=='Municipal' & uf == 'DF'))

# f - BDB542
# e - 36809A
# m - 9A3636
cores_nf = c('#BDB542',"#36809A","#9A3636")


# 2 - grafico 
ggplot(data) +
  geom_line(
            aes(x=ano, y=valor, color=categoria)) +
  facet_wrap(~uf2,ncol=4) +
  
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023.') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_color_manual(values=c("Federal" = cores_nf[1],
                              "Estadual" = cores_nf[2] ,
                              "Municipal" = cores_nf[3]),
                     labels = c("Federal", "Estadual", "Municipal")) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 14, margin = margin(t=0, b=0, unit="pt"), color="#141414"),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.text.x = element_text(size = 10, color="#141414"),
    axis.title.x = element_blank(),
    
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.title.y = element_text(size = 12,vjust = -2),
    
    strip.text = element_text(size = 12, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414"))

ggsave(filename = "./graficos/uf_rem_media_nivel_federativo.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)


# Gráfico 4 #########################################################


# 1 - tratamento
data = base_completa$rem_media_uf

data = data %>% 
  dplyr::filter(ano == 2021) %>% 
  mutate(codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla),
         rem_media = round(remuneracao_media_executivo_estadual),
         
         ### destaque para DF
         destaque = ifelse(codigo_uf=="DF","1","0")) %>% 
  select(ano,codigo_uf,destaque,rem_media)


data %>% 
  ggplot(aes(x=codigo_uf,y=rem_media,fill=destaque)) +
  geom_col() +
  geom_text(aes(label = rem_media), nudge_y = 300,
            size = 3) +
  
  scale_fill_manual(values = c("#1E69AB","#1EAB2C")) +
  
  labs(title = 'Unidades da Federação - Remuneração média',
       subtitle = 'Vínculos públicos no poder Executivo e nível estadual em 2021',
       caption = 'Fonte: Rais.') +
  ylab('Remuneração em reais') +
  theme_ipea() + 
  theme(
    legend.position = "none",
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color="#141414"),
    
    axis.title.y = element_text(size = 10,vjust = -2),
    axis.text.y = element_text(size = 10, margin = margin(l = 0), color="#141414"),
    axis.ticks.y = element_blank(),
    
    strip.text = element_text(size = 6, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414")
  ) 

#ggsave(filename = "./graficos/uf_rem_media_2021_barras.png", device = "png",
 #      width = 16, height = 10, units = "in")

ggsave(filename = "./graficos/uf_rem_media_2021_barras2.png",
       plot = last_plot(), width = 8.27, height = 4.68, units = "in", dpi = 300)




# Gráfico 7 #########################################################


# 1 - tratamento
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
  ) %>% 
  select(
    descricao_periodo,codigo_uf,cor,cor_descricao,variacao_absoluta,variacao_relativa)

ggplot(data %>% 
         filter(
            cor_descricao %in% c('Branca','Preta','Parda'),
            codigo_uf != 'RR'),
      aes(x=codigo_uf, y=variacao_relativa,fill=cor_descricao)
  ) + 
  geom_col() +
  facet_wrap(~cor_descricao,ncol=1,scales="free_y") +
  scale_fill_manual(values = c('#7AD151','#2A788E','#414487')) +
  labs(caption = 'Fonte: Rais. Notas: Escala livre no eixo vertical. Sem dados de Roraima. Dados de Rondônia são de 2020 em relação a 2004.') +
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



# Gráfico 8 #########################################################

# 1 - tratamento
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


# 2 - grafico
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

# eixo Y livre
#ggsave(filename = "./graficos/uf_total_cor_razao_apendice_eixo_y_livre.png",
 #      plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)



# Gráfico 10 #########################################################

# 1 - tratamento
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


# Gráfico 11 #########################################################

# 1 - tratamento
data = base_completa$total_uf_sexo

data = data %>% 
  filter(!(codigo_uf == 11 & ano == 2021))

data = data %>% 
  mutate(total_mulher = as.numeric(vinculos_executivo_estadual_feminino),
         total_homem = as.numeric(vinculos_executivo_estadual_masculino),
         total = total_mulher + total_homem,
         codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  select(!...1) %>% 
  na.omit()

data = data %>% select(ano,codigo_uf,total_mulher,total_homem) %>% 
  pivot_longer(-c('ano','codigo_uf'),
               names_to = 'sexo',
               values_to = 'total') %>% 
  mutate(sexo = factor(sexo,
                       levels = c('total_mulher','total_homem'),
                       labels = c('Mulher','Homem')
  ))

cores_sexo = c("#326297","#BDDFFF")


# 2- grafico
ggplot(data, aes(x=ano, y=total/1000, color=sexo)) +
  geom_line() +
  facet_wrap(~codigo_uf, ncol=4, scales="free_y") +
  
  scale_color_ipea() +
  labs(caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  # scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                    limits = c(0,5e5),
  #                    breaks = seq(0,5e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  scale_color_manual(values=cores_sexo) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 14, margin = margin(t=0, b=0, unit="pt"), color="#141414"),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.text.x = element_text(size = 10, color="#141414"),
    axis.title.x = element_blank(),
    
    axis.text.y = element_text(size = 8, margin = margin(l = 0), color="#141414"),
    axis.title.y = element_text(size = 12,vjust = -2),
    
    strip.text = element_text(size = 12, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414")) 

ggsave(filename = "./graficos/uf_total_sexo.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)


# Gráfico 12 #########################################################


# 1 - tratamento
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

ggplot(data %>% filter(codigo_uf != 'RR'),
       aes(x=codigo_uf, y=variacao_relativa,fill=sexo)) + 
  geom_col() +
  facet_wrap(~sexo,ncol=1,scales="free_y") +
  scale_fill_manual(values = cores_sexo) +
  labs(caption = 'Fonte: Rais. Notas: Escala livre no eixo vertical. Sem dados de Roraima. \nDados de Rondônia são de 2020 em relação a 1985. \nDados de Tocantins são de 2021 em relação a 1989.') +
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




# Gráfico 13 #########################################################

# 1 - tratamento
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



####
####



####
####

####
####


####
####


####
####






####
####




####
####


















# Gráfico 2 - #####################################################################

### 1 - tratamento
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

# f - BDB542
# e - 36809A
# m - 9A3636
cores_nf = c('#BDB542',"#36809A","#9A3636")

### 2 - grafico
ggplot() +
  geom_line(data = data, aes(x=ano, y=value, color=categoria),
            linewidth = 0.9) +
  scale_color_manual(values=c("Federal" = cores_nf[1],
                              "Estadual" = cores_nf[2] ,
                              "Municipal" = cores_nf[3]),
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
    
    #strip.text = element_text(size = 6, color="#141414"),
    plot.caption = element_text(size = 10,vjust = 0, color="#141414")
  )

#ggsave(filename = "./graficos/brasil_remuneracao_media_nivelfederativo.png", device = "png",
 #      width = 10, height = 6, units = "cm")

ggsave(filename = "./graficos/brasil_remuneracao_media_nivelfederativo.png",
       plot = last_plot(), width = 7.44, height = 4.68, units = "in", dpi = 300)



# Gráfico 5 - #####################################################################

### 1 - tratamento
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

# 2 - grafico
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


# 3-  tabela auxiliar - # variacao nos dados

df_cv_rem <- data %>% 
  group_by(categoria) %>% 
  summarise(desvio_padrao = sd(remuneracao),
            media = mean(remuneracao)) %>% 
  ungroup() %>% 
  mutate(cv = round(desvio_padrao/media,3))




# Gráfico 6 - #####################################################################

### 1 - tratamento
data = base_completa$rem_decil_brasil

data = data %>% 
  select(!...1) %>% 
  pivot_longer(!ano,
               names_to = 'categoria',
               values_to = 'remuneracao'
  ) %>% 
  filter(
    categoria == 'remuneracao_razao_decil_9_1_executivo_estadual')

# 2 - grafico
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



## A1
data = base_completa$total_brasil_cor

data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual),
         cor_descricao = as.factor(cor_descricao)) %>% 
  select(ano, cor, cor_descricao, total) %>% 
  filter(ano %in% c(2004,2021)) %>% 
  arrange(cor,ano) %>% 
  mutate(
    lag = lag(total)) %>% 
  filter(ano == 2021) %>% 
  mutate(
    variacao_absoluta = total - lag,
    descricao_periodo = rep("2004-2021",5),
    variacao_relativa = round(((total/lag)-1)*100,1)
  ) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,cor,cor_descricao,variacao_absoluta,variacao_relativa)

texto_caption = paste0(
  'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
  round(data$variacao_relativa[data$cor_descricao=='Amarela']),"%, ",
  round(data$variacao_relativa[data$cor_descricao=='Branca']),"%, ",
  round(data$variacao_relativa[data$cor_descricao=='Indígena']),"%, ",
  round(data$variacao_relativa[data$cor_descricao=='Parda']),"% e ",
  round(data$variacao_relativa[data$cor_descricao=='Preta']),"%.")

ggplot(data,
       aes(x=cor_descricao, y=variacao_absoluta,fill=cor_descricao)) +
  geom_col() +
  scale_color_ipea() + 
  scale_fill_manual(values = c('#FDE725','#7AD151','#22A884','#2A788E','#414487')) +
  labs(caption = texto_caption) +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(-100e3,30e3),
                     breaks = seq(-100e3,30e3,20e3)) +
  theme(
    legend.position = "none",
   
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

ggsave(filename = "./graficos/brasil_total_cor_variacao.png",
       plot = last_plot(), width = 7.44, height = 4.68, units = "in", dpi = 300)



























#######################################
#######################################
#######################################

#######################################
#######################################
#######################################

#######################################
#######################################
#######################################









# GRAFICO 1 - TOTAL #######################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total)) +
  scale_color_ipea() +
  labs(title = 'Brasil - Vínculos',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (milhões de unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0,4e6),
                     breaks = seq(0,4e6,0.5e6)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank()
  ) 

ggsave(filename = "./graficos/brasil_total.png", device = "png",
       width = 10, height = 6, units = "cm")



# GRAFICO 2 - variacao ####################################

variacao %>% 
  ggplot(aes(x=descricao_periodo, y=variacao_absoluta,fill=descricao_periodo)) +
  geom_col() +
  scale_color_ipea() + 
  scale_fill_manual(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Vínculos',
       subtitle = 'Variação em períodos de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = texto_caption) +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(-.75e6,.75e6),
                     breaks = seq(-.75e6,.75e6,0.25e6)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none")

ggsave(filename = "./graficos/brasil_total_variacao.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 21 - variacao cor ####################################
data = base_completa$total_brasil_cor

data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual),
         cor_descricao = as.factor(cor_descricao)) %>% 
  select(ano, cor, cor_descricao, total)

variacao3 = data %>% 
  filter(ano %in% c(2004,2021)) %>% 
  arrange(cor,ano) %>% 
  mutate(
  lag = lag(total)) %>% 
  filter(ano == 2021) %>% 
  mutate(
  variacao_absoluta = total - lag,
  descricao_periodo = rep("2004-2021",5),
  variacao_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,cor,cor_descricao,variacao_absoluta,variacao_relativa)

texto_caption = paste0(
  'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
  round(variacao3$variacao_relativa[variacao3$cor_descricao=='Amarela']),"%, ",
  round(variacao3$variacao_relativa[variacao3$cor_descricao=='Branca']),"%, ",
  round(variacao3$variacao_relativa[variacao3$cor_descricao=='Indígena']),"%, ",
  round(variacao3$variacao_relativa[variacao3$cor_descricao=='Parda']),"% e ",
  round(variacao3$variacao_relativa[variacao3$cor_descricao=='Preta']),"%.")

# GRAFICO 5 - TOTAL COR ###################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total, color=cor_descricao)) +
  
  scale_color_ipea() + 
  scale_color_manual(values = c('#FDE725','#7AD151','#22A884','#2A788E','#414487')) +
  labs(title = 'Brasil - Vínculos - Cor',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e cor (2004-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (milhões de unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0,2.5e6),
                     breaks = seq(0,2.5e6,0.5e6)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-3))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/brasil_total_cor.png", device = "png",
       width = 10, height = 6, units = "cm")



variacao3 %>% 
  ggplot(aes(x=cor_descricao, y=variacao_absoluta,fill=cor_descricao)) +
  geom_col() +
  scale_color_ipea() + 
  scale_fill_manual(values = c('#FDE725','#7AD151','#22A884','#2A788E','#414487')) +
  labs(title = 'Brasil - Variação de vínculos por cor',
       subtitle = 'Variação de vínculos públicos no poder Executivo e nível estadual (2004 e 2021)',
       caption = texto_caption) +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(-100e3,30e3),
                     breaks = seq(-100e3,30e3,20e3)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none")

ggsave(filename = "./graficos/brasil_total_cor_variacao.png", device = "png",
       width = 10, height = 6, units = "cm")

## SERIE BRASIL SEXO ##########################################################
## 

data = base_completa$total_brasil_sexo

data = data %>% 
  mutate(total_mulher = as.numeric(vinculos_executivo_estadual_feminino),
         total_homem = as.numeric(vinculos_executivo_estadual_masculino)) %>% 
  select(ano, total_mulher,total_homem) %>% 
  pivot_longer(!ano,
    names_to = 'sexo',
    values_to = 'total'
  ) %>% 
  mutate(sexo = factor(sexo,
                       levels = c('total_mulher','total_homem'),
                       labels = c('Mulher','Homem')))


variacao4 = data %>% 
  filter(ano %in% c(1985,2021)) %>% 
  arrange(sexo) %>% 
  mutate(lag = lag(total)) %>% 
  filter(ano == 2021) %>% 
  mutate(
  variacao_absoluta = total - lag,
  descricao_periodo = c("1985-2021","1985-2021"),
  variacao_relativa = round(((total/lag)-1)*100,1)) %>% 
  #na.omit() %>% 
  select(
    descricao_periodo,sexo,variacao_absoluta,variacao_relativa)

texto = list(mulher = round(variacao4$variacao_relativa[variacao4$sexo=="Mulher"]),
             homem = round(variacao4$variacao_relativa[variacao4$sexo=="Homem"]))


# variacao total brasil sexo
# 

variacao4 %>% 
  ggplot(aes(x=sexo, y=variacao_absoluta,fill=sexo)) +
  geom_col() +
  scale_color_ipea() + 
  #scale_fill_manual(values = c('#FDE725','#7AD151','#22A884','#2A788E','#414487')) +
  labs(title = 'Brasil - Variação de vínculos por cor',
       subtitle = 'Variação de vínculos públicos no poder Executivo e nível estadual (2004 e 2021)',
       caption = texto_caption) +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(-100e3,30e3),
                     breaks = seq(-100e3,30e3,20e3)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none")

ggsave(filename = "./graficos/brasil_total_cor_variacao.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 3 - TOTAL #######################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total, color=sexo)) +
  
  scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Vínculos - Sexo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e sexo (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (milhões de unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0,2.2e6),
                     breaks = seq(0,2e6,0.5e6)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())
  
ggsave(filename = "./graficos/brasil_total_sexo.png", device = "png",
       width = 10, height = 6, units = "cm")

# GRAFICO 4 - variacao ####################################

variacao4 %>% 
  ggplot(aes(x=sexo, y=variacao_absoluta,fill=sexo)) +
  geom_col() +
  scale_color_ipea() + 
  #scale_fill_manual(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Vínculos - Sexo',
       subtitle = 'Variação de vínculos públicos de homens e mulheres no poder Executivo e nível estadual entre 1985 e 2021',
       caption = paste0(
'Fonte: Rais. Nota: a variação relativa para Mulher é de ',texto$mulher,
'% e de Homem é ',texto$homem,'%')) +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(0,.5e6),
                     breaks = seq(0,5e6,0.1e6)) +
  theme(
      axis.title.x = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      
      
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 10),
      axis.title.y = element_text(size = 15),
      axis.text.x  = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      plot.caption = element_text(size = 10)
    )

ggsave(filename = "./graficos/brasil_variacao_sexo.png", device = "png",
       width = 10, height = 6, units = "cm")







#################################
#################################
# remuneracao brasil

# dados
data = base_completa$rem_media_brasil

data = data %>% 
  select(ano, 
         media = remuneracao_media_executivo_estadual,
         mediana = remuneracao_mediana_executivo_estadual) %>% 
  pivot_longer(!ano,
               names_to = 'categoria',
               values_to = 'remuneracao'
  ) %>% 
  mutate(categoria = factor(categoria,
                       levels = c("media", "mediana"),
                       labels = c('Média','Mediana')))

data2 <- base_completa$rem_media_brasil_nf %>% 
  rename(Federal = remuneracao_media_federal,
         Estadual = remuneracao_media_estadual,
         Municipal = remuneracao_media_municipal) %>% 
  select(-`...1`) %>% 
  pivot_longer(cols = Federal:Municipal,
               names_to = 'categoria') %>% 
  mutate(categoria = factor(categoria,
                            levels = c("Federal", "Estadual","Municipal"),
                            labels = c("Federal", "Estadual","Municipal")))


data2 %>% filter(ano %in% c(1985,2021)) %>% 
  View()


# GRAFICO 6 - remuneracao media brasil ####################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=remuneracao, color=categoria)) +
  
  scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Remuneração',
       subtitle = 'Remuneração média e mediana de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0,6200),
                     breaks = seq(0,6000,1000)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/brasil_remuneracao_media_mediana.png", device = "png",
       width = 10, height = 6, units = "cm")



# GRAFICO 6.1 - remuneracao media brasil por nivel federativo ###############


ggplot() +
  geom_line(data = data2, aes(x=ano, y=value, color=categoria),
            linewidth = 1.1) +
  
  #scale_color_discrete() + 
  scale_color_discrete(label=c("Federal","Estadual","Municipal")) +
  labs(title = 'Brasil - Remuneração',
       subtitle = 'Remuneração média de vínculos públicos por nível federativo por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
    limits = c(0,14000),
    breaks = seq(0,14000,2000)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 6),
    
    title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title.y = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.ticks.y = element_blank(),
    
    strip.text = element_text(size = 6),
    plot.caption = element_text(size = 6)
  )

ggsave(filename = "./graficos/brasil_remuneracao_media_nivelfederativo.png", device = "png",
       width = 10, height = 6, units = "cm")




###########################

# dados - decil
data = base_completa$rem_decil_brasil

# para grafico 9
data2 = data %>% 
  select(!...1) %>% 
  pivot_longer(!ano,
               names_to = 'categoria',
               values_to = 'remuneracao'
  ) %>% 
  filter(
    categoria == 'remuneracao_razao_decil_9_1_executivo_estadual')

# para graficos 7,7.1,8  
data = data %>% 
  select(!...1) %>% 
  pivot_longer(!ano,
               names_to = 'categoria',
               values_to = 'remuneracao'
  ) %>% 
  filter(
    categoria != 'remuneracao_razao_decil_9_1_executivo_estadual') %>% 
  mutate(categoria = factor(categoria,
                            levels= c(
                              'remuneracao_decil_1_executivo_estadual',
                              'remuneracao_decil_2_executivo_estadual',
                              'remuneracao_decil_3_executivo_estadual',
                              'remuneracao_decil_4_executivo_estadual',
                              'remuneracao_decil_5_executivo_estadual',
                              'remuneracao_decil_6_executivo_estadual',
                              'remuneracao_decil_7_executivo_estadual',
                              'remuneracao_decil_8_executivo_estadual',
                              'remuneracao_decil_9_executivo_estadual'
                            ),
                            labels = c(
                              'Decil 1','Decil 2','Decil 3',
                              'Decil 4','Decil 5','Decil 6',
                              'Decil 7','Decil 8','Decil 9'
                            ))) 
  



# GRAFICO 7 - remuneracao decil brasil ####################################

ggplot(data, aes(ano, categoria, fill= remuneracao)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, option = "H",begin = 0.15,end = 0.75) +
  theme_classic() +
  labs(title = 'Brasil - Decil de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023',
       fill = "Reais") +
  
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #legend.position = "top",
    legend.title = element_blank()) 


ggsave(filename = "./graficos/brasil_remuneracao_decis.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 7.1 - remuneracao decil brasil ####################################

ggplot(data, aes(ano, categoria, fill= remuneracao)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE, option = "H",begin = 0.15,end = 0.75) +
  theme_classic() +
  labs(title = 'Brasil - Decil de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023',
       fill = "Reais") +
  
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #legend.position = "top",
    legend.title = element_blank()) +
  geom_text(x=1990, y='Decil 1', label="1990 - Decil 1 - R$ 904") +
  geom_text(x=1990, y='Decil 9', label="1990 - Decil 9 - R$ 6719") +
  geom_text(x=2018, y='Decil 1', label="2018 - Decil 1 - R$ 1623") +
  geom_text(x=2018, y='Decil 9', label="2018 - Decil 9 - R$ 11554") 


ggsave(filename = "./graficos/brasil_remuneracao_decis_anotacao.png", device = "png",
       width = 10, height = 6, units = "cm")

# GRAFICO 8 - remuneracao decil brasil linhas #################################

ggplot(data, aes(ano,remuneracao , color= categoria)) + 
  geom_line() +
  scale_color_viridis(discrete=TRUE, option = "D",end = 0.8, direction = -1,
                      guide = guide_legend(reverse = TRUE)) +
  theme_classic() +
  labs(title = 'Brasil - Decil de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023',
       fill = "Reais") +
  scale_y_continuous(limits = c(0,12e3), breaks = seq(0,12e3,2e3)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #legend.position = "top",
    legend.title = element_blank()) 


ggsave(filename = "./graficos/brasil_remuneracao_decis_linha.png", device = "png",
       width = 10, height = 6, units = "cm")



# GRAFICO XXX - novo - remuneracao media decil brasil linhas #################################

#data = base_completa$

ggplot(data, aes(ano,remuneracao , color= categoria)) + 
  geom_line() +
  scale_color_viridis(discrete=TRUE, option = "D",end = 0.8, direction = -1,
                      guide = guide_legend(reverse = TRUE)) +
  theme_classic() +
  labs(title = 'Brasil - Decil de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023',
       fill = "Reais") +
  scale_y_continuous(limits = c(0,12e3), breaks = seq(0,12e3,2e3)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #legend.position = "top",
    legend.title = element_blank()) 


ggsave(filename = "./graficos/brasil_remuneracao_decis_linha.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 9 - remuneracao decil brasil linha de razao #################################

ggplot(data2, aes(ano, remuneracao)) + 
  geom_line() +
  theme_ipea() +
  labs(title = 'Brasil - Razão entre o nono e primeiro decil de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023',
       fill = "Reais") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #legend.position = "top",
    legend.title = element_blank()) 


ggsave(filename = "./graficos/brasil_remuneracao_decis_razao_9_1.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 14 - remuneracao media no decil brasil linhas #################################

# dados - decil
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

ggplot(data, aes(ano,remuneracao , color= categoria)) + 
  geom_line() +
  scale_color_viridis(discrete=TRUE, option = "D",end = 0.8, direction = -1,
                      guide = guide_legend(reverse = TRUE)) +
  theme_classic() +
  labs(title = 'Brasil - Média nos quintos de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023',
       fill = "Reais") +
  guides(color = guide_legend(title = "Quintos")) +
  scale_y_continuous(limits = c(0,15e3), breaks = seq(0,15e3,2e3)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #legend.position = "top",
    legend.title = element_blank()) 






# variacao nos dados

df_cv_rem <- data %>% 
  group_by(categoria) %>% 
  summarise(desvio_padrao = sd(remuneracao),
            media = mean(remuneracao)) %>% 
  ungroup() %>% 
  mutate(cv = round(desvio_padrao/media,3))

ggsave(filename = "./graficos/brasil_remuneracao_media_quintos_linha.png", device = "png",
       width = 10, height = 6, units = "cm")




# GRAFICO 15 - remuneracao media brasil sexo ####################################

# eixo Y
data = base_completa$rem_media_brasil_sexo

ggplot() +
  geom_line(data = data, aes(x=ano, y=razao_rem_media)) +
  
  scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Razão de remuneração média entre homens e mulheres',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre homens e mulheres') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
    limits = c(1,1.4),
    breaks = seq(1,1.4,.1)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/brasil_remuneracao_media_razao_sexo.png", device = "png",
       width = 10, height = 6, units = "cm")



# GRAFICO 16 - remuneracao media brasil cor ####################################

# eixo Y
data = base_completa$rem_media_brasil_cor

ggplot() +
  geom_line(data = data, aes(x=ano, y=razao_rem_media)) +
  
  scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Razão de remuneração média entre brancos e não brancos',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por ano (2004-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre brancos e não brancos') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
    limits = c(1,1.4),
    breaks = seq(1,1.4,.1)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-3))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/brasil_remuneracao_media_razao_cor.png", device = "png",
       width = 10, height = 6, units = "cm")





######################################
######################################
######################################
######################################
### DADOS  UF
### 


data = base_completa$total_uf_sexo

siglas = tibble::tibble(
  codigo = data$codigo_uf,
  sigla = data$sigla_uf
  ) %>% 
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

data2 = data %>% select(ano,codigo_uf,total_mulher,total_homem) %>% 
  pivot_longer(-c('ano','codigo_uf'),
               names_to = 'sexo',
               values_to = 'total') %>% 
  mutate(sexo = factor(sexo,
                       levels = c('total_mulher','total_homem'),
                       labels = c('Mulher','Homem')
                       ))

data3 = data2 %>% 
  filter(ano %in% c(1985,2021)) %>%
  pivot_wider(names_from = ano, values_from = total) %>% 
  mutate(`1985` = ifelse(is.na(`1985`),0,`1985`),
         `2021` = ifelse(is.na(`2021`),0,`2021`)) %>% 
  mutate(diff = `2021` - `1985`)

# GRAFICO 10 - TOTAL #######################################

# liberar o eixo Y, em mil unidades para diminiur label no eixo Y

dd = data
data$total[data$ano==2021 & data$codigo_uf=="RO"] = NA_integer_

ggplot() +
  geom_line(data = data, aes(x=ano, y=total/1000)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf, scales = "free") +
  
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades)',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    
    title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 12),
    axis.ticks.y = element_blank(),
    
    strip.text = element_text(size = 20),
    plot.caption = element_text(size = 30)
  ) 


# scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
#                    limits = c(0,8.3e5),
#                    breaks = seq(0,8e5,2e5))

ggsave(filename = "./graficos/uf_total.png", device = "png",
       width = 15, height = 15, units = "cm")


# GRAFICO 10.1 - TOTAL sem sp ###################################

ggplot() +
  geom_line(data = data %>% filter(sigla_uf!="SP"), 
            aes(x=ano, y=total)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf) +
  
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sem São Paulo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,3.8e5),
                     breaks = seq(0,4e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    title = element_text(size = 20)
  ) 

ggsave(filename = "./graficos/uf_total_sem_sp.png", device = "png",
       width = 15, height = 15, units = "cm")


# GRAFICO 11 - TOTAL SEXO ##################################

ggplot() +
  geom_line(data=data2, aes(x=ano, y=total, color=sexo)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf) +
  
  scale_color_ipea() +
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sexo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e sexo (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,5e5),
                     breaks = seq(0,5e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside",# = c(0.5,0.1),
    legend.justification = c(0.8,0.05),
    #legend.box.just = "bottom",
    #legend.margin = margin(6, 6, 6, 6)
  ) 

ggsave(filename = "./graficos/uf_total_sexo.png", device = "png",
       width = 15, height = 15, units = "cm")


# GRAFICO 11 - TOTAL SEXO LIBERAR EIXO Y #####################

ggplot() +
  geom_line(data=data2, aes(x=ano, y=total/1000, color=sexo)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf, scales = "free") +
  
  scale_color_ipea() +
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sexo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e sexo (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  # scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                    limits = c(0,5e5),
  #                    breaks = seq(0,5e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    
    title = element_text(size = 20),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 20),
    
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_blank(),
    #axis.title.y.left = element_text(vjust = -3),
    
    strip.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = "inside",# = c(0.5,0.1),
    legend.justification = c(0.8,0.05),
    legend.text = element_text(size = 20),
    #legend.justification = c(0.85, 0.05),
    #legend.direction = "horizontal",
    plot.caption = element_text(size = 30)
) 

ggsave(filename = "./graficos/uf_total_sexo_livre_eixo_y.png", device = "png",
       width = 15, height = 15, units = "cm")


# GRAFICO 11.1 - TOTAL SEXO SEM SAO PAULO##########################

ggplot() +
  geom_line(data=data2 %>% filter(codigo_uf!="SP"),
            aes(x=ano, y=total, color=sexo)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf) +
  
  scale_color_ipea() +
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sexo - Sem São Paulo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e sexo (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,2.5e5),
                     breaks = seq(0,2.5e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside",# = c(0.5,0.1),
    legend.justification = c(0.8,0.05),
    #legend.box.just = "bottom",
    #legend.margin = margin(6, 6, 6, 6)
  ) 

ggsave(filename = "./graficos/uf_total_sexo_sem_sp.png", device = "png",
       width = 15, height = 15, units = "cm")


# GRAFICO 11.3 - TOTAL SEXO variacao ##########################

ggplot() +
  geom_col(data=data3,
            aes(x=codigo_uf, y=diff)) +
  scale_color_ipea() +
  facet_wrap(~sexo,ncol=1) +
  
  scale_color_ipea() +
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sexo - Sem São Paulo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e sexo (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,2.5e5),
                     breaks = seq(0,2.5e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside",# = c(0.5,0.1),
    legend.justification = c(0.8,0.05),
    #legend.box.just = "bottom",
    #legend.margin = margin(6, 6, 6, 6)
  ) 

ggsave(filename = "./graficos/uf_total_sexo_sem_sp.png", device = "png",
       width = 15, height = 15, units = "cm")






##################################################
########## # DADOS DE COR OU RAÇA


data = base_completa$total_uf_cor

siglas = tibble::tibble(
  codigo = data$codigo_uf,
  sigla = data$sigla_uf
) %>% 
  distinct() %>% 
  na.omit() %>% 
  arrange(codigo)

data = data %>% 
  mutate(codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla),
         cor_descricao = factor(cor_descricao),
         total= as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano,codigo_uf,cor_descricao,total) %>% 
  na.omit()

data$total[data$ano==2021 & data$codigo_uf=="RO"] = NA_integer_

data2 = data %>% select(ano,codigo_uf,total_mulher,total_homem) %>% 
  pivot_longer(-c('ano','codigo_uf'),
               names_to = 'sexo',
               values_to = 'total') %>% 
  mutate(sexo = factor(sexo,
                       levels = c('total_mulher','total_homem'),
                       labels = c('Mulher','Homem')
  ))


# GRAFICO 12 - TOTAL cor #######################################

## liberar eixo Y

ggplot() +
  geom_line(data = data, aes(x=ano, y=total/1000,group=cor_descricao,color=cor_descricao)) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~codigo_uf, scales = "free") +
  
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Cor',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e cor (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  #scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                   limits = c(0,8.3e5),
   #                  breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-5))) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    
    title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 12),
    axis.ticks.y = element_blank(),
    
    strip.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = c(0.93, 0.05),
    legend.text = element_text(size = 20),
    legend.justification = c(0.93, 0.05),
    legend.direction = "horizontal",
    plot.caption = element_text(size = 30)
  ) 

ggsave(filename = "./graficos/uf_total_cor.png", device = "png",
       width = 18, height = 15, units = "cm")



# GRAFICO 12.1 - TOTAL cor sem SP #######################################

ggplot() +
  geom_line(data = data %>% filter(codigo_uf!='SP'), 
            aes(x=ano, y=total,group=cor_descricao,color=cor_descricao)) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~codigo_uf) +
  
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Cor - Sem São Paulo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano e cor (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  #scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                   limits = c(0,8.3e5),
  #                  breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-5))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) 

ggsave(filename = "./graficos/uf_total_cor_sem_SP.png", device = "png",
       width = 15, height = 15, units = "cm")


# GRAFICO 13 - razao cor (branco/(pretos e pardos)) #######################################
# com o data pré gráfico 12 
# LIBERAR EIXO Y

data13 = data %>% 
  pivot_wider(
    names_from = 'cor_descricao',
    values_from = 'total') %>%
  mutate_at(c("Indígena","Branca","Preta","Amarela","Parda") , ~ ifelse(is.na(.),0,.)) %>% 
  mutate(
    razao_b_pp = round(Branca/(Preta+Parda),1)
  ) %>% 
  select(ano,codigo_uf,razao_b_pp)
  

ggplot() +
  geom_line(data = data13,
            aes(x=ano, y=razao_b_pp)) +
  facet_wrap(~codigo_uf, scales = "free") +
  
  
  labs(title = 'Unidades da Federação - Razão de vínculos no poder Executivo e nível estadual - Cor (2004-2021)',
       subtitle = 'Número de vínculos de pessoas na cor Branca para cada um vínculo de pessoas na cor Preta ou Parda',
       caption = 'Fonte: Rais') +
  ylab('Razão de vínculos') +
  theme_ipea() +
  #scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                   limits = c(0,8.3e5),
  #                  breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-5))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x  = element_text(size = 20),
    axis.text.y = element_text(size = 40),
    strip.text = element_text(size = 40),
    plot.caption = element_text(size = 30)
    )

ggsave(filename = "./graficos/uf_total_cor_razao.png", device = "png",
       width = 16, height = 10, units = "in")


########## # DADOS DE COR OU RAÇA
### NOVO - BARRAS 2004 E 2021 - PROP DE BRANCOS


data = base_completa$total_uf_cor

siglas = tibble::tibble(
  codigo = data$codigo_uf,
  sigla = data$sigla_uf
) %>% 
  distinct() %>% 
  na.omit() %>% 
  arrange(codigo)

data = data %>% 
  mutate(codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla),
         cor_descricao = factor(cor_descricao),
         total= as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano,codigo_uf,cor_descricao,total) %>% 
  na.omit() %>% 
  
  # NOVA PARTE
  
  dplyr::filter(ano %in% c(2004,2021)) %>% 
  group_by(ano,codigo_uf) %>% 
  mutate(total_ano = sum(total)) %>% 
  ungroup() %>% 
  
  dplyr::filter(cor_descricao == "Branca") %>% 
  mutate(prop_branca = round(total/total_ano,3))
  
data %>% 
  ggplot(aes(x=codigo_uf,y=prop_branca*100,fill = factor(ano))) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  scale_fill_manual(values = c("#1E69AB","#1EAB2C")) +
  
  labs(title = 'Unidades da Federação - Proporção de vínculos de cor branca',
       subtitle = 'Vínculos públicos no poder Executivo e nível estadual por ano e cor (2004 e 2021)',
       caption = 'Fonte: Rais.') +
  ylab('Proporção') +
  theme_ipea() + 
  theme(
    legend.position = "top",
    legend.title= element_blank(),
    legend.text = element_text(size = 30),
    
    axis.title.x = element_blank(),
    axis.text.x  = element_text(size = 40),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.title.y = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    
    plot.caption = element_text(size = 30),
    strip.text = element_text(size = 40)
  )

ggsave(filename = "./graficos/uf_total_cor_prop_branca.png", device = "png",
       width = 16, height = 10, units = "in")


# GRAFICO xx - texto 3 - remuneracao media UF nf ####################################

data = df11_2 ## codigo: 01_tabulacao_dados.R - linha 129

## ajuste
data3 = data %>% 
  rename(Federal = rem_media_federal_total,
         Estadual = rem_media_estadual_total,
         Municipal = rem_media_municipal_total) %>% 
  pivot_longer(!c(ano,codigo,uf), names_to = "categoria", values_to= "valor") %>% 
  mutate(categoria = factor(categoria,
                            levels = c('Federal','Estadual','Municipal'),
                            labels = c('Federal','Estadual','Municipal')),
         uf2 = factor(codigo,
                     levels = data$codigo,
                     labels = data$uf))


# 
# apoio =  base_completa$total_uf_sexo %>% 
#   select(codigo_uf,sigla_uf) %>%
#   distinct() %>% 
#   na.omit()
# 
# data = base_completa$rem_media_brasil_nf
# 
# data = left_join(data,apoio, by = join_by(uf == codigo_uf)) %>% 
#   na.omit() %>% 
#   mutate(codigo_uf = factor(uf,
#                             levels = siglas$codigo,
#                             labels = siglas$sigla))

ggplot() +
  geom_line(data = data3,
            aes(x=ano, y=valor, color=categoria)) +
  facet_wrap(~uf2,ncol=4) +
  
  labs(caption = 'Fonte: Rais. Nota: valores em setembro de 2023.') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  #scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                   limits = c(0,8.3e5),
  #                  breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 14),
    
    #plot.title = element_text(size = 40),
    #plot.subtitle = element_text(size = 30),
    axis.title.y = element_text(size = 12),
    axis.text.x  = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 14),
    plot.caption = element_text(size = 10))

ggsave(filename = "./graficos/uf_rem_media_nivel_federativo.png", device = "png",
       width = 16, height = 10, units = "in")

ggsave(filename = "./graficos/uf_rem_media_nivel_federativo.png",
       plot = last_plot(), width = 9.10, height = 11.10, units = "in", dpi = 300)


# GRAFICO 16 - remuneracao media UF sexo ####################################

## ajuste
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

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=razao_rem_media)) +
  facet_wrap(~codigo_uf) +
  
  labs(title = 'Unidades da Federação - Razão de remuneração média entre homens e mulheres',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre homens e mulheres') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  #scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                   limits = c(0,8.3e5),
  #                  breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    strip.text = element_text(size = 40),
    plot.caption = element_text(size = 30))

ggsave(filename = "./graficos/uf_razao_rem_media_sexo.png", device = "png",
       width = 16, height = 10, units = "in")




### NOVO - BARRAS 2021 - REMUNERACAO MÉDIA - DESTAQUE DF


data = base_completa$rem_media_uf %>% 
  dplyr::filter(ano == 2021) %>% 
  mutate(codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla),
         rem_media = round(remuneracao_media_executivo_estadual),
         destaque = ifelse(codigo_uf=="DF","1","0")) %>% 
  select(ano,codigo_uf,destaque,rem_media)
  

data %>% 
  ggplot(aes(x=codigo_uf,y=rem_media,fill=destaque)) +
  geom_col() +
  geom_text(aes(label = rem_media), nudge_y = 300,
            size = 15) +
  
  scale_fill_manual(values = c("#1E69AB","#1EAB2C")) +
  
  labs(title = 'Unidades da Federação - Remuneração média',
       subtitle = 'Vínculos públicos no poder Executivo e nível estadual em 2021',
       caption = 'Fonte: Rais.') +
  ylab('Remuneração em reais') +
  theme_ipea() + 
  theme(
    legend.position = "none",
    
    axis.title.x = element_blank(),
    axis.text.x  = element_text(size = 40),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    
    plot.caption = element_text(size = 30),
    strip.text = element_text(size = 30)
  ) 

ggsave(filename = "./graficos/uf_rem_media_2021_barras.png", device = "png",
       width = 16, height = 10, units = "in")




# GRAFICO 17 - remuneracao media UF cor ####################################

## ajuste
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

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=razao_rem_media)) +
  facet_wrap(~codigo_uf) +
  
  
  labs(title = 'Unidades da Federação - Razão de remuneração média entre brancos e não brancos',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por ano (2004-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre brancos e não brancos.') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  #scale_y_continuous(limits = c(0.49,1.75)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-5))) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    strip.text = element_text(size = 40),
    plot.caption = element_text(size = 30))

ggsave(filename = "./graficos/uf_razao_rem_media_cor.png", device = "png",
       width = 16, height = 10, units = "in")

  
################
################
################
################
################
################

# detalhe DF



# GRAFICO xx - remuneracao media e mediana DF ####################################

## ajuste
apoio =  base_completa$total_uf_sexo %>% 
  select(codigo_uf,sigla_uf) %>%
  distinct() %>% 
  na.omit()

data = base_completa$rem_media_uf %>% 
  na.omit() %>% 
  filter(codigo_uf==53) %>% 
  mutate(codigo_uf = factor(codigo_uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  select(ano,codigo_uf,`Média` = remuneracao_media_executivo_estadual,
         `Mediana` = remuneracao_mediana_executivo_estadual) %>% 
  pivot_longer(!c(ano,codigo_uf), names_to = "categoria", values_to= "valor"
    
  )
  
  

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=valor,color=categoria)) +
  ### facet_wrap(~codigo_uf) +
  
  
  scale_color_ipea() + 
  #scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Distrito Federal - Remuneração média e mediana',
       subtitle = 'Remuneração média e mediana de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023.') +
  ylab('Remuneração em reais') +
  theme_ipea() +
   scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
     limits = c(0,12e3),
     breaks = seq(0,11e3,2e3)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
    theme(
      axis.title.x = element_blank(),
      #legend.position.inside = 
      legend.title = element_blank(),
      
      legend.position = "inside",
      legend.justification = c(0.25,0.99),
      legend.direction = "horizontal",
      
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_text(size = 15),
      axis.text.x  = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.y.left = element_text(vjust = -3),
      plot.caption = element_text(size = 10)
    ) 

ggsave(filename = "./graficos/DF_remuneracao_media_mediana.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 18 - remuneracao media DF sexo ####################################

## ajuste
apoio =  base_completa$total_uf_sexo %>% 
  select(codigo_uf,sigla_uf) %>%
  distinct() %>% 
  na.omit()

data = base_completa$rem_media_brasil_sexo_uf

data = left_join(data,apoio, by = join_by(uf == codigo_uf)) %>% 
  na.omit() %>% 
  mutate(codigo_uf = factor(uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  
  filter(uf==53)

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=razao_rem_media)) +
  ### facet_wrap(~codigo_uf) +
  
  
  scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Distrito Federal - Razão de remuneração média entre homens e mulheres',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre homens e mulheres') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
    limits = c(0.75,1.25),
    breaks = seq(0.8,1.2,.1)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/DF_remuneracao_media_razao_sexo.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO 19 - remuneracao media DF cor ####################################

## ajuste
apoio =  base_completa$total_uf_cor %>% 
  select(codigo_uf,sigla_uf) %>%
  distinct() %>% 
  na.omit()

data = base_completa$rem_media_brasil_cor_uf

data = left_join(data,apoio, by = join_by(uf == codigo_uf)) %>% 
  na.omit() %>% 
  mutate(codigo_uf = factor(uf,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  
  filter(uf==53)

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=razao_rem_media)) +
  ### facet_wrap(~codigo_uf) +
  
  
  scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Distrito Federal - Razão de remuneração média entre brancos e não brancos',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023. O valor 1 indica remuneração média igual entre brancos e não brancos.') +
  ylab('Remuneração em reais') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
    limits = c(1,1.3),
    breaks = seq(1,1.3,.1)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-3))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/DF_remuneracao_media_razao_cor.png", device = "png",
       width = 10, height = 6, units = "cm")



# GRAFICO 20 - total DF ####################################

data = base_completa$total_uf_sexo %>% 
  filter(codigo_uf==53) %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual_feminino) + 
           as.numeric(vinculos_executivo_estadual_masculino)) %>% 
  select(ano,total)

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=total)) +

  scale_color_ipea() + 
  #scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Distrito Federal - Vínculos',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais.') +
  ylab('Vínculos') +
  theme_ipea() +
  # scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
  #   limits = c(0,2e5),
  #   breaks = seq(5e4)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = "top",
    legend.title = element_blank())

ggsave(filename = "./graficos/DF_total.png", device = "png",
       width = 10, height = 6, units = "cm")


# GRAFICO xx - prop mulher DF e brasil  ####################################

data_p1 = base_completa$total_brasil_sexo %>%
  mutate(prop_mulher = round(
    as.numeric(vinculos_executivo_estadual_feminino) /
      (as.numeric(vinculos_executivo_estadual_feminino) + 
           as.numeric(vinculos_executivo_estadual_masculino)) , 4),
    local = "Brasil") %>% 
  select(ano,local,prop_mulher)

data_p2 = base_completa$total_uf_sexo %>% 
  filter(codigo_uf==53) %>% 
  mutate(prop_mulher = round(
    as.numeric(vinculos_executivo_estadual_feminino) /
      (as.numeric(vinculos_executivo_estadual_feminino) + 
         as.numeric(vinculos_executivo_estadual_masculino)) , 4),
    local = "Distrito Federal") %>% 
  select(ano,local,prop_mulher)

data = rbind(data_p1,data_p2)


ggplot(data = data,
       aes(x=ano, y=prop_mulher,color=local)) +
  geom_line() +
  geom_text(aes(label = round(prop_mulher*100)), nudge_y = 0.005) +
  scale_color_ipea() + 
  scale_color_manual(values = c('#006450','#0064ff')) +
  labs(title = 'Brasil e Distrito Federal - Proporção de mulheres',
       subtitle = 'Vínculos nos estados no Brasil e no Distrito Federal (1985-2021)',
       caption = 'Fonte: Rais.') +
  ylab('Proporção') +
  theme_ipea() +
  # scale_y_continuous(#labels = unit_format(unit = "M", scale = 1e-6),
  #   limits = c(0,2e5),
  #   breaks = seq(5e4)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      
      
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x  = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      plot.caption = element_text(size = 10)
    ) 
  

ggsave(filename = "./graficos/DF_BR_proporcao_mulher.png", device = "png",
       width = 10, height = 6, units = "cm")

# GRAFICO xx - total cor DF ####################################

data = base_completa$total_uf_cor

data = data %>% 
  filter(codigo_uf==53) %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual),
         cor_descricao = as.factor(cor_descricao)) %>% 
  select(ano, cor, cor_descricao, total) %>% 
  
  group_by(ano) %>% 
  mutate(total_ano = sum(total)) %>% 
  ungroup() %>% 
  mutate(prop_cor = round(total/total_ano*100,4))


ggplot() +
  geom_line(data = data, aes(x=ano, y=prop_cor, color=cor_descricao)) +
  
  scale_color_ipea() + 
  scale_color_manual(values = c('#FDE725','#7AD151','#22A884','#2A788E','#414487')) +
  labs(title = 'Distrito Federal - Vínculos - Cor',
       subtitle = 'Proporção de vínculos públicos no poder Executivo e nível estadual por ano e cor (2004-2021)',
       caption = 'Fonte: Rais') +
  ylab('Proporção') +
  theme_ipea() +
  scale_y_continuous(limits = c(0,85),
                     breaks = seq(0,80,20)) +
  scale_x_continuous(limits = c(2003,2022), breaks = sort(seq(2021,2004,-3))) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    
    
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    plot.caption = element_text(size = 10)
    )

ggsave(filename = "./graficos/df_proporcao_cor.png", device = "png",
       width = 10, height = 6, units = "cm")

# variacao do DF - total 
# 


data = base_completa$total_uf_sexo

data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual_feminino) +
          as.numeric(vinculos_executivo_estadual_masculino)  ) %>% 
  select(ano, codigo_uf,sigla_uf, total)


variacao = data %>% filter(
  ano %in% c(1985,1995,2005,2015,2021),
  codigo_uf == 53
) %>% mutate(
  lag = lag(total),
  variacao_absoluta = total - lag,
  descricao_periodo = c("","1985-1995","1995-2005","2005-2015","2015-2021"),
  variacao_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,variacao_absoluta,variacao_relativa)

variacao2 = data %>% filter(
  ano %in% c(1985,2021), codigo_uf == 53
) %>% mutate(
  lag = lag(total),
  variacao_absoluta = total - lag,
  descricao_periodo = c("","1985-2021"),
  variacao_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,variacao_absoluta,variacao_relativa)

variacao = rbind(
  variacao, variacao2
) %>% mutate(descricao_periodo = factor(
  descricao_periodo, levels = descricao_periodo, labels=descricao_periodo
))

texto_caption = paste0(
  'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
  round(variacao$variacao_relativa[1]),"%, ",
  round(variacao$variacao_relativa[2]),"%, ",
  round(variacao$variacao_relativa[3]),"%, ",
  round(variacao$variacao_relativa[4]),"% e ",
  round(variacao$variacao_relativa[5]),"%.")


variacao %>% 
  ggplot(aes(x=descricao_periodo, y=variacao_absoluta,fill=descricao_periodo)) +
  geom_col() +
  scale_color_ipea() + 
  scale_fill_manual(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Distrito Federal - Vínculos',
       subtitle = 'Variação em períodos de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = texto_caption) +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(#labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(-20e3,70e3),
                     breaks = seq(-20e3,70e3,20e3)) +

  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    
    
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    axis.title.y.left = element_text(vjust = -3),
    axis.text.x  = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    plot.caption = element_text(size = 10)
    )

ggsave(filename = "./graficos/DF_total_variacao.png", device = "png",
       width = 10, height = 6, units = "cm")

