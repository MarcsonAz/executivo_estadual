# graficos apoio
# 
# 
# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ipeaplot','scales','ggtext','viridis'),
  character.only = TRUE)


## SERIE BRASIL ###############################################################
## 

# dados
{ 
source('./codigos/dfs_para_relatorio.R')

data = base_completa$total_brasil

data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano, total)


variacao = data %>% filter(
  ano %in% c(1985,1995,2005,2015,2021)
) %>% mutate(
  lag = lag(total),
  variacao_absoluta = total - lag,
  descricao_periodo = c("","1985-1995","1995-2005","2005-2015","2015-2021"),
  variaca_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,variacao_absoluta,variaca_relativa)

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
    descricao_periodo,variacao_absoluta,variaca_relativa)

variacao = rbind(
  variacao, variacao2
) %>% mutate(descricao_periodo = factor(
  descricao_periodo, levels = descricao_periodo, labels=descricao_periodo
))

texto_caption = paste0(
    'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
    round(variacao$variaca_relativa[1]),"%, ",
    round(variacao$variaca_relativa[2]),"%, ",
    round(variacao$variaca_relativa[3]),"%, ",
    round(variacao$variaca_relativa[4]),"% e ",
    round(variacao$variaca_relativa[5]),"%.")

}
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


variacao3 = data %>% 
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

texto = list(mulher = round(variacao3$variacao_relativa[variacao3$sexo=="Mulher"]),
             homem = round(variacao3$variacao_relativa[variacao3$sexo=="Homem"]))


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

variacao3 %>% 
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
    legend.position = 'none'
    )

ggsave(filename = "./graficos/brasil_variacao_sexo.png", device = "png",
       width = 10, height = 6, units = "cm")




## SERIE BRASIL COR ##########################################################
## 

data = base_completa$total_brasil_cor

data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual),
         cor_descricao = as.factor(cor_descricao)) %>% 
  select(ano, cor, cor_descricao, total)


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


# GRAFICO 10 - TOTAL #######################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total)) +
  scale_color_ipea() +
  facet_wrap(~codigo_uf) +
  
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades)',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,8.3e5),
                     breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    axis.title.x = element_blank(),
  ) 

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

data2 = data %>% select(ano,codigo_uf,total_mulher,total_homem) %>% 
  pivot_longer(-c('ano','codigo_uf'),
               names_to = 'sexo',
               values_to = 'total') %>% 
  mutate(sexo = factor(sexo,
                       levels = c('total_mulher','total_homem'),
                       labels = c('Mulher','Homem')
  ))


# GRAFICO 12 - TOTAL cor #######################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total,group=cor_descricao,color=cor_descricao)) +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~codigo_uf) +
  
  
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
    legend.title = element_blank()
  ) 

ggsave(filename = "./graficos/uf_total_cor.png", device = "png",
       width = 15, height = 15, units = "cm")

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
  facet_wrap(~codigo_uf) +
  
  
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
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    strip.text = element_text(size = 40))

ggsave(filename = "./graficos/uf_total_cor_razao.png", device = "png",
       width = 16, height = 10, units = "in")

