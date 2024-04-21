# graficos UF total


# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ipeaplot','scales','ggtext','viridis'),
  character.only = TRUE)


## SERIE UF TOTAL #############################################################
## 

# dados
source('./codigos/dfs_para_relatorio.R')

#### DADOS PARA TOTAL ####

{ 
  data = base_completa$total_uf_sexo
  
  siglas = tibble::tibble(
    codigo = data$codigo_uf,
    sigla = data$sigla_uf
  ) %>% 
    distinct() %>% 
    na.omit() %>% 
    arrange(codigo)
  
  anos = sort(unique(data$ano))
  
  base_ano_uf <- tibble::tibble(
    ano = as.numeric(
      paste0(rep(seq(anos[1],anos[length(anos)]), 
                 each = length(siglas$codigo)))),
    codigo_uf = rep(siglas$codigo, length(anos))
  )
  
  # ajuste dados faltantes
  data = base_ano_uf %>% 
    left_join(data) %>%
    mutate(total_mulher = ifelse(is.na(vinculos_executivo_estadual_feminino),0,
                                 as.numeric(vinculos_executivo_estadual_feminino)),
           total_homem = ifelse(is.na(vinculos_executivo_estadual_masculino),0,
                                 as.numeric(vinculos_executivo_estadual_masculino)),
           total = total_mulher + total_homem,
           codigo_uf = factor(codigo_uf,
                              levels = siglas$codigo,
                              labels = siglas$sigla)) %>% 
    select(ano,codigo_uf,total,total_homem,total_mulher) %>% 
    na.omit()
  
  # DATA RELATIVO
  
  
  
  # DATA SEXO
  
  data_sexo = data %>% select(ano,codigo_uf,total_mulher,total_homem) %>% 
    pivot_longer(-c('ano','codigo_uf'),
                 names_to = 'sexo',
                 values_to = 'total') %>% 
    mutate(sexo = factor(sexo,
                         levels = c('total_mulher','total_homem'),
                         labels = c('Mulher','Homem')
    ))
  
  rm(base_ano_uf)
  rm(siglas)
  
}


#### FIM DE DADOS PARA TOTAL ####
#### 


#### GRAFICOS DE UF PARA TOTAL ####
#### 



# GRAFICO 10 - TOTAL ######################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total)) +
  facet_wrap(~codigo_uf) +
  
  theme_ipea() +
  scale_color_ipea() +
  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,8.3e5),
                     breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades)',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais',
       y = 'Vínculos (mil unidades)') +
  
  theme(
    legend.position = "none",
    #legend.title = element_blank(),
    
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 40),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 30),
    
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 40))

ggsave(filename = "./graficos/uf_total.png", device = "png",
       width = 16, height = 10, units = "in")



# GRAFICO 10.1 - TOTAL sem sp ###################################

ggplot() +
  geom_line(data = data %>% filter(codigo_uf!="SP"), 
            aes(x=ano, y=total)) +
  facet_wrap(~codigo_uf) +
  
  theme_ipea() +
  scale_color_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                    limits = c(0,3.8e5),
                    breaks = seq(0,4e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sem São Paulo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
 
  theme(
    legend.position = "none",
    #legend.title = element_blank(),
    
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 40),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 30),
    
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 40))

ggsave(filename = "./graficos/uf_total_sem_sp.png", device = "png",
       width = 16, height = 10, units = "in")



# GRAFICO 10.2 - TOTAL RELATIVO ###################################
# ANO BASE 1990 = 1 UNIDADE

ggplot() +
  geom_line(data = data %>% filter(codigo_uf!="SP"), 
            aes(x=ano, y=total)) +
  facet_wrap(~codigo_uf) +
  
  theme_ipea() +
  scale_color_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     limits = c(0,3.8e5),
                     breaks = seq(0,4e5,1e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  
  labs(title = 'Unidades da Federação - Vínculos (mil unidades) - Sem São Paulo',
       subtitle = 'Total de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais') +
  ylab('Vínculos (mil unidades)') +
  
  theme(
    legend.position = "none",
    #legend.title = element_blank(),
    
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 40),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 30),
    
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 40))

ggsave(filename = "./graficos/uf_total_sem_sp.png", device = "png",
       width = 16, height = 10, units = "in")


















# GRAFICO 14 - UF remuneracao media e mediana
#

ggplot() +
  geom_line(data = data, aes(x=ano, y=remuneracao, color=categoria)) +
  facet_wrap(~codigo_uf) +
  
  theme_ipea() +
  
  scale_color_ipea() + 
  scale_color_manual(values = c('#006450','#0064ff'),labels = c('Média','Mediana')) +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(0,11200),
                     breaks = seq(0,10990,2000)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  
  labs(title = 'Brasil - Remuneração',
       subtitle = 'Remuneração média e mediana de vínculos públicos no poder Executivo e nível estadual por ano (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023',
       y = 'Remuneração em mil reais') +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 40),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 30),
    
    axis.text.x  = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 40))

ggsave(filename = "./graficos/uf_remuneracao_media_mediana.png", device = "png",
       width = 16, height = 10, units = "in")










































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


#### FIM DE GRAFICOS DE UF PARA TOTAL ####
####
