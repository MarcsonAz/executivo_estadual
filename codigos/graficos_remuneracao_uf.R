# graficos UF remuneracao

 
# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ipeaplot','scales','ggtext','viridis'),
  character.only = TRUE)


## SERIE UF REMUNERACAO ##########################################################
## 

# dados
source('./codigos/dfs_para_relatorio.R')

#### DADOS PARA REMUNERACAO MEDIA ####

{ 
  data = base_completa$rem_media_uf
  
  siglas = tibble::tibble(
      codigo = data$codigo_uf,
      sigla = data$sigla_uf) %>% 
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
    mutate(codigo_uf = factor(codigo_uf,
                              levels = siglas$codigo,
                              labels = siglas$sigla)) %>% 
    select(ano,codigo_uf, 
           media = remuneracao_media_executivo_estadual,
           mediana = remuneracao_mediana_executivo_estadual) %>% 
    mutate(
      media = round(ifelse(is.na(media),0,media),2),
      mediana = round(ifelse(is.na(mediana),0,mediana),2)) %>% 
    
    pivot_longer(!c('ano','codigo_uf'),
                 names_to = 'categoria',
                 values_to = 'remuneracao')
  
  # variacao = data %>% filter(
  #   ano %in% c(1985,1995,2005,2015,2021)
  # ) %>% mutate(
  #   lag = lag(total),
  #   variacao_absoluta = total - lag,
  #   descricao_periodo = c("","1985-1995","1995-2005","2005-2015","2015-2021"),
  #   variaca_relativa = round(((total/lag)-1)*100,1)
  # ) %>% 
  #   na.omit() %>% 
  #   select(
  #     descricao_periodo,variacao_absoluta,variaca_relativa)
  # 
  # variacao2 = data %>% filter(
  #   ano %in% c(1985,2021)
  # ) %>% mutate(
  #   lag = lag(total),
  #   variacao_absoluta = total - lag,
  #   descricao_periodo = c("","1985-2021"),
  #   variacao_relativa = round(((total/lag)-1)*100,1)
  # ) %>% 
  #   na.omit() %>% 
  #   select(
  #     descricao_periodo,variacao_absoluta,variaca_relativa)
  # 
  # variacao = rbind(
  #   variacao, variacao2
  # ) %>% mutate(descricao_periodo = factor(
  #   descricao_periodo, levels = descricao_periodo, labels=descricao_periodo
  # ))
  # 
  # texto_caption = paste0(
  #   'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
  #   round(variacao$variaca_relativa[1]),"%, ",
  #   round(variacao$variaca_relativa[2]),"%, ",
  #   round(variacao$variaca_relativa[3]),"%, ",
  #   round(variacao$variaca_relativa[4]),"% e ",
  #   round(variacao$variaca_relativa[5]),"%.")
  # 
  
  rm(base_ano_uf)
  rm(siglas)
  
}


#### FIM DE DADOS PARA REMUNERACAO MEDIA ####
#### 

# GRAFICO 14 - UF remuneracao media e mediana ####################################
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
