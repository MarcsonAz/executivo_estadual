# graficos apoio
# 
# 
# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ipeaplot','scales','ggtext'),
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
  geom_line(data = data, aes(x=ano, y=total, color=cor_descricao), linewidth = 1) +
  
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




####################################################
####################################################
####################################################
## APOIO PLOTLY



library(plotly)

widget_file_size <- function(p,filename="grafico") {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, paste0(filename,".html")))
  f <- file.path(d, paste0(filename,".html"))
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
  cat(f)
  fs::file_move(f,paste0(getwd(),"/",filename,".html"))
}
#widget_file_size(p)
#widget_file_size(partial_bundle(p))

fig <- plot_ly(data, x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = '') 
fig <- fig %>% add_trace(x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),
                         name = 'Valor') 

limites = list(0,1.1*as.numeric(max(data$total)))
fig <- fig %>% layout(title = "Brasil - Vínculos públicos no poder Executivo no nível estadual",
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Vínculos",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = TRUE,
                                   range = limites))

fig
widget_file_size(fig,"v1")

# range = c(-4,4))


data = base_completa$total_brasil %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano, total)


fig <- plot_ly(data, x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = '') 
fig <- fig %>% add_trace(x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),
                         name = 'Valor') 

limites = list(0,1.1*as.numeric(max(data$total)))
fig <- fig %>% layout(title = "Brasil - Vínculos públicos no poder Executivo no nível estadual",
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Vínculos",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = TRUE,
                                   range = limites))

fig

widget_file_size(fig,"v2")


library(plotly)

fig <- plot_ly()
fig <- fig %>% add_bars(
  x = variacao$descricao_periodo,
  y = variacao$variacao_absoluta,
  marker = list(
    color = 'rgb(0,100,80)'
  ),
  name = 'expenses'
)

fig <- fig %>% add_bars(
  x = c("2016", "2017", "2018"),
  y = c(300,400,700),
  base = 0,
  marker = list(
    color = 'blue'
  ),
  name = 'revenue'
)

fig
