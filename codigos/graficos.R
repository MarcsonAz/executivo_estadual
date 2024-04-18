# graficos apoio
# 
# 
# Development version
utils::remove.packages('ipeaplot')
remotes::install_github("ipeadata-lab/ipeaplot")

require(ipeaplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(scales)
library(ggtext)



## SERIE BRASIL ###############################################################
## 

# dados
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
  variaca_relativa = round(((total/lag)-1)*100,1)
) %>% 
  na.omit() %>% 
  select(
    descricao_periodo,variacao_absoluta,variaca_relativa)

variacao = rbind(
  variacao, variacao2
) %>% mutate(descricao_periodo = factor(
  descricao_periodo, levels = descricao_periodo, labels=descricao_periodo
))


# GRAFICO 1 - TOTAL #######################################

ggplot() +
  geom_line(data = data, aes(x=ano, y=total)) +
  scale_color_ipea() +
  labs(title = 'Brasil - Vínculos públicos no poder Executivo e nível estadual') +
  ylab('Vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0,4e6),
                     breaks = seq(0,4e6,0.5e6)) +
  theme(
    axis.title.x = element_blank()
  )

ggsave(filename = "./graficos/brasil_total.png", device = "png",
       width = 10, height = 5, units = "cm")



# GRAFICO 2 - variacao ####################################

variacao %>% 
  ggplot(aes(x=descricao_periodo, y=variacao_absoluta,fill=descricao_periodo)) +
  geom_col() +
  scale_color_ipea() + 
  scale_fill_manual(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Variação em períodos de vínculos públicos no poder Executivo e nível estadual') +
  ylab('Variação de vínculos') +
  theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "Mil", scale = 1e-3),
                     limits = c(-.75e6,.75e6),
                     breaks = seq(-.75e6,.75e6,0.25e6)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none")

ggsave(filename = "./graficos/brasil_total_variacao.png", device = "png",
       width = 10, height = 5, units = "cm")




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
  )

# g3

ggplot() +
  geom_line(data = data, aes(x=ano, y=total, color=sexo)) +
  
  #scale_color_ipea() + 
  scale_color_discrete() + #(values = c(rep('#006450',4),'#0064ff')) +
  labs(title = 'Brasil - Vínculos públicos no poder Executivo e nível estadual por sexo') +
       #subtitle = "Milhões de vínculos de <span style = 'color:#E69F00'>Mulher</span> e <span style = 'color:#E69FFF'>Homem</span>") +
  ylab('Vínculos (milhões de unidades)') +
  #theme_ipea() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-6),
                     limits = c(0,2.1e6),
                     breaks = seq(0,2e6,0.5e6)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = "none") 
  
  # 
  # annotate(
  #   'richtext',
  #   x = 2021,
  #   y = 1.9e6,
  #   label = "ACCURATE | NULL | <span style = 'color:#E69F00'>ERROR</span>",
  #   hjust = 1,
  #   vjust = 0, 
  #   #col = unhighlighed_col_darker, 
  #   size = 4,
  #   label.colour = NA,
  #   fill = NA
  # )

ggsave(filename = "./graficos/brasil_total_variacao.png", device = "png",
       width = 10, height = 5, units = "cm")








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
