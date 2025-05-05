# tabelas e graficos de segregacao

library(ggplot2)

resultados %>% ggplot(aes(x=ano,y=theil)) + geom_line() +
  ylim(c(0,0.3))




