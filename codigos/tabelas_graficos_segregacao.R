# tabelas e graficos de segregacao

library(ggplot2)
library(patchwork)

{
g1 = resultados %>% ggplot(aes(x=ano,y=theil)) + geom_line() +
  ylim(c(0.0,0.55))

g2 = resultados %>% ggplot(aes(x=ano,y=diss)) + geom_line() +
  ylim(c(0.0,0.55))

g3 = resultados %>% ggplot(aes(x=ano,y=diss_hoff)) + geom_line() +
  ylim(c(0.0,0.55))

g4 = resultados %>% ggplot(aes(x=ano,y=km_hoff)) + geom_line() +
  ylim(c(0.0,0.3))

}
g1/g2/g3/g4


# dados co m o pacote

{
  g1 = resultados %>% ggplot(aes(x=ano,y=m_)) + geom_line() + ylim(c(0.0,0.55))
  
  g2 = resultados %>% ggplot(aes(x=ano,y=h_)) + geom_line() + ylim(c(0.0,0.55))
  
  g3 = resultados %>% ggplot(aes(x=ano,y=d_)) + geom_line() + ylim(c(0.0,0.55))

}
g1/g2/g3



#### grafico barrar por UF diss

res %>% ggplot(aes(x=uf,y=d_)) + 
  geom_col() + ylim(c(0,0.55)) +
  labs(title = "Dissimilaridade de cor (PP x BR) por UF")

res %>% ggplot(aes(x=uf,y=d_)) + 
  geom_col() + ylim(c(0,0.55)) +
  labs(title = "Dissimilaridade de sexo por UF")


#### grafico barrar por UF IM normalizado H

res %>% ggplot(aes(x=uf,y=h_)) + 
  geom_col() + ylim(c(0,0.3)) +
  labs(title = "Informação mútua normalizado de cor (PP x BR) por UF")

res %>% ggplot(aes(x=uf,y=h_)) + 
  geom_col() + ylim(c(0,0.3)) +
  labs(title = "Informação mútua normalizado de sexo por UF")

res %>% ggplot(aes(x=uf,y=h_)) + 
  geom_col() + ylim(c(0,0.3)) +
  labs(title = "Informação mútua normalizado de sexo e cor por UF")
