#
# REVISAO FINAL - FEVEREIRO DE 2025
# graficos TEXTO - BUROCRACIA NOS ESTADOS

###############################################
# OBJETIVO                                    #
# GERACAO DE GRÁFICOS                         #
#                                             #
###############################################


# Development version
#utils::remove.packages('ipeaplot')
#remotes::install_github("ipeadata-lab/ipeaplot")
#install.packages('pacman')

pacman::p_load(
  c('dplyr','tidyr','ggplot2','ggrepel','ipeaplot',
    'scales','ggtext','viridis','grid'),
  character.only = TRUE)

### DADOS ####################################################################
{ 
  source('./codigos/01_tabulacao_dados.R')
  source('./apoio/dfs_para_relatorio.R')
  
  nome_grafico <- function(nome="grafico_base"){
    return(paste0(
      "./graficos_final/",
      nome,
      ".png"
    ))
  }
  
  
  # data = base_completa$total_brasil
  # 
  # data = data %>% 
  #   mutate(total = as.numeric(vinculos_executivo_estadual)) %>% 
  #   select(ano, total)
  # 
  # 
  # variacao = data %>% filter(
  #   ano %in% c(1985,1995,2005,2015,2021)
  # ) %>% mutate(
  #   lag = lag(total),
  #   variacao_absoluta = total - lag,
  #   descricao_periodo = c("","1985-1995","1995-2005","2005-2015","2015-2021"),
  #   variacao_relativa = round(((total/lag)-1)*100,1)
  # ) %>% 
  #   na.omit() %>% 
  #   select(
  #     descricao_periodo,variacao_absoluta,variacao_relativa)
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
  #     descricao_periodo,variacao_absoluta,variacao_relativa)
  # 
  # variacao = rbind(
  #   variacao, variacao2
  # ) %>% mutate(descricao_periodo = factor(
  #   descricao_periodo, levels = descricao_periodo, labels=descricao_periodo
  # ))
  # 
  # texto_caption = paste0(
  #   'Fonte: Rais. Nota: a variação relativa para para os respectivos períodos são: ',
  #   round(variacao$variacao_relativa[1]),"%, ",
  #   round(variacao$variacao_relativa[2]),"%, ",
  #   round(variacao$variacao_relativa[3]),"%, ",
  #   round(variacao$variacao_relativa[4]),"% e ",
  #   round(variacao$variacao_relativa[5]),"%.")
  
}

###
###
###  GRÁFICOS
###
###

###  UF - TOTAL #######################################


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

# data2 = data %>% select(ano,codigo_uf,total_mulher,total_homem) %>% 
#   pivot_longer(-c('ano','codigo_uf'),
#                names_to = 'sexo',
#                values_to = 'total') %>% 
#   mutate(sexo = factor(sexo,
#                        levels = c('total_mulher','total_homem'),
#                        labels = c('Mulher','Homem')
#   ))
# 
# data3 = data2 %>% 
#   filter(ano %in% c(1985,2021)) %>%
#   pivot_wider(names_from = ano, values_from = total) %>% 
#   mutate(`1985` = ifelse(is.na(`1985`),0,`1985`),
#          `2021` = ifelse(is.na(`2021`),0,`2021`)) %>% 
#   mutate(diff = `2021` - `1985`)

# GRAFICO  - TOTAL 

# liberar o eixo Y, em mil unidades para diminiur label no eixo Y

#dd = data
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
    plot.caption = element_text(size = 30),
    
    panel.spacing = unit(0.5, "lines")
  )


ggsave(filename = nome_grafico("total_brasil"), 
       device = "png",
       width = 21, height = 18, units = "cm")



###  BRASIL - REMUNERAÇÃO MÉDIA - NIVEL FEDERATIVO #######################################

# GRAFICO - remuneracao media brasil por nivel federativo 

data = base_completa$rem_media_brasil_nf %>% 
  pivot_longer(cols = remuneracao_media_federal:remuneracao_media_municipal,
               names_to = 'categoria')

ggplot() +
  geom_line(data = data, aes(x=ano, y=value, color=categoria),
            linewidth = 1.5) +
  
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
    legend.text = element_text(size = 20),
    
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    
    title = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 20),
    axis.ticks.y = element_blank(),
    
    plot.caption = element_text(size = 30)
  )

ggsave(filename = nome_grafico("brasil_remuneracao_media_nivelfederativo"), 
       device = "png",
       width = 21, height = 11, units = "cm")



###  UF - REMUNERAÇÃO MÉDIA - NIVEL FEDERATIVO #######################################

# GRAFICO - remuneracao media UF sexo 
siglas = tibble::tibble(
  codigo = base_completa$total_uf_sexo$codigo_uf,
  sigla = base_completa$total_uf_sexo$sigla_uf
) %>% 
  distinct() %>% 
  na.omit() %>% 
  arrange(codigo)

data = tabela$rem_media_uf_nf

data = data %>% mutate(uf = factor(codigo,
                            levels = siglas$codigo,
                            labels = siglas$sigla)) %>% 
  pivot_longer(cols = rem_media_federal_total:rem_media_municipal_total,
               names_to = 'categoria') %>% 
  mutate(categoria = factor(categoria,
                     levels = c("rem_media_federal_total","rem_media_estadual_total","rem_media_municipal_total"),
                     labels = c("Federal","Estadual","Municipal")))

ggplot() +
  geom_line(data = data,
            aes(x=ano, y=value, color=categoria)) +
  facet_wrap(~uf) +
  
  labs(title = 'Unidades da Federação - Remuneração média por nível federativo',
       subtitle = 'Remuneração média de vínculos públicos no poder Executivo e nível estadual por nível federativo (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores em setembro de 2023.') +
  ylab('Remuneração em reais') +
  #scale_color_discrete(label=c("Federal","Estadual","Municipal")) +
  theme_ipea() +
  #scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
  #                   limits = c(0,8.3e5),
  #                  breaks = seq(0,8e5,2e5)) +
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-10))) +
  theme(
    
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_blank(),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    
    strip.text = element_text(size = 40),
    plot.caption = element_text(size = 30),
    panel.spacing = unit(0.5, "lines")
    )

ggsave(filename = nome_grafico("rem_media_nivel_federativo"), 
       device = "png",
       width = 21, height = 18, units = "cm")


###  UF - REMUNERAÇÃO MÉDIA - BARRAS 2021 #######################################

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
            size = 8) +
  
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

ggsave(filename = nome_grafico("uf_rem_media_2021_barras"), 
       device = "png",
       width = 21, height = 11, units = "cm")

###  BRASIL - REMUNERAÇÃO MÉDIA NOS QUINTOS #######################################

# GRAFICO 14 - remuneracao media no decil brasil linhas 

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
  theme_ipea() + 
  labs(title = 'Brasil - Média nos quintos de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023') + 
  ylab('Remuneração em reais') +
  guides(color = guide_legend(title = "Quintos")) +
  scale_y_continuous(limits = c(0,15e3), breaks = seq(0,15e3,2e3)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
  
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_blank(),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    
    plot.caption = element_text(size = 30),
    plot.caption.position = "plot"
)

ggsave(filename = nome_grafico("rem_media_quintos"), 
       device = "png",
       width = 21, height = 11, units = "cm")


###  BRASIL - RAZAO DE REMUNERAÇÃO ENTRE 9 DECIL E 1 DECIL##############################


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

ggplot(data2, aes(ano, remuneracao)) + 
  geom_line() +
  theme_ipea() +
  labs(title = 'Brasil - Razão entre o nono e primeiro decil de remuneração',
       subtitle = 'Remuneração de vínculos públicos no poder Executivo e nível estadual (1985-2021)',
       caption = 'Fonte: Rais. Nota: valores corrigidos para setembro de 2023') +
  ylab('Razão entre decis das remuneraçãoes') +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) + 
  scale_x_continuous(limits = c(1984,2022), breaks = sort(seq(2021,1985,-5))) +
  theme(
    
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 30),
    
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_blank(),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    
    plot.caption = element_text(size = 30),
    plot.caption.position = "plot"
  ) 

ggsave(filename = nome_grafico("rem_decis_razao_9_1"), 
       device = "png",
       width = 21, height = 11, units = "cm")

### NOVO - BARRAS 2004 E 2021 - PROP DE COR OU RAÇA ##############################


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


# GRAFICO - razao cor (branco/(pretos e pardos)) #######################################

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


#  barras (pretos e pardos))  2021 variacao na forca de trabalho e publicos ################

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



#  uf razao remuneracao (pretos e pardos)) ###############################

# GRAFICO - remuneracao media UF cor 

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



#  uf total sexo ###################################################################


# GRAFICO  - TOTAL SEXO LIBERAR EIXO Y 

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



#  uf barras total sexo variacao 2021 e 2004###################################################


# GRAFICO 11.3 - TOTAL SEXO variacao 

data3 = data2 %>% 
  filter(ano %in% c(1985,2021)) %>%
  pivot_wider(names_from = ano, values_from = total) %>% 
  mutate(`1985` = ifelse(is.na(`1985`),0,`1985`),
         `2021` = ifelse(is.na(`2021`),0,`2021`)) %>% 
  mutate(diff = `2021` - `1985`)

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


#  uf razao remuneracao sexo ###################################################

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



#  ocupacoes dispersao prop de PP com rem media 2021 ##########################################

# colocar dados na tabulacao - ocupacao_cor
## ajuste
apoio =  df2 %>% 
  select(codigo_uf=codigo,sigla_uf=uf) %>%
  distinct() %>% 
  na.omit()

tt = left_join(tt,apoio, by = join_by(uf_ipea == codigo_uf))

tt %>% ggplot(aes(x=prop_cor_pre_par,y=rem_media,color=factor(sigla_uf))) + 
  geom_point() + theme_ipea() +
  labs(y='Remuneração média de ocupações dentro da UF',
  x='Proporção de pretos ou pardos em ocupações dentro da UF') +
  scale_y_continuous(limits = c(0,3e4), breaks = seq(0,3e4,5e3)) + 
  guides(color = guide_legend(title = "Unidade da Federação")) +
  theme(
      
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30)
  ) 

ggsave(filename = nome_grafico("dispersao_ocupacao_cor_rem_media"), 
       device = "png",
       width = 21, height = 11, units = "cm")

#  ocupacoes dispersao prop de mulheres com rem media 2021 ##########################################

# colocar dados na tabulacao - ocupacao_sexo

tt %>% ggplot(aes(x=prop_sexo_mulher,y=rem_media,color=factor(sigla_uf))) + 
  geom_point() + theme_ipea() +
  ylab('Remuneração média de ocupações dentro da UF') +
  xlab('Proporção de mulheres em ocupações dentro da UF') +
  scale_y_continuous(limits = c(0,3e4), breaks = seq(0,3e4,5e3)) + 
  guides(color = guide_legend(title = "Unidade da Federação")) +
  theme(
    
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 30),
    
    axis.text.x = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  ) 

ggsave(filename = nome_grafico("dispersao_ocupacao_sexo_rem_media"), 
       device = "png",
       width = 21, height = 11, units = "cm")



# sexo X cor

tab16 %>% ggplot(aes(x=sexo_cor,y=rem_media)) + 
  geom_boxplot() + theme_ipea() +
  ylab('Remuneração média de ocupações') +
  xlab('Sexo e cor') +
  scale_y_continuous(limits = c(0,3e4), breaks = seq(0,3e4,5e3)) + 
  theme(
    
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30)
  ) 

ggsave(filename = nome_grafico("boxplot_ocupacao_sexo_cor_rem_media"), 
       device = "png",
       width = 21, height = 18, units = "cm")


#######
#######
#######
#######
#######  APENDICE

#  variacao cor 2021 e 2004 barras ##########################################

# GRAFICO - variacao cor 
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


#  uf - total cor  #############################################################

#DADOS DE COR OU RAÇA


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


data %>% filter(codigo_uf=="RS") %>%
  pivot_wider(names_from = cor_descricao, values_from = total) %>% 
  mutate(razao = Branca/(Preta+Parda)) %>% 
ggplot(aes(x=ano, y=razao)) +
  geom_line() #+
  scale_color_viridis_d(direction = -1)

# GRAFICO 12 - TOTAL cor 

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


