# correcao de proventos
# projeto de dados do DF
# tabela com 400 mil informações, porém com 233 mil informações de remuneração.
# dados de 2013 a 2023.

# vou aplicar correção monetário pelo ipca

##########################################
# Resultado serão valores de 2023
##########################################

###
# 2 etapas - 
#  - tabela com valores de correção
#  - caregar tabela com proventos e corrigir valores 

#copiei o código de atualização de remuneracao usado na rais, 
# original no diretorio de apoio


##########################################
# ETAPA 1

### correcao de valores

# os testes para validação serão baseados no resultado comparado com a
# calculaDOra do banco central de correcao de valores

# https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice

library(pacman)

p_load(dplyr,lubridate)

# conectar ao banco
source("./apoio/conectar.R")

# consultas de dados de apoio

qq <- paste("SELECT ano, mes, salario_minimo_em_real_ipeadata, salario_minimo_em_real_calculado, 
moeda, fator_de_conversao_para_o_real, ipca, inpc, salario_na_moeda_do_ano, obs
FROM site_2022.gabarito_correcao_inflacao;")

df <- DBI::dbGetQuery(con, qq)

formato_ano_mes <- function(ano,mes){
  ano = as.numeric(ano)
  mes = as.numeric(mes)
  
  if(!(mes %in% 1:12)) return(cat("valor inválido para mês - insira valores entre 1 e 12"))
  if(!(ano %in% 1950:2100)) return(cat("valor inválido para ano - a partir de 1980"))
  
  if(mes < 10){
    res = paste0(ano,"0",mes,"01")
  }else{
    res = paste0(ano,mes,"01")
  }
  return(res)
}


df <- df %>% 
  rowwise() %>% 
  mutate(data = as_date( formato_ano_mes( ano,mes)))

##########################################
# PROCESSO
# 1 - ativar ano_inicio abaixo par criar o df
# sera o ano seguinte, caso queira atualizacao anual
# 2 - dentro do vetor no for colocar do segundo ano ate o fim da serie desejado

# exemplo - atualizar dados de 1985 até dez de 2023, anual
#ano_inicio = 1986
# no primeiro processo - ignorar o for e na linha 107 criar o df
# colocar dentro do vetor do for - for(ano_inicio in 1987:2023){
# comentar linha 107 para atualizar df 
##########################################


#ano_inicio = 2014

lista = list(ano_i = rep(2013:2023,each=12),
             mes_i = rep(1:12,11))

df_saida = tibble::tibble(
  inicio=NULL,
  fim = NULL,
  mes_referencia_correcao = NULL,
  ano_referencia_correcao = NULL,
  fator_acumulado_ipca = NULL
)

for(i in 1:length(lista$ano_i)){

  ano_inicio = lista$ano_i[i]
  mes_inicio = lista$mes_i[i]
  
  ano_fim = 2023
  mes_fim = 12
  
  #valor_em_sm = 3.63
  
  inicio = as_date( formato_ano_mes( ano_inicio,mes_inicio))
  fim = as_date( formato_ano_mes(ano_fim,mes_fim))
  inicio_referencia_correcao = inicio %m-% months(1)
  mes_referencia_correcao = lubridate::month(inicio_referencia_correcao)
  ano_referencia_correcao = lubridate::year(inicio_referencia_correcao)
  
  
  vetor_ipca = df %>%  # contar mes proximo
    dplyr::filter((data >= inicio & data <= fim)) %>% 
    ### IMPORTANTE ###
    ### SELECIONAR O INDICE DE PRECOS  ###
    pull(ipca)
  
  
  fa = 1
  
  for(k in vetor_ipca){
    fa = fa + (fa * (k/100))
  }
  
  (fa = round(fa,7))
  
  df_apoio = tibble(
    inicio=inicio,
    fim = fim,
    mes_referencia_correcao = mes_referencia_correcao,
    ano_referencia_correcao = ano_referencia_correcao,
    fator_acumulado_ipca = fa
  )
  
  df_saida = rbind(df_saida,df_apoio)
  
}

# valor 1 no fator acumulado para dezembro de 2023
df_saida = rbind(df_saida,tibble(
  inicio=as_date( formato_ano_mes( 2023,12)),
  fim = as_date( formato_ano_mes( 2023,12)),
  mes_referencia_correcao = 12,
  ano_referencia_correcao = 2023,
  fator_acumulado_ipca = 1
))

rm(list = setdiff(ls(),'df_saida'))

# FIM DA ETAPA 1
##########################################



##########################################
# ETAPA 2

## dados zipados e qu são 3 arquivos

zipdir <- "novo_dados/secretarias.zip"
tmp <- paste0(getwd(),'/novo_dados/')
unzip(zipdir, exdir = tmp)
dir(tmp, recursive = TRUE)

df_31 <- read_xlsx("novo_dados/secretaria_31.xlsx")
df_50 <- read_xlsx("novo_dados/secretaria_50.xlsx")
df_07 <- read_xlsx("novo_dados/secretaria_7.xlsx" )

df_31$PROVENTOS %>% head()
df_50$PROVENTOS %>% head()
df_07$PROVENTOS %>% head()

df_31 <- df_31 %>% mutate(
  PROVENTOS = as.numeric(stringr::str_replace(as.character(PROVENTOS),",","."))
)

df_50 <- df_50 %>% mutate(
  PROVENTOS = as.numeric(stringr::str_replace(as.character(PROVENTOS),",","."))
)

df_empilhado_inicial <- rbind(
  df_07,df_50,df_31)

table(
  df_empilhado_inicial$EMPRESA,
  is.na(df_empilhado_inicial$PROVENTOS)
)
# 
#       FALSE
# 031  39167
# 050 130576
# 7   232990


arrow::write_parquet(df_empilhado_inicial,
                     sink = "novo_dados/df_empilhado.parquet")

rm(list = setdiff(ls(),'df_saida'))
gc()
cat('\014')


## lar aqruivo pra correcao de valores

df_empilhado <- arrow::read_parquet("novo_dados/df_empilhado.parquet")
df_empilhado <- janitor::clean_names(df_empilhado)


# adicionar dados
df_empilhado2 = left_join(
  df_empilhado,
  df_saida %>% 
    select(ano_referencia_correcao, mes_referencia_correcao, fator_acumulado_ipca),
  by = join_by(ano_referencia == ano_referencia_correcao,
               mes_referencia == mes_referencia_correcao)
)

df_empilhado2 = df_empilhado2 %>% 
  mutate(proventos_correcao = case_when(
    !is.na(proventos) ~ proventos * fator_acumulado_ipca,
    .default = NA_real_
  ))


openxlsx::write.xlsx(df_empilhado2,
  "//srjn4/atlas/executivo_estadual/dados/df_empilhado2.xlsx",
)

# FIM DA ETAPA 2
##########################################


##########################################
# analise de dados faltantes

names(df_empilhado)

# [1] "empresa"                  "ano_referencia"           "mes_referencia"          
# [4] "versao"                   "matricula"                "unidade_administrativa"  
# [7] "desc_sexo"                "lotacao"                  "status"                  
# [10] "descricao_do_cargo"       "descricao_da_carreira"    "ref_funcional_vertical"  
# [13] "ref_funcional_horizontal" "proventos"                "grau_de_instrucao"

table(is.na(df_empilhado$proventos))
#   FALSE    TRUE 
#  232.990 169.743 

table(df_empilhado$empresa,is.na(df_empilhado$proventos))
#     FALSE   TRUE
# 7  232990      0
# 31      0  39167
# 50      0 130576

#empresa 31 e 50 tem todas as remuneracoes faltantes

table(df_empilhado$ano_referencia,is.na(df_empilhado$proventos))
#      FALSE  TRUE
# 2013 18511 17460
# 2014 17949 17890
# 2015 16871 13678
# 2016 16319 15286
# 2017 15804 13630
# 2018 15070 13534
# 2019 25908 14213
# 2020 25865 16497
# 2021 26169 16698
# 2022 27647 15708
# 2023 26877 15149

table(df_empilhado$versao,is.na(df_empilhado$proventos))
#    FALSE   TRUE
# 1 232990 169743

aa = df_empilhado %>% 
  group_by(stringr::str_sub(matricula,1,1),is_na = is.na(proventos)) %>% 
  summarise(n=n())
  
# somente matriculas que comecam com zero e um que tem faltantes

table(stringr::str_sub(df_empilhado$matricula,1,1),is.na(df_empilhado$proventos))
#    FALSE   TRUE
# 0  20348  63349
# 1  65681 106394
# 2  42915      0
# 3  41785      0
# 4  46660      0
# 5     33      0
# 6    103      0
# 7    261      0
# 8   1413      0
# 9  13791      0

aa = df_empilhado %>% 
  filter(stringr::str_sub(matricula,1,1) %in% 0:1) %>% 
  group_by(mat = stringr::str_sub(matricula,1,3),is_na = is.na(proventos)) %>% 
  summarise(n=n())

# espalhado nos subgrupos


table(df_empilhado$unidade_administrativa,is.na(df_empilhado$proventos))
#    FALSE   TRUE
# 1  219705  69552
# 2       0    727
# 3       0   1055
# 4   13285    884
# 5       0   1467
# 6       0  44410
# 7       0     79
# 8       0  33697
# 9       0   2623
# 10      0    795
# 11      0   1167
# 12      0   3163
# 13      0     92
# 14      0   3597
# 15      0   3871
# 16      0    390
# 17      0    324
# 18      0   1627
# 19      0    223








