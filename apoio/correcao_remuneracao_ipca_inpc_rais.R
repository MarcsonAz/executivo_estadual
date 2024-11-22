### correcao de valores
# deixar duas funcoes que pegam valores de salarios e taxas no banco e calculam a correcao


# são funcoes baseadas na funcao atualiza_ipca do Bruno Portes feita no banco de dados 
# do projeto do Atlas do Estado

# os testes para validação serão baseados no resultado comparado com a
# calculara do banco central de correcao de valores

# https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice

library(pacman)

p_load(dplyr,lubridate)

# conectar ao banco
source("./scripts-r/conectar.R")

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

# preciso

# ano inicio, ano fim, mes inicio, mes fim, valor para corrigir


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


#ano_inicio = 1986

for(ano_inicio in 1987:2023){
mes_inicio = 1

ano_fim = 2023
mes_fim = 12

#valor_em_sm = 3.63

inicio = as_date( formato_ano_mes( ano_inicio,mes_inicio))
fim = as_date( formato_ano_mes(ano_fim,mes_fim))


vetor_inpc = df %>%  # contar mes proximo
  dplyr::filter((data >= inicio & data <= fim)) %>% 
  ### IMPORTANTE ###
  ### SELECIONAR O INDICE DE PRECOS  ###
  pull(inpc)


fa = 1

for(i in vetor_inpc){
  fa = fa + (fa * (i/100))
}

(fa = round(fa,7))


df_apoio = tibble(
  ano_inicio = ano_inicio,
  mes_inicio = mes_inicio,
  ano_fim = ano_fim,
  mes_fim = mes_fim,
  ano_referencia = ano_inicio-1,
  fator_acumulado = fa
)

# no primeiro caso
#df_saida = df_apoio
# nos casos seguintes
df_saida = rbind(df_saida,df_apoio)

}
# exportar
openxlsx2::write_xlsx(df_saida,'./scripts-r/teste_rais_2022/data/inpc_acumulado_anual_ref_202312.xlsx')

# sm em 2021 foi de 1100 reais
#valor_em_sm * 1100 * fa

# criar tabela com dados de sm corrigido para remuneracao


# consultas de dados de apoio

qq <- paste("SELECT ano, mes, salario_minimo_em_real_ipeadata, salario_minimo_em_real_calculado, 
moeda, fator_de_conversao_para_o_real, ipca, inpc, salario_na_moeda_do_ano, obs
FROM site_2022.gabarito_correcao_inflacao;")

df_sm <- DBI::dbGetQuery(con, qq)


df_sm <- df_sm %>% 
  group_by(ano) %>% 
  summarise(media_sm = mean(salario_minimo_em_real_ipeadata)) %>% 
  ungroup() %>% 
  left_join(
    df_saida, by = join_by(ano == ano_referencia)
  ) %>% 
  rowwise() %>% 
  mutate(
    sm_valor_presente_202312 = media_sm * fator_acumulado
  ) %>% 
  select(ano,sm_valor_presente_202312) %>% 
  na.omit() %>% 
  tibble::tibble()


# criar tabela no banco

DBI::dbCreateTable(con,DBI::Id(schema = "site_2022", table = "gabarito_sm_202312"),df_sm)
DBI::dbAppendTable(con,DBI::Id(schema = "site_2022", table = "gabarito_sm_202312"),df_sm)








