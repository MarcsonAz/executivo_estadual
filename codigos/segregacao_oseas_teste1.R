### 05/04/2025 - codigos do oseas com dados do Atlas

### segregacao_oseas_teste1


## trazer dados do banco de 2021


#install.packages("segregation")
library(segregation)
library(dplyr)
library(DBI)

source('../atlas_estado/ATLAS_2024/conectar.R')

query1 <- paste(
"select v.ano, substring(v.cbo2002,1,4) as cbo_familia, 
      fam.titulo, v.genero,cor.raca_script_r_resultado,
      count(v.*) as total_ee
from vinculos_v6.tb_vinculos v
join rfb.tb_ipea_rfb_publicos rfb on rfb.cnpj_texto = v.id_estab
left join raca_cor.raca_cor_adm_publica_rfb_parte_1 cor on cor.cpf = v.cpf
left join cbo.\"04_familia\" fam on fam.codigo = substring(v.cbo2002,1,4)
where v.ano in (2004,2010,2015,2021) and rfb.poder = 'E' and rfb.esfera = 'E' and cor.raca_script_r_resultado is not null
group by v.ano, substring(v.cbo2002,1,4), 
      fam.titulo, v.genero,cor.raca_script_r_resultado")

df1 <- DBI::dbGetQuery(con, query1)

# sexo
query2 <- paste(
  "select v.ano, substring(v.cbo2002,1,1) as cbo_gg, gg.titulo, v.genero,
count(v.*) as total_ee
from vinculos_v6.tb_vinculos_2021 v
join rfb.tb_ipea_rfb_publicos rfb on rfb.cnpj_texto = v.id_estab
left join cbo.\"01_grande_grupo\" gg on gg.codigo = substring(v.cbo2002,1,1)
where rfb.poder = 'E' and rfb.esfera = 'E'
group by v.ano, substring(v.cbo2002,1,1), gg.titulo,v.genero")

df2 <- DBI::dbGetQuery(con, query2)

# trabalho do oséas
query3 <- paste(
  "select v.ano, v.cbo_base, v.sexo, v.cor_raca,
count(v.*) as total_ee
from vinculos_v6_auxiliar.v_publico v
where v.ano = 2021 and v.poder = 'Executivo' and v.nivel_federativo in ('Estadual','Municipal')
group by v.ano, v.cbo_base, v.sexo, v.cor_raca")

df3 <- DBI::dbGetQuery(con, query3)
# colunas: ocupacao - grupo (sexo_cor) - n

##  TRATAMENTO


dados <- df1 %>% 
  na.omit()

dados$cbo_gg <- as.factor(dados$cbo_gg)
dados$total_ee <- as.numeric(dados$total_ee)

# VALOR ZERO NÃO FOI CALCULADO - INSERIR

dados <- rbind(dados,data.frame(
  ano = 2021L, cbo_gg = '9',
  titulo = 'TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO',
  genero = 2L, raca_script_r_resultado = 1,total_ee = 0
))

################################ OCUPACAO #####################################

dados <- dados %>% 
  select(cbo_gg, total_ee) %>% 
  group_by(cbo_gg) %>% 
  summarise(total_ee = sum(total_ee)) %>% 
  ungroup()
  
  
# Calcular a proporção de cada grupo na população total
dados$total_pop <- sum(dados$total_ee)
dados$P_j <- dados$total_ee / dados$total_pop

# Calcular a entropia total
entropia_total <- -sum(dados$P_j * log2(dados$P_j))

# Mostrar resultado
print(paste("Entropia Total da População:", round(entropia_total, 4)))


# Função para calcular entropia por ocupação
calcular_entropia <- function(df) {
  P_ij <- df$total_ee / sum(df$total_ee)
  entropia <- -sum(P_ij * log2(P_ij), na.rm = TRUE)
  return(entropia)
}

# Aplicar a função para cada ocupação
entropia_por_ocupacao <- aggregate(total_ee ~ cbo_gg, data = dados, FUN = sum)
entropia_por_ocupacao$E_i <- sapply(split(dados, dados$cbo_gg), calcular_entropia)

# Calcular a média ponderada da entropia dentro das ocupações
entropia_media_ocupacoes <- sum((entropia_por_ocupacao$total_ee / sum(entropia_por_ocupacao$total_ee))
                                * entropia_por_ocupacao$E_i)

# Mostrar resultado
print(paste("Entropia Média Dentro das Ocupações (E_W):", round(entropia_media_ocupacoes, 4)))
# Calcular H
H <- (entropia_total - entropia_media_ocupacoes) / entropia_total

# Mostrar resultado
print(paste("Índice de Informação Mútua (H):", round(H, 4)))

# Normalização do índice de Theil
H_prime <- H / log2(nrow(entropia_por_ocupacao))


# Mostrar resultado
print(paste("Índice de Theil (H'):", round(H_prime, 4)))

## Após rodar o código, você terá:

# Entropia total da população (ETE_TET) → Mede a diversidade total.
# Entropia média dentro das ocupações (EWE_WEW) → Mede a diversidade dentro de cada ocupação.
# Índice de Informação Mútua (H) → Mede a diferença entre ETE_TET e EWE_WEW.
# Índice de Theil (H') → Versão normalizada de HHH.
# Se HHH e H′H'H′ forem altos, há alta segregação. Se forem baixos, há baixa segregação


################################ SEXO #####################################

dados <- dados %>% 
  select(cbo_gg, genero, total_ee) %>% 
  group_by(cbo_gg, genero) %>% 
  summarise(total_ee = sum(total_ee)) %>% 
  ungroup()


# Calcular a proporção de cada grupo na população total
dados$total_pop <- sum(dados$total_ee)
dados$P_j <- dados$total_ee / dados$total_pop

# Calcular a entropia total
entropia_total <- -sum(dados$P_j * log2(dados$P_j))

# Mostrar resultado
print(paste("Entropia Total da População:", round(entropia_total, 4)))


# Função para calcular entropia por ocupação
calcular_entropia <- function(df) {
  P_ij <- df$total_ee / sum(df$total_ee)
  entropia <- -sum(P_ij * log2(P_ij), na.rm = TRUE)
  return(entropia)
}

# Aplicar a função para cada sexo
entropia_por_sexo <- aggregate(total_ee ~ genero, data = dados, FUN = sum)
entropia_por_sexo$E_i <- sapply(split(dados, dados$genero), calcular_entropia)

# Calcular a média ponderada da entropia dentro das ocupações
entropia_media_sexos <- sum((entropia_por_sexo$total_ee / sum(entropia_por_sexo$total_ee))
                                * entropia_por_sexo$E_i)

# Mostrar resultado
print(paste("Entropia Média Dentro das categorias de sexo (E_W):", round(entropia_media_sexos, 4)))
# Calcular H
H <- (entropia_total - entropia_media_sexos) / entropia_total

# Mostrar resultado
print(paste("Índice de Informação Mútua (H):", round(H, 4)))

# Normalização do índice de Theil
H_prime <- H / log2(nrow(entropia_por_sexo))


# Mostrar resultado
print(paste("Índice de Theil (H'):", round(H_prime, 4)))

## Após rodar o código, você terá:

# Entropia total da população (ETE_TET) → Mede a diversidade total.
# Entropia média dentro das ocupações (EWE_WEW) → Mede a diversidade dentro de cada ocupação.
# Índice de Informação Mútua (H) → Mede a diferença entre ETE_TET e EWE_WEW.
# Índice de Theil (H') → Versão normalizada de HHH.
# Se HHH e H′H'H′ forem altos, há alta segregação. Se forem baixos, há baixa segregação


################################ SEXO E COR ####################################

dados <- dados %>% 
  select(genero,raca_script_r_resultado,total_ee) %>% 
  mutate(sexo_cor = case_when(
    genero == 1 & raca_script_r_resultado %in% c(4,8) ~ 'homem_preta_parda',
    genero == 1 & raca_script_r_resultado == 2 ~ 'homem_branca',
    genero == 2 & raca_script_r_resultado %in% c(4,8) ~ 'mulher_preta_parda',
    genero == 2 & raca_script_r_resultado == 2 ~ 'mulher_branca',
    .default = NA_character_
  )) %>% 
  select(sexo_cor,total_ee) %>% 
  mutate(sexo_cor = as.factor(sexo_cor)) %>% 
  group_by(sexo_cor) %>% 
  summarise(total_ee = sum(total_ee)) %>% 
  ungroup() %>% 
  na.omit()

# Calcular a proporção de cada grupo na população total
dados$total_pop <- sum(dados$total_ee)
dados$P_j <- dados$total_ee / dados$total_pop

# Calcular a entropia total
entropia_total <- -sum(dados$P_j * log2(dados$P_j))

# Mostrar resultado
print(paste("Entropia Total da População:", round(entropia_total, 4)))


# Função para calcular entropia por sexo cor
calcular_entropia <- function(df) {
  P_ij <- df$total_ee / sum(df$total_ee)
  entropia <- -sum(P_ij * log2(P_ij), na.rm = TRUE)
  return(entropia)
}

# Aplicar a função para cada sexo cor
entropia_por_sexo_cor <- aggregate(total_ee ~ sexo_cor, data = dados, FUN = sum)
entropia_por_sexo_cor$E_i <- sapply(split(dados, dados$sexo_cor), calcular_entropia)

# Calcular a média ponderada da entropia dentro das categorias de sexo cor
entropia_media_sexo_cor <- sum((entropia_por_sexo_cor$total_ee / sum(entropia_por_sexo_cor$total_ee))
                                * entropia_por_sexo_cor$E_i)

# Mostrar resultado
print(paste("Entropia Média Dentro das categorias de sexo cor (E_W):", round(entropia_media_sexo_cor, 4)))
# Calcular H
H <- (entropia_total - entropia_media_sexo_cor) / entropia_total

# Mostrar resultado
print(paste("Índice de Informação Mútua (H):", round(H, 4)))

# Normalização do índice de Theil
H_prime <- H / log2(nrow(entropia_por_sexo_cor))


# Mostrar resultado
print(paste("Índice de Theil (H'):", round(H_prime, 4)))



################################ pacote #####################################
################################ pacote #####################################
################################ pacote #####################################


df_pacote <- dados %>% mutate(
  cbo_gg = as.factor(cbo_gg),
  genero = as.factor(as.character(genero)),
  cor = as.factor(as.character(raca_script_r_resultado)),
  n=as.numeric(total_ee)) %>% 
  mutate(sexo_cor = case_when(
    genero == 1 & cor %in% c(4,8) ~ 'homem_preta_parda',
    genero == 1 & cor == 2 ~ 'homem_branca',
    genero == 2 & cor %in% c(4,8) ~ 'mulher_preta_parda',
    genero == 2 & cor == 2 ~ 'mulher_branca',
    .default = NA_character_
  ))


# sexo
entropy(df_pacote, 'genero', weight = 'n')


# cor
entropy(df_pacote, 'cor', weight = 'n')

# sexo_cor
entropy(df_pacote %>% filter(!is.na(sexo_cor)), 'sexo_cor', weight = 'n')

# ocupacao
entropy(df_pacote, 'cbo_gg', weight = 'n')

# informacao

# ocupacao sexo
mutual_total(data = df_pacote, group = 'cbo_gg', unit = 'genero', weight = 'n')

# ocupacao cor
mutual_total(data = df_pacote, group = 'cbo_gg', unit = 'cor', weight = 'n')

# ocupacao sexo_cor
mutual_total(data = df_pacote %>% filter(!is.na(sexo_cor)),
             group = 'cbo_gg', unit = 'cor', weight = 'n')


mutual_total(data = dados, group = 'cbo_gg', unit = 'genero', weight = 'total_ee')



################################ pacote ######################################
########################## TESTE COM TEXTO DO OSEAS ##########################

dados <- df3 %>% 
  na.omit() %>%
  mutate(sexo_cor = case_when(
    sexo == 1 & cor_raca %in% c(4,8) ~ 'homem_preta_parda',
    sexo == 1 & cor_raca == 2 ~ 'homem_branca',
    sexo == 2 & cor_raca %in% c(4,8) ~ 'mulher_preta_parda',
    sexo == 2 & cor_raca == 2 ~ 'mulher_branca',
    sexo %in% c(1,2) & cor_raca %in% c(1,6) ~ NA_character_,
    .default = NA_character_
  )) %>% 
  filter(!is.na(sexo_cor)) %>% 
  select(cbo_base,sexo_cor,total_ee) %>% 
  group_by(cbo_base,sexo_cor) %>% 
  summarise(n=sum(total_ee)) %>% 
  ungroup() %>% 
  mutate(n=as.numeric(n))

# ocupacao sexo_cor
z = mutual_total(data = dados, group = 'cbo_base', unit = 'sexo_cor', weight = 'n')




# dois grupos


dados <- df3 %>% 
  na.omit() %>%
  mutate(sexo_cor = case_when(
    sexo == 1 & cor_raca %in% c(4,8) ~ 'homem_preta_parda',
    sexo == 1 & cor_raca == 2 ~ 'homem_branca',
    sexo == 2 & cor_raca %in% c(4,8) ~ 'mulher_preta_parda',
    sexo == 2 & cor_raca == 2 ~ 'mulher_branca',
    sexo %in% c(1,2) & cor_raca %in% c(1,6) ~ NA_character_,
    .default = NA_character_
  )) %>% 
  filter(!is.na(sexo_cor)) %>% 
  select(cbo_base,sexo_cor,total_ee) %>% 
  group_by(cbo_base,sexo_cor) %>% 
  summarise(n=sum(total_ee)) %>% 
  ungroup() %>% 
  mutate(n=as.numeric(n))








dissimilarity(data = dados, group = 'cbo_base', unit = 'sexo_cor', weight = 'n')

m1 <- matrix_to_long(matrix(c(100, 60, 40, 0, 0, 40, 60, 100), ncol = 2))
m2 <- matrix_to_long(matrix(c(80, 80, 20, 20, 20, 20, 80, 80), ncol = 2)) 
dissimilarity(m1, "group", "unit", weight = "n") 
dissimilarity(m2, "group", "unit", weight = "n")





### 
#pegar df3 
 # selecionar cbo, sexo, total_pop
# remover na
 #agrupar por cbo, sexo  e somar total

# entropia por cbo, entropia por sexo

 











