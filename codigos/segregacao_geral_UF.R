## novo

# medidads do oseas com os nossos dados

dados <- df3;ANO = 1985

{
dados <- dados %>%
    na.omit() %>% 
    filter(ano == ANO) %>% 
    group_by(ano, grupo = sexo_descricao,ocupacao = cbo_familia) %>% 
    summarise(n = sum(total_vinculos)) %>% 
    ungroup()


## verificar se tem homem e mulher em cada familia

dados_completo <- data.frame(
  ocupacao = rep(unique(dados$ocupacao),each=2),
  grupo = rep_len(unique(dados$grupo),length.out = 2*length(unique(dados$ocupacao)))
)

dados <- dados_completo %>% left_join(dados) %>% 
  mutate(familia_vazia = ifelse(is.na(n),ocupacao,NA_character_))

familias_ocupacoes_remover <- na.omit(dados$familia_vazia)

dados <- dados %>% 
  filter(!(ocupacao %in% familias_ocupacoes_remover)) %>% 
  select(ocupacao,grupo,n)

}
{
  
# Calcular a proporção de cada grupo na população total
dados$total_pop <- sum(dados$n)
dados$P_j <- dados$n / dados$total_pop

# Calcular a entropia total
entropia_total <- -sum(dados$P_j * log2(dados$P_j))

# Mostrar resultado
print(paste("Entropia Total da População:", round(entropia_total, 4)))


# Função para calcular entropia por ocupação
calcular_entropia <- function(df) {
  P_ij <- df$n / sum(df$n)
  entropia <- -sum(P_ij * log2(P_ij), na.rm = TRUE)
  return(entropia)
}

# Aplicar a função para cada ocupação
entropia_por_ocupacao <- aggregate(n ~ ocupacao, data = dados, FUN = sum)
entropia_por_ocupacao$E_i <- sapply(split(dados, dados$ocupacao), calcular_entropia)

# Calcular a média ponderada da entropia dentro das ocupações
entropia_media_ocupacoes <- sum((entropia_por_ocupacao$n / sum(entropia_por_ocupacao$n)) * entropia_por_ocupacao$E_i)

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


# Para 2021
# "Entropia Total da População: 2.6583"
# "Entropia Média Dentro das Ocupações (E_W): 0.3705"
# "Índice de Informação Mútua (H): 0.8606"
# "Índice de Theil (H'): 0.2203"


# dissimilaridade

df = dados

#library(tidyr)

df = df %>% 
  select(ocupacao,grupo,P_j) %>% 
  pivot_wider(names_from = grupo, values_from = P_j) %>% 
  mutate(diff = abs(Homem-Mulher))

(D = 1/2 * sum(df$diff))


# d2
df = dados
df = df %>% 
  select(ocupacao,grupo,n) %>% 
  group_by(grupo) %>% 
  mutate(total_sexo = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop_ocup = n/total_sexo) %>% 
  select(ocupacao,grupo,prop_ocup) %>%
  pivot_wider(names_from = grupo, values_from = prop_ocup) %>% 
  mutate(diff = abs(Mulher-Homem))

(D2 = 1/2 * sum(df$diff))

# km no texto do hoffman
df = dados
a_de_km = df %>% 
  group_by(grupo) %>% 
  summarise(total_sexo = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = total_sexo/sum(total_sexo)) %>% 
  filter(grupo == "Homem") %>% 
  select(prop) %>% 
  pull()

(KM = 2*a_de_km*(1-a_de_km)*D2)

}
# fazer dataframe para armazenar informacao


resultados_ano <- data.frame(
  ano = ANO,
  entropia_total = entropia_total,
  entropia_media_ocupacoes = entropia_media_ocupacoes,
  informacao_mutua = H,
  theil = H_prime,
  diss = D,
  diss_hoff = D2,
  km_hoff = KM
)

resultados <- rbind(resultados,resultados_ano)


# resultados inicial 

resultados <- data.frame(
  ano = ANO,
  entropia_total = entropia_total,
  entropia_media_ocupacoes = entropia_media_ocupacoes,
  informacao_mutua = H,
  theil = H_prime,
  diss = D,
  diss_hoff = D2,
  km_hoff = KM
)



## teste do D



df = dados

library(tidyr)

df = df %>% 
  select(ocupacao,grupo,P_j) %>% 
  pivot_wider(names_from = grupo, values_from = P_j) %>% 
  mutate(diff = abs(Homem-Mulher))

(D = 1/2 * sum(df$diff))



df3 %>% tibble() %>% 
  group_by(ano,sexo_descricao) %>% 
  summarise(t = sum(total_vinculos)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sexo_descricao, values_from = t) %>%
  mutate(prop_mulher = (Mulher/(Homem+Mulher)))
  

# teste para comparra com o pacote



dados <- df3;ANO = 2021

{  dados <- dados %>%
    na.omit() %>% 
    filter(ano == ANO) %>% 
    group_by(ano, grupo = sexo_descricao,ocupacao = cbo_familia) %>% 
    summarise(n = sum(total_vinculos)) %>% 
    ungroup()
  
  
  ## verificar se tem homem e mulher em cada familia
  
  dados_completo <- data.frame(
    ocupacao = rep(unique(dados$ocupacao),each=2),
    grupo = rep_len(unique(dados$grupo),length.out = 2*length(unique(dados$ocupacao)))
  )
  
  dados <- dados_completo %>% left_join(dados) %>% 
    mutate(familia_vazia = ifelse(is.na(n),ocupacao,NA_character_))
  
  familias_ocupacoes_remover <- na.omit(dados$familia_vazia)
  
  dados <- dados %>% 
    filter(!(ocupacao %in% familias_ocupacoes_remover)) %>% 
    select(ocupacao,grupo,n)
  
  
  
  # Calcular a proporção de cada grupo na população total
  dados$total_pop <- sum(dados$n)
  dados$P_j <- dados$n / dados$total_pop
  
  # Calcular a entropia total
  entropia_total <- -sum(dados$P_j * log2(dados$P_j))
  
  # Mostrar resultado
  print(paste("Entropia Total da População:", round(entropia_total, 4)))
  
  
  # Função para calcular entropia por ocupação
  calcular_entropia <- function(df) {
    P_ij <- df$n / sum(df$n)
    entropia <- -sum(P_ij * log2(P_ij), na.rm = TRUE)
    return(entropia)
  }
  
  # Aplicar a função para cada ocupação
  entropia_por_ocupacao <- aggregate(n ~ ocupacao, data = dados, FUN = sum)
  entropia_por_ocupacao$E_i <- sapply(split(dados, dados$ocupacao), calcular_entropia)
  
  # Calcular a média ponderada da entropia dentro das ocupações
  entropia_media_ocupacoes <- sum((entropia_por_ocupacao$n / sum(entropia_por_ocupacao$n)) * entropia_por_ocupacao$E_i)
  
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

}


# seguindo o texto do b2njamim elbhers
# A Method for Studying Differences in Segregation Across Time and Space
# https://paa2019.populationassociation.org/uploads/190592



dados <- df3;ANO = 2021

### preparacao de dados
{
  dados <- dados %>%
    na.omit() %>% 
    filter(ano == ANO) %>% 
    group_by(ano, grupo = sexo_descricao,ocupacao = cbo_familia) %>% 
    summarise(n = sum(total_vinculos)) %>% 
    ungroup()
  
  
  ## verificar se tem homem e mulher em cada familia
  
  dados_completo <- data.frame(
    ocupacao = rep(unique(dados$ocupacao),each=2),
    grupo = rep_len(unique(dados$grupo),length.out = 2*length(unique(dados$ocupacao)))
  )
  
  dados <- dados_completo %>% left_join(dados) %>% 
    mutate(familia_vazia = ifelse(is.na(n),ocupacao,NA_character_))
  
  familias_ocupacoes_remover <- na.omit(dados$familia_vazia)
  
  dados <- dados %>% 
    filter(!(ocupacao %in% familias_ocupacoes_remover)) %>% 
    select(ocupacao,grupo,n)
  
}


#library(segregation)


mut = mutual_total(data = dados,
             group = "grupo",
             unit = "ocupacao",
             weight = "n"
             )

diss = dissimilarity(data = dados,
              group = "grupo",
              unit = "ocupacao",
              weight = "n")


# fazer dataframe para armazenar informacao


resultados_ano <- data.frame(
  ano = ANO,
  m_ = mut$est[1],
  h_ = mut$est[2],
  d_ = diss$est[1]
)

resultados <- rbind(resultados,resultados_ano)


# resultados inicial 

resultados <- data.frame(
  ano = ANO,
  m_ = mut$est[1],
  h_ = mut$est[2],
  d_ = diss$est[1]
)


mutual_local(data = dados,
              group = "grupo",
              unit = "ocupacao",
              weight = "n",
              wide = TRUE
)












# aplicando calculos


dados2 = dados %>% 
  group_by(ocupacao) %>% 
  mutate(total_ocupacao = sum(n)) %>% 
  ungroup() %>% 
  group_by(grupo) %>% 
  mutate(total_grupo = sum(n)) %>% 
  ungroup() %>% 
  mutate(total = sum(n),
         prop_ocupacao = total_ocupacao/total,
         prop_grupo = total_grupo/total,
         prop_grupo_ocupacao = n/total_ocupacao,
         parcela_soma_interna = prop_grupo_ocupacao * logb(prop_grupo_ocupacao/prop_grupo)) %>% 
  group_by(ocupacao) %>% 
  mutate(soma_interna = sum(parcela_soma_interna)) %>% 
  ungroup() %>% 
  mutate(parcela_m = prop_ocupacao * soma_interna)

(M = sum(dados2$parcela_m))

(H = M / log2(length(unique(dados2$ocupacao))))





















