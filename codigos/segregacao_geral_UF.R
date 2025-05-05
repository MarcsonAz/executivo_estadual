## novo

# medidads do oseas com os nossos dados

dados <- df4

ANO = 2021

{
dados <- dados %>% 
  filter(ano == ANO) %>% 
  select(
    ocupacao = cbo_familia,
    grupo = sexo_descricao,
    n = total_vinculos
  ) %>% 
  select(ocupacao,grupo,n)


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

# fazer dataframe para armazenar informacao


resultados_ano <- data.frame(
  ano = ANO,
  entropia_total = entropia_total,
  entropia_media_ocupacoes = entropia_media_ocupacoes,
  informacao_mutua = H,
  theil = H_prime
)

resultados <- rbind(resultados,resultados_ano)


# resultados inicial 

resultados <- data.frame(
  ano = ANO,
  entropia_total = entropia_total,
  entropia_media_ocupacoes = entropia_media_ocupacoes,
  informacao_mutua = H,
  theil = H_prime
)



## teste do D


df = dados

library(tidyr)

df = df %>% 
  select(ocupacao,grupo,P_j) %>% 
  pivot_wider(names_from = grupo, values_from = P_j) %>% 
  mutate(diff = abs(Homem-Mulher))

(D = 1/2 * sum(df$diff))





