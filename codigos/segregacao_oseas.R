### 05/04/2025 - codigos do oseas


install.packages("segregation")
library(segregation)

# Criando um dataset com servidores públicos separados por ocupação, sexo e cor
dados <- data.frame(
  ocupacao = c("Administração", "Saúde", "TI", "Educação", "Segurança",
               "Administração", "Saúde", "TI", "Educação", "Segurança"),
  grupo = c("Mulher_Branca", "Mulher_Branca", "Mulher_Branca", "Mulher_Branca", "Mulher_Branca",
            "Homem_Preto", "Homem_Preto", "Homem_Preto", "Homem_Preto", "Homem_Preto"),
  n = c(500, 600, 300, 800, 400, 300, 700, 500, 600, 900)  # Número de servidores em cada categoria
)

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



