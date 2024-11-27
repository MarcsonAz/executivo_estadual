## projeto dissertação Daniel Alvarenga ##

library(dplyr)
library(readxl)

#empilhando o arquivo de três diferentes secretarias (2013-2023)
# Carregar os três arquivos CSV
df1 <- read_excel("data/secretaria_7.xlsx")
df2 <- read_excel("data/secretaria_31.xlsx")
df3 <- read_excel("data/secretaria_50.xlsx")

# Obtenha os nomes das colunas de cada data frame
cols_df1 <- colnames(df1)
cols_df2 <- colnames(df2)
cols_df3 <- colnames(df3)

# Verifique se todos os nomes de colunas são iguais
identical(cols_df1, cols_df2) && identical(cols_df2, cols_df3)

#vendo o tipo de variavel
str(df1$PROVENTOS)
str(df2$PROVENTOS)
str(df3$PROVENTOS)


#harmonizando a variável
#df1$EMPRESA <- as.character(df1$EMPRESA)
df2$PROVENTOS <- as.numeric(df2$PROVENTOS)
df3$PROVENTOS <- as.numeric(df3$PROVENTOS)

df2$VERSAO <- as.numeric(df2$VERSAO)
df3$VERSAO <- as.numeric(df3$VERSAO)



# Empilhar os arquivos em um único data frame
df_empilhado <- bind_rows(df1, df2, df3)

# Verificar a estrutura do data frame combinado
head(df_empilhado)

library(janitor)
df_empilhado <- clean_names(df_empilhado)
names(df_empilhado)

write.csv(df_empilhado, "data/df_empilhado.csv", row.names = FALSE)

glimpse(df_empilhado)
unique(df_empilhado$descricao_da_carreira)

# calculos iniciais
media_proventos_por_ano <- df_empilhado %>%
        filter(mes_referencia == 2) %>% 
        group_by(ano_referencia) %>%
        summarise(media_proventos = mean(proventos, na.rm = TRUE))

media_proventos_por_ano


#calculando a remuneração média por carreira em 2023

media_carreira_2023 <- df_empilhado %>%
        filter(ano_referencia == 2023, mes_referencia == 2) %>%                       # Filtrar ano e mês
        group_by(descricao_da_carreira) %>%                       # Agrupar por descricao_carreira
        summarise(media_proventos = mean(proventos, na.rm = TRUE)) # Calcular a média

media_carreira_2023

#calcular media da remuneração por carreira e sexo

media_carreira_sexo_2023 <- df_empilhado %>%
        filter(ano_referencia == 2023, mes_referencia == 2) %>%
        group_by(descricao_da_carreira, desc_sexo) %>%
        summarise(media_proventos = mean(proventos, na.rm = TRUE))

media_carreira_sexo_2023

names(df_empilhado)


##calculando o GINI em diferentes anos

install.packages("ineq")
library(ineq)

# Filtrar os dados para o ano de 2023
gini_2023 <- df_empilhado %>%
        filter(ano_referencia == 2013, mes_referencia == 2) %>%                                  # Filtrar para 2023
        select(proventos) %>%                                    # Selecionar coluna de proventos
        filter(!is.na(proventos)) 

# Calcular o Índice de Gini
gini_2023_resultado <- Gini(gini_2023$proventos)

# Exibir o resultado
print(paste("Índice de Gini para o ano de 2023:", round(gini_2023_resultado, 4)))


#grafico de gini

# Criar um dataframe para o índice de Gini
gini_df <- data.frame(
        ano = 2023,
        gini = gini_2023_resultado # Use o valor calculado do Gini
)

# Criar o gráfico de barras
ggplot(gini_df, aes(x = factor(ano), y = gini)) +
        geom_col(fill = "steelblue") +
        geom_text(aes(label = round(gini, 4)), vjust = -0.5, size = 5) +
        labs(title = "Índice de Gini para o Ano de 2023",
             x = "Ano",
             y = "Índice de Gini") +
        theme_minimal()


#calcular e gerar o lorenz
# Filtrar os dados para o ano e mês desejados, removendo NAs
lorenz_data <- df_empilhado %>%
        filter(ano_referencia == 2013, mes_referencia == 2) %>%
        filter(!is.na(proventos)) %>%   # Remover valores NA de proventos
        pull(proventos)                # Extrair a coluna "proventos" como vetor numérico

# Visualizar a curva de Lorenz
plot(lorenz_result,
     main = "Curva de Lorenz para Fevereiro de 2023",
     xlab = "Proporção da População",
     ylab = "Proporção da Renda Acumulada",
     col = "blue",
     lwd = 2)

# Adicionar linha de igualdade perfeita
abline(0, 1, col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c("Curva de Lorenz", "Igualdade Perfeita"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)