## novo

# medidads do oseas com os nossos dados

dados <- df13;ANO = 2021

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
    entropia <- -sum(P_ij * log(P_ij), na.rm = TRUE)
    return(entropia)
  }
  
  # Aplicar a função para cada ocupação
  entropia_por_ocupacao <- aggregate(n ~ unidade, data = dados, FUN = sum)
  entropia_por_ocupacao$E_i <- sapply(split(dados, dados$unidade), calcular_entropia)
  
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



dados <- df3;ANO = 1985

### preparacao de dados
{
  dados <- dados %>%
    na.omit() %>% 
    filter(ano == ANO) %>% 
    group_by(ano, grupo = sexo_descricao,ocupacao = cbo2002) %>% 
    summarise(n = sum(total_vinculos_publicos)) %>% 
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




################################################################
################################################################
################################################################
# DADOS CALCULADOS PARA O BAPI

# id e imn

# dados do codigo - consultas_segregacao.R
# df14 = arrow::read_parquet('./dados/consulta_segregacao_familia_sexo_cor_16072025.parquet')

library(segregation)
dados <- df14;ANO <- 2021; VARIAVEL = 'COR_SEXO'

### preparacao de dados
{
  if(VARIAVEL == 'SEXO'){
    dados <- dados %>%
      filter(ano == ANO & !(is.na(sexo_descricao) | is.na(cbo_familia))) %>% 
      group_by(ano, grupo = sexo_descricao,ocupacao = cbo_familia) %>% 
      summarise(n = sum(total_vinculos)) %>% 
      ungroup()
    qtd_categorias = dados$grupo %>% unique() %>% length()
  }
  
  if(ANO > 2004 & VARIAVEL == 'COR'){
    dados <- dados %>%
      filter(ano == ANO & !(is.na(cor_raca_descricao) | is.na(cbo_familia))) %>% 
      mutate(cor_raca_descricao_cat = case_when(
        cor_raca_descricao == 'Branca' ~ 'BR',
        cor_raca_descricao %in% c('Preta','Parda') ~ 'PP',
        .default = NA_character_
      )) %>% 
      select(ano,cor_raca_descricao_cat,cbo_familia,total_vinculos) %>% 
      na.omit() %>% 
      group_by(ano, grupo = cor_raca_descricao_cat,ocupacao = cbo_familia) %>% 
      summarise(n = sum(total_vinculos)) %>% 
      ungroup()
    qtd_categorias = dados$grupo %>% unique() %>% length()
  }
  
  if(ANO > 2004 & VARIAVEL == 'COR_SEXO'){
    dados <- dados %>%
      filter(ano == ANO & !(is.na(cor_raca_descricao) | is.na(cor_raca_descricao) | is.na(cbo_familia))) %>% 
      mutate(cor_sexo_cat = case_when(
        sexo_descricao == "Homem" & cor_raca_descricao == 'Branca' ~ 'H_BR',
        sexo_descricao == "Homem" & cor_raca_descricao %in% c('Preta','Parda') ~ 'H_PP',
        sexo_descricao == "Mulher" & cor_raca_descricao == 'Branca' ~ 'M_BR',
        sexo_descricao == "Mulher" & cor_raca_descricao %in% c('Preta','Parda') ~ 'M_PP',
        .default = NA_character_
      )) %>% 
      select(ano,cor_sexo_cat,cbo_familia,total_vinculos) %>% 
      na.omit() %>% 
      group_by(ano, grupo = cor_sexo_cat,ocupacao = cbo_familia) %>% 
      summarise(n = sum(total_vinculos)) %>% 
      ungroup()
    qtd_categorias = dados$grupo %>% unique() %>% length()
  }
  
  ## verificar se tem valores de todas as categorias em cada família de ocupações
  # por exemplo, se tem homem e mulher em cada familia
  
  dados_completo <- data.frame(
    ocupacao = rep(unique(dados$ocupacao),each=qtd_categorias),
    grupo = rep_len(unique(dados$grupo),length.out = qtd_categorias*length(unique(dados$ocupacao)))
  )
  
  dados <- dados_completo %>% left_join(dados) %>% 
    mutate(familia_vazia = ifelse(is.na(n),ocupacao,NA_character_))
  
  familias_ocupacoes_remover <- na.omit(dados$familia_vazia)
  
  dados <- dados %>% 
    filter(!(ocupacao %in% familias_ocupacoes_remover)) %>% 
    select(ocupacao,grupo,n)
  
}

mut = mutual_total(data = dados,
                   group = "grupo",
                   unit = "ocupacao",
                   weight = "n")

if(VARIAVEL != 'COR_SEXO'){
  diss = dissimilarity(data = dados,
                     group = "grupo",
                     unit = "ocupacao",
                     weight = "n")
}else{
  diss$est[1] = NA_real_
}
# resultados inicial  # PRIMEIRA VEZ

# resultados <- data.frame(
#   ano = ANO,
#   variavel = VARIAVEL,
#   m_ = mut$est[1],
#   h_ = mut$est[2],
#   d_ = diss$est[1]
# )

# fazer dataframe para armazenar informacao


resultados_ano <- data.frame(
  ano = ANO,
  variavel = VARIAVEL,
  m_ = mut$est[1],
  h_ = mut$est[2],
  d_ = diss$est[1]
)

resultados <- rbind(resultados,resultados_ano)

openxlsx2::write_xlsx(resultados,'./dados/resultado_segregacao_16072025.xlsx')























################ ANTIGO
###############

###############
###############


###############
###############


###############
###############

###############
###############





# aplicando calculos


dados2 = dados %>% 
  group_by(unidade) %>% 
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
  group_by(unidade) %>% 
  mutate(soma_interna = sum(parcela_soma_interna)) %>% 
  ungroup() %>% 
  mutate(parcela_m = prop_ocupacao * soma_interna)

(M = sum(dados2$parcela_m))

(H = M / log2(length(unique(dados2$unidade))))



entropy()

sum()




#########################################################
#########################################################
#########################################################
#########################################################

# dados 
df13 <- arrow::read_parquet('./dados/consulta_segregacao_2021_uf_ocupacao_sexo_cor_06052025.parquet')

query15 <- 
  "select codigo, titulo
  from cbo.\"05_ocupacao\""
cbo <- DBI::dbGetQuery(con, query15)

dados <- df13;ANO = 2021



### preparacao de dados

# fazer por sexo geral - por sexo para cada UF 

{
  
  # adicionar titulo da ocupacao
  dados <- dados %>% left_join(cbo,by = join_by(cbo2002 == codigo))
  
  # remover ocupacoes com baixa variabilidade e quantidade
  dados2 <- dados %>% filter(!(uf_ipea %in% c(11,23,51,16,21) & total_vinculos_publicos>=10))
  
  dados <- dados %>%
    na.omit() %>% 
    filter(ano == ANO) %>% 
    group_by(grupo = genero,unidade = titulo) %>% 
    summarise(n = sum(total_vinculos_publicos)) %>% 
    ungroup()
  
  
  ## verificar se tem homem e mulher em cada familia
  
  dados_completo <- data.frame(
    unidade = rep(unique(dados$unidade),each=2),
    grupo = rep_len(unique(dados$grupo),length.out = 2*length(unique(dados$unidade)))
  )
  
  dados <- dados_completo %>% left_join(dados) %>% 
    mutate(ocupacao_vazia = ifelse(is.na(n),unidade,NA_character_))
  
  ocupacoes_remover <- na.omit(dados$ocupacao_vazia)
  
  dados <- dados %>% 
    filter(!(unidade %in% ocupacoes_remover)) %>% 
    select(unidade,grupo,n)
  
}


#library(segregation)


mut = mutual_total(data = dados,
                   group = "grupo",
                   unit = "unidade",
                   weight = "n"
)

diss = dissimilarity(data = dados,
                     group = "grupo",
                     unit = "unidade",
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


mut_loc = mutual_local(data = dados,
                   group = "grupo",
                   unit = "unidade",
                   weight = "n"
)

a = mut_loc %>% as_tibble() %>% mutate(est = round(100*est,4))



mutual_total <- function(data, group, unit, within = NULL, weight = NULL,
                         se = FALSE, CI = 0.95, n_bootstrap = 100, base = exp(1)) {
  stopifnot(CI > 0 & CI < 1)
  d <- prepare_data(data, group, unit, weight, within)
  
  if (is.null(within)) {
    ret <- mutual_total_compute(d, group, unit, base)
  } else {
    ret <- mutual_total_within_compute(d, group, unit, within, base)
  }
  
  if (se == TRUE) {
    vars <- attr(d, "vars")
    n_total <- sum(d[["freq"]])
    
    if (all.equal(n_total, round(n_total)) == TRUE) {
      message(paste0(n_bootstrap, " bootstrap iterations on ", n_total, " observations"))
    } else {
      stop(paste0(
        "bootstrap with a total sample size that is not an integer is not allowed, ",
        "maybe scale your weights?"
      ))
    }
    # draw from a multinomial with weights specified by the cell counts
    draws <- stats::rmultinom(n_bootstrap, n_total, d[["freq"]] / n_total)
    
    boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
      if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)
      d[, freq := as.double(draws[, i])]
      
      if (is.null(within)) {
        mutual_total_compute(d[freq > 0], group, unit, base)
      } else {
        mutual_total_within_compute(d[freq > 0], group, unit, within, base)
      }
    })
    close_log()
    boot_ret <- rbindlist(boot_ret)
    ret <- bootstrap_summary(ret, boot_ret, "stat", CI)
    setattr(ret, "bootstrap", boot_ret)
  }
  ret
}


mutual_total_compute <- function(data, group, unit, base) {
  n_total <- sum(d[, freq])
  data[, n_unit := sum(freq), by = unit]
  data[, n_group := sum(freq), by = group]
  data[, `:=`(p = freq / n_total, p_exp = n_group * n_unit / (n_total^2))]
  
  # calculate M
  M <- data[p > 0, sum(p * log(p / p_exp, base = base))]
  
  # calculate H
  p <- data[, list(p = first(n_group / n_total)), by = group][["p"]]
  entropy_group <- sum(p * log(1 / p, base))
  H <- M / entropy_group
  
  data.table(
    stat = c("M", "H"), est = c(M, H),
    stringsAsFactors = FALSE
  )
}

prepare_data <- function(data, group, unit, weight, within = NULL) {
  if ("data.frame" %in% class(data)) {
    if (nrow(data) == 0) {
      stop("data.frame is empty")
    }
    test_vars <- c(group, unit, weight, within)
    test_vars <- test_vars[!test_vars %in% names(data)]
    if (length(test_vars) > 0) {
      test_vars <- paste0(test_vars, collapse = ", ")
      stop(paste0("variable(s) ", test_vars, " not in data.frame"))
    }
  } else {
    stop("not a data.frame")
  }
  vars <- c(group, unit)
  
  # create a copy
  data <- as.data.table(data)
  
  # check whether there is variation
  n_groups <- nrow(data[, .N, by = group])
  n_units <- nrow(data[, .N, by = unit])
  if (n_groups == 1) stop("Cannot compute segregation: the group variable is constant")
  if (n_units == 1) stop("Cannot compute segregation: the unit variable is constant")
  
  # use provided weight or weight of 1
  weight_no_conflict <- weight
  if (!is.null(weight_no_conflict)) {
    if (weight_no_conflict == "weight") {
      data[, freq := as.double(weight)]
    } else {
      data[, freq := as.double(get(weight_no_conflict))]
    }
  } else {
    data[, freq := 1]
  }
  
  if (!is.null(within)) {
    vars <- c(vars, within)
  }
  
  # drop unused factor levels - these can lead to problems downstream
  for (var in vars) {
    if (is.factor(data[[var]])) {
      data[[var]] <- droplevels(data[[var]])
    }
  }
  
  # collapse on vars, and select only positive weights
  data <- data[freq > 0, list(freq = sum(freq)), by = vars]
  setattr(data, "vars", vars)
  setkey(data, NULL)
  data
}


# Aplicar a função para cada ocupação
entropia_por_ocupacao <- aggregate(n ~ unidade, data = dados, FUN = sum)
entropia_por_ocupacao$E_i <- sapply(split(dados, dados$unidade), calcular_entropia)

entropia_por_ocupacao$prop <- entropia_por_ocupacao$n/sum(entropia_por_ocupacao$n)

# Aplicar a função para cada ocupação
entropia_por_sexo <- aggregate(n ~ grupo, data = dados, FUN = sum)
entropia_por_sexo$E_i <- sapply(split(dados, dados$grupo), calcular_entropia)


df = dados %>% 
  left_join(entropia_por_ocupacao %>% select(unidade, entropia_unidade = E_i)) %>% 
  left_join(entropia_por_sexo %>% select(grupo, entropia_grupo = E_i))

df$P_j = as.numeric(df$P_j)
df$entropia_unidade = as.numeric(df$entropia_unidade)
df$entropia_grupo = as.numeric(df$entropia_grupo)

df = df %>% 
  mutate(parcela_m = P_j * (entropia_grupo - entropia_por_ocupacao))









