###########################################################
###########################################################
###########################################################

# PROJETO: BUROCRACIAS ESTADUAIS
# 09/03/2024
# 
# ALIMENTAR TEXTO DE MESTRANDO DO IDP
# E PRODUCAO DE DADOS PARA ARTIGO DO PROJETO

###########################################################
###########################################################
###########################################################

require(here)

















#######
##################################################################
###########################################################
###########################################################

# ANTIGO

# EPA trabalho final PPEPA

require(PNADcIBGE)
require(survey)
require(dplyr)

diretorio <- "C:/ipea/ods_1671/"
diretorio <- "C:/Temp/"

#openxlsx::write.xlsx(iris, file = paste0(diretorio,"dados/arquivo.xlsx"))

# Coleta e organização dos dados
variaveis_selecionadas <- c(
  "Ano","Trimestre","UF","UPA","Estrato","V1008","V1014","V1016","V1022","V1023",
  "V1027","V1028","V1029","V1033","posest","posest_sxi",
  "V2001","V2003","V2005","V2007","V2009","V2010",
  "VD4001","VD4002",
  "S11001","S11002","S11003","S11004","S11005","S11006","S11007","S11008"
)

pnadc_design <- get_pnadc(
  year = 2022,
  topic = 3, # anual com suplemento do terceiro trimestre
  selected = TRUE,
  vars = variaveis_selecionadas,
  savedir = paste0(diretorio,"dados/")
  )


# salvar versao

saveRDS(pnadc_design, file = paste0(diretorio,"dados/PNADC_2022_trimestre3_DESIGN"))

rm(list = ls())
gc()

# carregar versao
diretorio <- "C:/Temp/"
pnadc_design <- readRDS(file = paste0(
  diretorio,"dados/PNADC_2022_trimestre3_DESIGN.rds"))

# confirmando dados
# deletar arquivo txt da pnadc
# 
#fs::file_delete(paste0(diretorio,"dados/PNADC_2022_trimestre3.txt"))
#fs::file_delete(paste0(diretorio,"dados/PNADC_2022_trimestre3.zip"))


# TESTE DOS DADOS COM O SURVEY
pnadc_design <- update(pnadc_design, um = 1)

# vetor de deficiencias
t <- c("Tem, não consegue de modo algum","Tem muita dificuldade")

pnadc_design <- update(pnadc_design, com_def = ifelse(
  S11001 %in% t | S11002 %in% t | S11003 %in% t | S11004 %in% t |
    S11005 %in% t | S11006 %in% t | S11007 %in% t | S11008 %in% t, 1L, 0L))

# regiao
pnadc_design <- update(pnadc_design, cod_regiao = substr(as.character(UPA),1,1))



# desocupados
# não ocupados + fora da força


# TOTAL DA POPULACAO
est_1 <- svytotal(~um,design = pnadc_design)

# TOTAL DA POPULACAO por regiao
est_1_1 <- svyby(~um, ~cod_regiao, design = pnadc_design, svytotal)

# TOTAL DA POPULACAO DE 14 A 59 ANOS
est_2 <- svytotal(~um,design = subset(pnadc_design, V2009 %in% 14:59 ))


# TOTAL DA POPULACAO ocupados
est_3 <- svyby(~um,~VD4002,
         design = subset(pnadc_design, V2009 %in% 14:59 ),
         svytotal,
         na.rm=TRUE)


# TOTAL DA POPULACAO deficiencia
est_4 <- svyby(~um,~com_def,
               design = subset(pnadc_design, V2009 %in% 14:59),
               svytotal,
               na.rm=TRUE)

# taxa de desocupacao
#  razao
#  filtro:  mais de 14 anos
#  num: descupado - VD4002 = 2
#  denom: na FT - VD4001 = 1

est_5 <- svyratio(numerator = ~VD4002=="Pessoas desocupadas",
         denominator = ~VD4001=="Pessoas na força de trabalho",
         design = subset(pnadc_design, V2009 >= 14),
         na.rm=TRUE)


# taxa de desocupacao com deficiencia
#  razao
#  filtro:  mais de 14 anos
#  num: descupado - VD4002 = 2
#  denom: na FT - VD4001 = 1

est_6 <- svyby(~(VD4002=="Pessoas desocupadas"),
               by = ~com_def,
               denominator = ~(VD4001=="Pessoas na força de trabalho"),
               design = subset(pnadc_design, V2009 >= 14),
               svyratio,
               na.rm=TRUE)


df_est_6 <- tibble::tibble(est_6)

names(df_est_6) <- c("com_def","desocup_sob_FT","SE_desocup_sob_FT")

df_est_6 <- df_est_6 %>% mutate(
  cv_percent = round(100*(SE_desocup_sob_FT/desocup_sob_FT),1),
  desocup_sob_FT = round(desocup_sob_FT*100,1)
)

# consegui comparar


#Indicador
# pessoas com deficiencia segundo grande região

est_7 <- svyby(~um,~com_def+cod_regiao,
               design = subset(pnadc_design, V2009 %in% 14:59),
               svytotal,
               na.rm=TRUE,
               drop.empty.groups=FALSE)

df_est_7 <- tibble::tibble(est_7)

names(df_est_7) <- c("com_def","cod_regiao","total","se")

df_est_7 <- df_est_7 %>% mutate(
  cv_percent = round(100*(se/total),1),
  est_total = round(total/1000))


# taxa de desocupação com deficiencia para BRASIL

est_8 <- svyby(~VD4002=="Pessoas desocupadas",
               by=~com_def,
               design = subset(pnadc_design, V2009 >=14),
               denominator = ~VD4001=="Pessoas na força de trabalho",
               svyratio,
               na.rm=TRUE)


df_est_8 <- tibble::tibble(est_8)

names(df_est_8) <- c("com_def","razao","se")

df_est_8 <- df_est_8 %>% mutate(
  cv_percent = round(100*(se/razao),1),
  est_razao = round(razao*100,1))



# taxa de desocupação com deficiencia segundo grande região

est_9 <- svyby(~VD4002=="Pessoas desocupadas",
               ~com_def+cod_regiao,
               design = subset(pnadc_design, V2009 >=14),
               denominator = ~VD4001=="Pessoas na força de trabalho",
               svyratio,
               na.rm=TRUE)


df_est_9 <- tibble::tibble(est_9)

names(df_est_9) <- c("com_def","cod_regiao","razao","se")

df_est_9 <- df_est_9 %>% mutate(
  cv_percent = round(100*(se/razao),4),
  est_razao = round(razao*100,1))



# taxa de desocupação com deficiencia segundo grande região

est_10 <- svyby(~VD4002=="Pessoas desocupadas",
               ~com_def+UF,
               design = subset(pnadc_design, V2009 >=14),
               denominator = ~VD4001=="Pessoas na força de trabalho",
               svyratio,
               na.rm=TRUE)


df_est_10 <- tibble::tibble(est_10)

names(df_est_10) <- c("com_def","UF","razao","se")

df_est_10 <- df_est_10 %>% mutate(
  cv_percent = round(100*(se/razao),4),
  est_razao = round(razao*100,1))


require(ggplot2)

ggplot(df_est_10, aes(y=cv_percent)) +
  geom_boxplot()+
  facet_grid(~com_def)







################## FIM DO DOCUMENTO ##################

# variaveis
# Ano, Trimestre, UF, UPA, Estrato, V1008,V1014, V1016,V1022,V1023,
# V1027,V1028,V1029,V1033,posest,posest_sxi,
# V2001, V2003,V2005,V2007,V2009,V2010,
# VD4001,VD4002,
# S11001, S11002, S11003, S11004, S11005, S11006, S11007, S11008
 

# Indicador
# Pessoa com Deficiência:  
# S11001 in ("1","2") ou S11002 in ("1","2") ou S11003 in ("1","2") ou 
# S11004 in ("1","2") ou S11005 in ("1","2") ou S11006 in ("1","2") ou 
# S11007 in ("1","2") ou S11008 in ("1","2")

#Pessoa sem deficiência: se D7 for diferente de 1 e V2009 >= 2
