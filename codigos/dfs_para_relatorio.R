# leitura e tratamento de dados para o relatorio

## tratamento inicial - 01/04/2024

require(dplyr)
require(openxlsx)
require(stringr)
require(readxl)

diretorio_trabalho = getwd()

if(!stringr::str_detect(diretorio_trabalho,"executivo_estadual")){
  cat("######################################################")
  cat('\n')
  cat("######################################################")
  cat('\n')
  stop("CÓDIGO INTERROMPIDO -- VERIFICAR DIRETÓRIO DE TRABALHO")
}

# funcao de leitura de multiplas planilhas
multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  return(data_frame)
} 


# tabela fonte 1: vinculos_v6_resumos.brasil_v12_esfera_x_poder
# tabela fonte 2: vinculos_v6_resumos.brasil_v12_corxsexoxpoderxesfera
# tabela fonte 3: public.brasil_ee_sexo_85_03
# tabela fonte 4: public.brasil_ee_decis
# tabela fonte 5: vinculos_v6_resumos.uf_v11_genero_poder_esfera
# tabela fonte 6: vinculos_v6_resumos.brasil_v12_publico_rem_decil_ee

# gerar tabelas e colocar numa planilha

################################################################################
# planilha
################################################################################

caminho_planilha = '../dados/planilha_dados.xlsx'
base_completa <- multiplesheets(caminho_planilha)


################################################################################
# FIM
################################################################################



