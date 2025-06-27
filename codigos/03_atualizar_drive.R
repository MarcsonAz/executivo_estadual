###
# PASSAR GRAFICOS PARA PASTA NO DRIVE

### ARQUIVOS ALTERADOS NO DIA

## atualizados no dia

# lista de figuras
arquivos <- fs::dir_ls('./graficos',glob = '*.png')

# lista de planilhas
arquivos_dados <- fs::dir_ls('./dados',glob = '*.csv')


hoje = as.Date(Sys.time())


for(arq in arquivos){
  arq_info = fs::file_info(arq)
  arq_date = as.Date(arq_info$modification_time)
  
  if(hoje == arq_date){
    
    googledrive::drive_put(
      path = googledrive::as_dribble("Burocracias subnacionais"),
      media = arq)
    
  }
}








## ANTIGO ###################################################
## ANTIGO ###################################################


# lista de figuras
arquivos <- fs::dir_ls('./graficos',glob = '*.png')

# lista de planilhas
arquivos_dados <- fs::dir_ls('./dados',glob = '*.xlsx')




## Atualizar no Google Drive
### verificar autorização

### a função tenta inserir arquivos

for(arq in arquivos){
  print(arq)
  
  googledrive::drive_put(
    path = googledrive::as_dribble("Burocracias subnacionais/graficos"),
    media = arq)
}



for(arq in arquivos_dados){
  print(arq)
  
  googledrive::drive_put(
    path = googledrive::as_dribble("Burocracias subnacionais"),
    media = arq
  )
}





