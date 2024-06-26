###
# PASSAR GRAFICOS PARA PASTA NO DRIVE


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


