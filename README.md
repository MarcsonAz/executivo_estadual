
# Burocracias subnacionais


Códigos de produção de dados e relatórios com dados para municiar o projeto de burocracias subnacionais


repositório: **executivo_estadual**

no servidor: \\srjn4\atlas\executivo_estadual



### Dados:

Série de 1985 até 2021 de vínculos públicos no executivo estadual

Para o Brasil: total, segundo sexo, segundo cor, remuneração média e mediana, segundo decis de remuneração

Para as unidades da federação: segundo sexo, segundo cor, remuneração média e mediana, segundo decis de remuneração

!!!! Importante !!!! - Fazer upload dos dados e gráficos no Google Drive.

### Fazer: 

**escolaridade**

- arrumar processo geral para voltar a produzir dados e visualizações

- dados para gerar no banco: escolaridade

- tabelas e gráficos para gerar: escolaridade

- analises no texto no drive: verificar




**Gráficos**

Fazer o relativo do total de vínculos com 1993 de ano base

Fazer ajuste nos código de gráficos, serão 3 arquivos de gráficos: Brasil, total UF e remuneração UF. 
Estou arrumando o layout, já existem gráficos de UF com layout corrigido, como o *uf_total.png* e o *uf_remuneracao_media_mediana.png*.



### Estrturas de pastas e arquivos

\apoio\ - códigos de teste e arquivos não usados na versão atual
\codigos\ - códigos em R para produção de dados e gráficos para analises no texto
\dados\ - arquivos de dados para gerar gráficos e análises na versão atual
\graficos\ - arquivos de imagem de gráficos produzidos para as análises
\LICENSE - licensa de uso - MIT License
\README.md - (este arquivo)
\executivo_estadual.Rproj - arquivo do R para gerenciar o ambiente de produção dos códigos


### Códigos R

No diretório '\codigos' temos arquivos do tipo .R que irão fazer:

1 - As leituras, tratamentos nos dados e exportação em planilha

2 - Gerar gráficos -
  _1 no Brasil
  _2 por Unidades da federação
  _3 por medidas de remuneração no Brasil
  _4 por medidas de remuneração nas Unidades da federação


Em 1, são feitas as leituras das tabelas geradas no banco com dados da Rais,
realizados os tratamentos iniciais com os dados, tabulados individualmente e 
exportados para a planilha '\dados\planilha_dados.xlsx'.

Em 2, são gerados os gráficos e facetas com as informações das tabelas. As 
imagens são enviadas para o diretório '\graficos\'. Foi considerada uma taxonomia 
de localização, medida de total ou remuneração e mais detalhamentos na nomenclatura 
dos arquivos. Um exemplo é '\graficos\uf_total_cor_razao.png'. Que representa a razão 
entre o total de vínculos brancos e pretos ou pardos em cada unidade da federação. 
Entendendo pela nomenclatura, uf nas unidades da federação, o total pois se tratam de 
vínculos e não de remuneração, cor dado que são categorias de cor ou raça e razão para 
identifcar o indicador usado.







