# tabulacao_dados

###############################################
# OBJETIVO                                    #
# PREPARAR DADOS NO BANCO                     #
#                                             #
###############################################

## VERSOES DO CODIGO

## tabulacao inicial           - 15/03/2024
## tabulacao inicial           - 21/03/2024
## revisão de código           - 26/06/2024
## volta a trabalhar nos dados - 16/11/2024
## revisao final do codigo     - 11/06/2025

pacman::p_load(char = c("dplyr","openxlsx","tidyr","stringr"))


diretorio_trabalho = "//srjn4/atlas/executivo_estadual/"
setwd(diretorio_trabalho)


source('./apoio/conectar.R')





