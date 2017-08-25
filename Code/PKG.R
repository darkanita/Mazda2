# ------------------------------------------
#  librerías necesarias para el aplicativo
# ------------------------------------------
#Extraer Informacion HTML Paginas WEB
if(!require(rvest))
  install.packages('rvest')
suppressPackageStartupMessages(require(rvest))
#Manejo de String
if(!require(stringr))
  install.packages('stringr')
suppressPackageStartupMessages(require(stringr))
#Conexion a BD por RODBC
if(!require(RODBC))
  install.packages('RODBC')
suppressPackageStartupMessages(require(RODBC))
#Manejo de data.table
if(!require(data.table))
  install.packages('data.table')
suppressPackageStartupMessages(require(data.table))
#Manejo de xtable (sale en Formato Latex)
if(!require(xtable))
  install.packages('xtable')
suppressPackageStartupMessages(require(xtable))
#Manejo de rockchalk (Para presentar modelos que compiten)
if(!require(rockchalk))
  install.packages('rockchalk')
suppressPackageStartupMessages(require(rockchalk))
#Graficos en R
if(!require(ggplot2))
  install.packages('ggplot2')
suppressPackageStartupMessages(require(ggplot2))
#Graficos en R
if(!require(tabplot))
  install.packages('tabplot')
suppressPackageStartupMessages(require(tabplot))
#Compiler
if(!require(compiler))
  install.packages('compiler')
suppressPackageStartupMessages(require(compiler))