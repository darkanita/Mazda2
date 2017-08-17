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