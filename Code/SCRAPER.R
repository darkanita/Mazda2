# ---------------------------------------------------------------------------#
#                   SCRAPER - TUCARRO.COM
#                           2017                     
#                                                
# OBJETIVO: Extraer información del Carro Mazda2 desde tucarro.com
# Desarrollado por: Ana Maria Lopez / Pedro Pablo Villegas
# Curso: Modelos Lineales
# Profesor: Juan Carlos Correa
# 
# ---------------------------------------------------------------------------#
#                            INICIO DE PROCESO
# ---------------------------------------------------------------------------#
# Estructura de Directorios
dir.principal  <- 'D:/UNAL/Modelos Lineales/Mazda2/Solucion'
dir.funciones  <- 'D:/UNAL/Modelos Lineales/Mazda2/Solucion/Code'
dir.input      <- 'D:/UNAL/Modelos Lineales/Mazda2/Solucion/Data/in/'
dir.output     <- 'D:/UNAL/Modelos Lineales/Mazda2/Solucion/Data/out/'
# --------------------------------------------------------------------------#
#                            INSTALAR LIBRERIAS
# --------------------------------------------------------------------------#
setwd(dir.funciones)
options(warn = -1)
list.files()
# cargando/instalando librerias necesarias
source(list.files(pattern = "PKG"))
# --------------------------------------------------------------------------#
#                            CAPTURAR INFORMACION
# --------------------------------------------------------------------------#
#URL 
url <- 'http://carros.tucarro.com.co/carros-camionetas/mazda/mazda-2/'
#Leyendo Codigo HTML
webpage <- read_html(url)
#Capturar Cantidad de Resultados.
result_data_html <- str_replace_all(html_text(html_nodes(webpage,'.page'),'.fromPage'),"\t"," ")
result_data_html <- str_trim(result_data_html)
result_data_html <- strsplit(result_data_html," ")
total_data <- as.integer(unlist(result_data_html)[4])
i <- 1
all_data <- NULL
#Recorrer Paginas
while (i<=total_data){
  
  #Leyendo Codigo HTML
  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the rankings section
  title_data_html <- html_nodes(webpage,'.list-view-item-title')
  price_data_html <- html_nodes(webpage,'.ch-price')
  model_data_html <- html_nodes(webpage,'.destaque')
  site_data_html <- html_nodes(webpage,'.more-info')
  
  #Results
  result_data_html <- str_replace_all(html_text(html_nodes(webpage,'.page'),'.fromPage'),"\t"," ")
  result_data_html <- str_trim(result_data_html)
  result_data_html <- strsplit(result_data_html," ")
  #str(result_data_html)
  
  #URL
  url_data_html <- html_attr(html_children(title_data_html), "href", default = NA_character_)
  
  #Converting the ranking data to text
  title_data <- str_trim(html_text(title_data_html))
  price_data <- str_trim(html_text(price_data_html))
  model_data <- str_extract(str_trim(html_text(model_data_html)), "[0-9 ]{1,4}")
  kms_data <- str_trim(substr(str_trim(html_text(model_data_html)),8,20))
  site_data <- str_trim(html_text(site_data_html))
  range_data <- str_trim(str_replace_all(unlist(result_data_html)[2],"-",":"))
  id_data <- c(unlist(strsplit(range_data,":"))[1]:unlist(strsplit(range_data,":"))[2])
  #total_data <- unlist(result_data_html)[4]
  
  #Captura URL pagina siguiente
  pag_data_html <- html_nodes(webpage,'.last-child')
  
  i <- as.integer(unlist(strsplit(range_data,":"))[2]) + 1
  url <- html_attr(html_children(pag_data_html), "href", default = NA_character_)
    
  data <- data.frame(
                      Id=id_data,
                      Titulo=title_data,
                      Precio=price_data,
                      Modelo_anyo=model_data,
                      Kilometros=kms_data,
                      Ubicacion=site_data,
                      URL=url_data_html
                    )
  
  all_data <- rbind(all_data,data)
  
  
}

#Adicionar Columnas 
all_data["Color"] <- NA
all_data["Combustible"] <- NA
all_data["Recorrido"] <- NA
all_data["Marca"] <- NA

all_data["Modelo"] <- NA
all_data["Unico"] <- NA
all_data["Placa"] <- NA
all_data["Anyo"] <- NA

all_data["Version"] <- NA
all_data["FrenosABS"] <- NA
all_data["Aire"] <- NA
all_data["Airbag"] <- NA

all_data["Asientos"] <- NA
all_data["Cilindros"] <- NA
all_data["Direccion"] <- NA
all_data["Financiamiento"] <- NA

all_data["Motor"] <- NA
all_data["Negociable"] <- NA
all_data["MotorReparado"] <- NA
all_data["Sonido"] <- NA

all_data["Traccion"] <- NA
all_data["Transmision"] <- NA
all_data["Vidrios"] <- NA

for(i in 1:length(all_data$Id))
{
  url <- as.character(all_data[i,"URL"])
  webpage <- read_html(url)
  details_data_html <- html_nodes(webpage,'.attribute-group')
  details_data <- strsplit(str_replace_all(str_replace_all(html_text(details_data_html),"\t",""),"\n",""),":")

  for (j in 1:length(details_data))
  {
    if(unlist(details_data[j])[1] == "Color")
      all_data[i,"Color"] <- str_trim(unlist(details_data[j])[2])
    if(unlist(details_data[j])[1] == "Combustible")
      all_data[i,"Combustible"] <- str_trim(unlist(details_data[j])[2])
    if(unlist(details_data[j])[1] == "Recorrido")
      all_data[i,"Recorrido"] <- str_trim(unlist(details_data[j])[2])   
    if(unlist(details_data[j])[1] == "Marca")
      all_data[i,"Marca"] <- str_trim(unlist(details_data[j])[2]) 
    
    if(unlist(details_data[j])[1] == "Modelo")
      all_data[i,"Modelo"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "?nico due?o")
      all_data[i,"Unico"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "Placa")
      all_data[i,"Placa"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "A?o")
      all_data[i,"Anyo"] <- str_trim(unlist(details_data[j])[2]) 
    
    if(unlist(details_data[j])[1] == "Versi?n")
      all_data[i,"Version"] <- str_trim(unlist(details_data[j])[2])   
    if(unlist(details_data[j])[1] == "Frenos ABS")
      all_data[i,"FrenosABS"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "Aire Acondicionado")
      all_data[i,"Aire"] <- str_trim(unlist(details_data[j])[2])  
    if(unlist(details_data[j])[1] == "Airbag")
      all_data[i,"Airbag"] <- str_trim(unlist(details_data[j])[2]) 
    
    if(unlist(details_data[j])[1] == "Asientos")
      all_data[i,"Asientos"] <- str_trim(unlist(details_data[j])[2])   
    if(unlist(details_data[j])[1] == "Nro. de cilindros")
      all_data[i,"Cilindros"] <- str_trim(unlist(details_data[j])[2])  
    if(unlist(details_data[j])[1] == "Direcci?n")
      all_data[i,"Direccion"] <- str_trim(unlist(details_data[j])[2])   
    if(unlist(details_data[j])[1] == "Doy financiamiento")
      all_data[i,"Financiamiento"] <- str_trim(unlist(details_data[j])[2])  
    
    if(unlist(details_data[j])[1] == "Motor")
      all_data[i,"Motor"] <- str_trim(unlist(details_data[j])[2])   
    if(unlist(details_data[j])[1] == "Negociable")
      all_data[i,"Negociable"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "Motor reci?n reparado")
      all_data[i,"MotorReparado"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "Sonido")
      all_data[i,"Sonido"] <- str_trim(unlist(details_data[j])[2]) 
    
    if(unlist(details_data[j])[1] == "Tracci?n")
      all_data[i,"Traccion"] <- str_trim(unlist(details_data[j])[2])  
    if(unlist(details_data[j])[1] == "Transmisi?n")
      all_data[i,"Transmision"] <- str_trim(unlist(details_data[j])[2]) 
    if(unlist(details_data[j])[1] == "Vidrios")
      all_data[i,"Vidrios"] <- str_trim(unlist(details_data[j])[2]) 
  }
}
all_data$fecha_grabacion <- Sys.Date()

write.table(all_data, 
            paste(dir.output,"Datos_Mazda2.csv",sep=''),
            sep="|", col.names=TRUE, row.names=FALSE, quote=TRUE, na="",dec=',',fileEncoding = "UTF-8")