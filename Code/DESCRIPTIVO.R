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
# Ejecutar funciones de Eliminacion de Campos no Necesarios
funciones <- list.files(pattern = 'DELETE')   # NUM: Vision unica numericas
for(f in funciones) source(f)
# --------------------------------------------------------------------------#
#                            MAZDA2
# --------------------------------------------------------------------------#
system.time(mazda2 <- fread(paste(dir.output,'Datos_Mazda2_20170826.csv',sep=''),sep = "|",encoding="UTF-8",head=T, na.strings=c("NULL","",NA,NULL)))
system.time(divipola <- fread(paste(dir.input,'departamento.csv',sep=''),sep = ",",encoding="UTF-8",head=T, na.strings=c("NULL","",NA,NULL)))
# --------------------------------------------------------------------------#
#                     ELIMINAR CAMPOS NO NECESARIOS
# --------------------------------------------------------------------------#
system.time(mazda2 <-DELETE01(mazda2))
# --------------------------------------------------------------------------#
#                       ANALISIS DESCRIPTIVO
# --------------------------------------------------------------------------#
descriptivo <- data.frame(
  Variable=NULL,
  tipoVariable=NULL,
  Min=NULL,
  Mediana=NULL,
  Media=NULL,
  Max=NULL,
  Q5=NULL,
  Q10=NULL,
  Q25=NULL,
  Q50=NULL,
  Q75=NULL,
  Q90=NULL,
  Q95=NULL,
  Porc_Nulos=NULL,
  Cant_Nulos=NULL,
  Total=NULL
)
colnames <- names(mazda2)
i <- 1
for (col in colnames){
  colname <- parse(text = paste0("mazda2$",col))
  if(class(eval(colname))=="integer" || class(eval(colname))=="numeric"){
    print(col)
    descriptivo[i,"Variable"] <- col
    descriptivo[i,"tipoVariable"] <- "integer"
    descriptivo[i,"Min"] <- min(eval(colname))
    descriptivo[i,"Mediana"] <- median(eval(colname))
    descriptivo[i,"Media"] <- mean(eval(colname))
    descriptivo[i,"Max"] <- max(eval(colname))
    descriptivo[i,"Q5"] <- quantile(eval(colname), c(0.05), na.rm = TRUE)  
    descriptivo[i,"Q10"] <- quantile(eval(colname), c(0.1), na.rm = TRUE)
    descriptivo[i,"Q25"] <- quantile(eval(colname), c(0.25), na.rm = TRUE)  
    descriptivo[i,"Q50"] <- quantile(eval(colname), c(0.5), na.rm = TRUE)  
    descriptivo[i,"Q75"] <- quantile(eval(colname), c(0.75), na.rm = TRUE)
    descriptivo[i,"Q90"] <- quantile(eval(colname), c(0.9), na.rm = TRUE)
    descriptivo[i,"Q95"] <- quantile(eval(colname), c(0.95), na.rm = TRUE)
    descriptivo[i,"Porc_Nulos"] <- sum(is.na(eval(colname))==TRUE)/length(eval(colname))
    descriptivo[i,"Cant_Nulos"] <- sum(is.na(eval(colname))==TRUE)
    descriptivo[i,"Total"] <- length(eval(colname))
  }
  
  if(class(eval(colname))=="character"){
    print(col)
    
    etiquetas <- sort(unique(eval(colname)))
    #length(etiquetas)
    maet <- data.frame(ETIQUETA=NULL)
    
    for (j in 1:length(etiquetas))
      maet[j,"ETIQUETA"] <- str_trim(etiquetas[j])
    
    #print(maet)
    tmp <-eval(colname)
    
    for (x in 1:length(tmp)){
      for (j in 1:length(maet$ETIQUETA)){
        if (!is.na(tmp[x]) && str_trim(tmp[x])==maet[j,"ETIQUETA"]){
          tmp[x] <- as.integer(j)
          break
        }
      }
    }
    
    tmp1 <- as.data.frame(tmp)
    names(tmp1) <- paste(col,"_r",sep="")
    mazda2 <- cbind(mazda2,tmp1)
   
    descriptivo[i,"Variable"] <- col
    descriptivo[i,"tipoVariable"] <- "character"
    descriptivo[i,"Min"] <- min(eval(colname))
    
    descriptivo[i,"Mediana"] <- maet[as.integer(median(as.integer(tmp),na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Media"] <-  maet[as.integer(mean(as.integer(tmp),na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Max"] <- max(eval(colname))
    descriptivo[i,"Q5"] <- maet[as.integer(quantile(as.integer(tmp), c(0.05), na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Q10"] <- maet[as.integer(quantile(as.integer(tmp), c(0.1), na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Q25"] <- maet[as.integer(quantile(as.integer(tmp), c(0.25), na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Q50"] <- maet[as.integer(quantile(as.integer(tmp), c(0.5), na.rm = TRUE)),"ETIQUETA"]  
    descriptivo[i,"Q75"] <- maet[as.integer(quantile(as.integer(tmp), c(0.75), na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Q90"] <- maet[as.integer(quantile(as.integer(tmp), c(0.9), na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Q95"] <- maet[as.integer(quantile(as.integer(tmp), c(0.95), na.rm = TRUE)),"ETIQUETA"]
    descriptivo[i,"Porc_Nulos"] <- sum(is.na(eval(colname))==TRUE)/length(eval(colname))
    descriptivo[i,"Cant_Nulos"] <- sum(is.na(eval(colname))==TRUE)
    descriptivo[i,"Total"] <- length(eval(colname))
    
  }
  i <- i + 1  
}

fecha <- format(Sys.Date(),"%Y%m%d")

write.table(descriptivo, 
            paste(dir.output,"Descriptivo_Mazda2_",fecha,'.csv',sep=''),
            sep="|", col.names=TRUE, row.names=FALSE, quote=TRUE, na="",dec=',',fileEncoding = "UTF-8")


#(mazda2, 
#            paste(dir.output,"DatosFA_Mazda2_",fecha,'.csv',sep=''),
#            sep="|", col.names=TRUE, row.names=FALSE, quote=TRUE, na="",dec=',',fileEncoding = "ASCII")

write.table(mazda2, 
            paste(dir.output,"DatosFU_Mazda2_",fecha,'.csv',sep=''),
            sep="|", col.names=TRUE, row.names=FALSE, quote=TRUE, na="",dec=',',fileEncoding = "UTF-8")

#unique(mazda2$Ubicacion)







#tableplot(mazda2,cex = 1.8)
#summary(mazda2)

#hist(AirPassengers, 
#    main="Histogram for Air Passengers", 
#   xlab="Passengers", 
#  border="blue", 
# col="green",
#     xlim=c(100,700),
#    las=1, 
#   breaks=5)


