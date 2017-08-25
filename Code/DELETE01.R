# ------------------------------------------------------------------------------------------------------------------
# ELIMINAMOS VARIABLES TEMPORALES DEL PROCESO
# ------------------------------------------------------------------------------------------------------------------

DELETE01 <- function(mazda2=mazda2)
{
  #names(mazda2)
  options("scipen"=100, "digits"=4)
  mazda2$Modelo_anyo<-as.numeric(mazda2$Modelo_anyo)
  mazda2$Recorrido<-as.numeric(mazda2$Recorrido)
  
  for (i in 1:length(mazda2$Unico)){
    if(!is.na(mazda2[i,"Unico"])){
      if(mazda2[i,"Unico"]=="Sí")
        mazda2[i,"Unico"] <- 1
      else
        mazda2[i,"Unico"] <- 0
    }
  } 
    

  # ELIMINAMOS VARIABLES NO NECESARIAS EN PROCESO DE MODELAMIENTO
  mazda2[,Id:=NULL]
  mazda2[,Titulo:=NULL]
  #mazda2[,Precio:=NULL]
  #mazda2[,Modelo_anyo:=NULL]
  mazda2[,Kilometros:=NULL]
  #mazda2[,Ubicacion:=NULL]
  mazda2[,URL:=NULL]
  #mazda2[,Color:=NULL]
  #mazda2[,Combustible:=NULL]
  #mazda2[,Recorrido:=NULL]
  mazda2[,Marca:=NULL]
  mazda2[,Modelo:=NULL]
  #mazda2[,Unico:=NULL]
  #mazda2[,Anyo:=NULL]
  #mazda2[,Version:=NULL]
  #mazda2[,FrenosABS:=NULL]
  #mazda2[,Aire:=NULL]
  #mazda2[,Airbag:=NULL]
  #mazda2[,Asientos:=NULL]
  #mazda2[,Cilindros:=NULL]
  #mazda2[,Direccion:=NULL]
  #mazda2[,Financiamiento:=NULL]
  #mazda2[,Motor:=NULL]
  #mazda2[,Negociable:=NULL]
  #mazda2[,MotorReparado:=NULL]
  #mazda2[,Sonido:=NULL]
  #mazda2[,Traccion:=NULL]
  #mazda2[,Transmision:=NULL]
  #mazda2[,Vidrios:=NULL]
  #mazda2[,AlarmaControl:=NULL]
  #mazda2[,Asegurado:=NULL]
  #mazda2[,RastreoSatelital:=NULL]
  #mazda2[,CD:=NULL]
  #mazda2[,DVD:=NULL]
  #mazda2[,Planta:=NULL]
  #mazda2[,Buffer:=NULL]
  #mazda2[,Estribos:=NULL]
  #mazda2[,ForroLlantaRepuesto:=NULL]
  #mazda2[,LlantasNuevas:=NULL]
  #mazda2[,LucesAntiNiebla:=NULL]
  #mazda2[,PeliculaSeguridad:=NULL]
  #mazda2[,RetrovisoresElectricos:=NULL]
  #mazda2[,ReVTecnicoMecanica:=NULL]
  #mazda2[,RinesLujo:=NULL]
  #mazda2[,Spoiler:=NULL]
  #mazda2[,SunRoof:=NULL]
  #mazda2[,BloqueoCentral:=NULL]
  #mazda2[,ForroVolante:=NULL]
  #mazda2[,ForroAsientos:=NULL]
  #mazda2[,VolanteDeportivo:=NULL]
  mazda2[,fecha_grabacion:=NULL]
  
  mazda2
  
}

DELETE01 <- cmpfun(DELETE01)