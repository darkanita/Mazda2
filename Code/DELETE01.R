# ------------------------------------------------------------------------------------------------------------------
# ELIMINAMOS VARIABLES TEMPORALES DEL PROCESO
# ------------------------------------------------------------------------------------------------------------------

DELETE01 <- function(mazda2=mazda2)
{
  #names(mazda2)
  options("scipen"=100, "digits"=4)
  
  mazda2$Modelo_anyo<-as.integer(mazda2$Modelo_anyo)
  mazda2$Recorrido<-as.integer(mazda2$Recorrido)
  
  for (i in 1:length(mazda2$Unico)){
    mazda2[i,"Precio"] <- str_replace_all(mazda2[i,"Precio"],"\\.","")
    mazda2[i,"Precio"] <- str_replace_all(mazda2[i,"Precio"],"[$]","")
    mazda2[i,"Precio"] <- str_replace_all(mazda2[i,"Precio"],"[U]","")
    if(!is.na(mazda2[i,"Unico"])){
      if(mazda2[i,"Unico"]=="Sí")
        mazda2[i,"Unico"] <- 1
      else
        mazda2[i,"Unico"] <- 0
    }
    if(!is.na(mazda2[i,"FrenosABS"])){
      if(mazda2[i,"FrenosABS"]=="Sí")
        mazda2[i,"FrenosABS"] <- 1
      else
        mazda2[i,"FrenosABS"] <- 0
    }
    
    if(!is.na(mazda2[i,"Aire"])){
      if(mazda2[i,"Aire"]=="Sí")
        mazda2[i,"Aire"] <- 1
      else
        mazda2[i,"Aire"] <- 0
    }
    if(!is.na(mazda2[i,"Airbag"])){
      if(mazda2[i,"Airbag"]=="Sí")
        mazda2[i,"Airbag"] <- 1
      else
        mazda2[i,"Airbag"] <- 0
    }

    if(!is.na(mazda2[i,"Financiamiento"])){
      if(mazda2[i,"Financiamiento"]=="Sí")
        mazda2[i,"Financiamiento"] <- 1
      else
        mazda2[i,"Financiamiento"] <- 0
    }
    if(!is.na(mazda2[i,"Negociable"])){
      if(mazda2[i,"Negociable"]=="Sí")
        mazda2[i,"Negociable"] <- 1
      else
        mazda2[i,"Negociable"] <- 0
    }
    if(!is.na(mazda2[i,"MotorReparado"])){
      if(mazda2[i,"MotorReparado"]=="Sí")
        mazda2[i,"MotorReparado"] <- 1
      else
        mazda2[i,"MotorReparado"] <- 0
    }
    if(is.na(mazda2[i,"AlarmaControl"])){
        mazda2[i,"AlarmaControl"] <- 0
    }
    if(is.na(mazda2[i,"RastreoSatelital"])){
      mazda2[i,"RastreoSatelital"] <- 0
    }
    if(is.na(mazda2[i,"Asegurado"])){
      mazda2[i,"Asegurado"] <- 0
    }
    if(is.na(mazda2[i,"CD"])){
      mazda2[i,"CD"] <- 0
    }
    if(is.na(mazda2[i,"DVD"])){
      mazda2[i,"DVD"] <- 0
    }
    if(is.na(mazda2[i,"Planta"])){
      mazda2[i,"Planta"] <- 0
    }
    if(is.na(mazda2[i,"Estribos"])){
      mazda2[i,"Estribos"] <- 0
    }
    if(is.na(mazda2[i,"Buffer"])){
      mazda2[i,"Buffer"] <- 0
    }
    if(is.na(mazda2[i,"ForroLlantaRepuesto"])){
      mazda2[i,"ForroLlantaRepuesto"] <- 0
    }
    if(is.na(mazda2[i,"LlantasNuevas"])){
      mazda2[i,"LlantasNuevas"] <- 0
    }
    if(is.na(mazda2[i,"LucesAntiNiebla"])){
      mazda2[i,"LucesAntiNiebla"] <- 0
    }
    if(is.na(mazda2[i,"PeliculaSeguridad"])){
      mazda2[i,"PeliculaSeguridad"] <- 0
    }
    if(is.na(mazda2[i,"RetrovisoresElectricos"])){
      mazda2[i,"RetrovisoresElectricos"] <- 0
    }
    if(is.na(mazda2[i,"ReVTecnicoMecanica"])){
      mazda2[i,"ReVTecnicoMecanica"] <- 0
    }
    if(is.na(mazda2[i,"RinesLujo"])){
      mazda2[i,"RinesLujo"] <- 0
    }
    if(is.na(mazda2[i,"Spoiler"])){
      mazda2[i,"Spoiler"] <- 0
    }
    if(is.na(mazda2[i,"SunRoof"])){
      mazda2[i,"SunRoof"] <- 0
    }
    if(is.na(mazda2[i,"BloqueoCentral"])){
      mazda2[i,"BloqueoCentral"] <- 0
    }
    if(is.na(mazda2[i,"ForroVolante"])){
      mazda2[i,"ForroVolante"] <- 0
    }
    if(is.na(mazda2[i,"ForroAsientos"])){
      mazda2[i,"ForroAsientos"] <- 0
    }
    if(is.na(mazda2[i,"VolanteDeportivo"])){
      mazda2[i,"VolanteDeportivo"] <- 0
    }
    if(is.na(as.numeric(mazda2[i,"Precio"]))){
      mazda2[i,"Precio"] <- 0
    }
  } 
  
  mazda2$Precio<-as.numeric(mazda2$Precio)
  mazda2$Unico<-as.integer(mazda2$Unico)
  mazda2$FrenosABS<-as.integer(mazda2$FrenosABS)
  mazda2$Aire<-as.integer(mazda2$Aire)
  mazda2$Airbag<-as.integer(mazda2$Airbag)
  mazda2$VolanteDeportivo<-as.integer(mazda2$VolanteDeportivo)
  mazda2$ForroAsientos<-as.integer(mazda2$ForroAsientos)
  mazda2$ForroVolante<-as.integer(mazda2$ForroVolante)
  mazda2$Financiamiento<-as.integer(mazda2$Financiamiento)
  mazda2$Negociable<-as.integer(mazda2$Negociable)
  mazda2$MotorReparado<-as.integer(mazda2$MotorReparado)
  mazda2$AlarmaControl<-as.integer(mazda2$AlarmaControl)
  mazda2$RastreoSatelital<-as.integer(mazda2$RastreoSatelital)
  mazda2$CD<-as.integer(mazda2$CD)
  mazda2$DVD<-as.integer(mazda2$DVD)
  mazda2$Planta<-as.integer(mazda2$Planta)
  mazda2$Buffer<-as.integer(mazda2$Buffer)
  mazda2$Estribos<-as.integer(mazda2$Estribos)
  mazda2$ForroLlantaRepuesto<-as.integer(mazda2$ForroLlantaRepuesto)
  mazda2$LlantasNuevas<-as.integer(mazda2$LlantasNuevas)
  mazda2$LucesAntiNiebla<-as.integer(mazda2$LucesAntiNiebla)
  mazda2$PeliculaSeguridad<-as.integer(mazda2$PeliculaSeguridad)
  mazda2$RetrovisoresElectricos<-as.integer(mazda2$RetrovisoresElectricos)
  mazda2$ReVTecnicoMecanica<-as.integer(mazda2$ReVTecnicoMecanica)
  mazda2$Spoiler<-as.integer(mazda2$Spoiler)
  mazda2$RinesLujo<-as.integer(mazda2$RinesLujo)
  mazda2$SunRoof<-as.integer(mazda2$SunRoof)
  mazda2$BloqueoCentral<-as.integer(mazda2$BloqueoCentral)
  mazda2$ForroVolante<-as.integer(mazda2$ForroVolante)
  mazda2$Asegurado<-as.integer(mazda2$Asegurado)
  
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
  mazda2[,Anyo:=NULL]
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