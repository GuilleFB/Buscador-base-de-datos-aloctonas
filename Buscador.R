# Script Buscador en la Base de Datos
# Guillermo Follana Berna
# Este script busca dentro de la base de datos por especie o demarcacion y crea una taba y/o mapa de la especie buscada

# Inicio del Script
rm(list=ls(all=TRUE)) # Borrar todo los objetos
invisible(capture.output(gc())) # Limpiar la memoria

# Instalacion y carga de paquetes de R en bucle
paquetes=c("stringi",
           "stringr",
           "rgdal",
           "sf",
           "ggplot2",
           "RColorBrewer",
           "ggspatial"
           )

for (i in 1:length(paquetes)) {
  if (!require(paquetes[i], character.only = TRUE)){
    install.packages(paquetes[i], character.only = TRUE)
  }  
  library(paquetes[i], character.only = TRUE)
}

# Elegir archivo de busqueda ####
Archivo=file.choose()
BD_EAI=read.csv2(Archivo,sep = ";", dec = ",")
Directorio=paste(strsplit(Archivo,"[\\]")[[1]][1:(length(strsplit(Archivo,"[\\]")[[1]])-1)], collapse ="\\")
setwd(Directorio)
load("mapas_obj.RData")

# Funciones a usar ####
Buscar_Especie = function(x){
  temp=which(str_detect(BD_EAI$scientificname,regex(as.character(x), ignore_case=T))|
               str_detect(BD_EAI$Specie,regex(as.character(x), ignore_case=T))|
    str_detect(BD_EAI$valid_name,regex(as.character(x), ignore_case=T)))
  temp2<<-BD_EAI[temp,]
}

Buscar_Demarcacion = function(x){
  if (length(strsplit(x,",")[[1]])==5) {
    temp=which(str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][1])
               |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][2])
               |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][3])
               |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][4])
               |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][5]))
    }  else if (length(strsplit(x,",")[[1]])==4) {
      temp=which(str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][1])
                 |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][2])
                 |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][3])
                 |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][4]))
    }  else if (length(strsplit(x,",")[[1]])==3){
      temp=which(str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][1])
                 |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][2])
                 |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][3]))
    }  else if (length(strsplit(x,",")[[1]])==2){
      temp=which(str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][1])
                 |str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][2]))
    }  else {
      temp=which(str_detect(BD_EAI$Demarcacion,strsplit(x,",")[[1]][1]))
    }
  Tabla<<-BD_EAI[temp,]
}

Crear_mapa= function(x){
temp_CAN=which(x$Demarcacion=="CAN")
temp_Otras=which(x$Demarcacion!="CAN")

Can = ggplot(data = Canarias) +
  geom_sf(size = 0.1)+
  geom_sf(aes(fill= Demarcaciones, alpha=0.05), data = demarcaciones, size= 0.50, color= NA)+
  coord_sf(xlim = c(-18.2, -13.5), ylim = c(27.5,29))+ # zoom hacia la zona del mapa
  geom_point(data=x[temp_CAN,], aes(x=Longitud, y=Latitud), 
             color = "black", size=1.25, shape = 21, fill = "darkred")+
  scale_x_continuous(breaks=c(-18,-14), labels = paste0(c(18,14), "\u00b0W"))+
  scale_y_discrete(breaks=28:29,labels = paste0(seq(28,29,1), "\u00b0N"))+
  theme_minimal()+
  annotation_scale(pad_x=unit(0.1, "cm"), pad_y=unit(0.1, "cm"), width_hint = 0.2, height = unit(0.1, "cm")) +
  theme(axis.text.x =element_text(size = 4),
        axis.title.x =element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_text(size = 4),
        plot.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey10",size = 0.1555),
        legend.position="none")

Esp = ggplot(data = Spain) +
  geom_sf(size = 0.1)+
  geom_sf(aes(fill= Demarcaciones, alpha=0.05), data = demarcaciones, size= 0.50, color = NA)+
  coord_sf(xlim = c(-20, 6), ylim = c(35,46.5))+
  geom_point(data=x[temp_Otras,], aes(x=Longitud, y=Latitud), 
             color = "black", size=1.25, shape = 21, fill = "darkred")+
  scale_x_discrete(labels = c("20°W", "15°W", "10°W", "5°W",  "0°", "5°E"))+
  scale_y_discrete(labels = paste0(seq(36,46,2), "\u00b0N"))+
  theme_minimal()+
  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.1, "cm")) +
  ggtitle(unique(x$scientificname)) +
  theme(plot.title = element_text(hjust = 0.5, face="italic"),
        axis.text.x =element_text(size = 4),
        axis.title.x =element_text(size = 5),
        axis.title.y =element_text(size = 5),
        axis.text.y=element_text(size = 4),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey10",size = 0.05),
        legend.position="none")

a=Esp+annotation_custom(grob = ggplotGrob(Can)
                      ,xmin = -25, xmax = -5, ymin = 36, ymax = 40
                      )
print(a)
}

# GUI del programa ####
Sys.sleep(1)
if (interactive() ){
Nombre <- readline("Cual es tu nombre? ")
repeat{
  if (length(Nombre)>=1) {
  Orden=readline(paste("OK, ", Nombre, ", vamos a trabajar. Que quieres hacer: Buscar una especie, Buscar por demarcacion: ",sep=""))
  }
  repeat{
    if (str_detect(Orden, regex("buscar una especie", ignore_case=T))) {
      cat("Sino sabes que especie buscar puedes preguntarme que especies tengo en la Base de Datos escribiendo 'Especies'")
      Especie = readline("Que especie?, ")
  
      if (str_detect(Especie, regex("especies", ignore_case=T))) {
        print(stri_trans_general(gsub("[[:space:]]", " ",as.character(sort(unique(BD_EAI$scientificname)))),"Latin-ASCII"))
        next
      } 
      else {
        Buscar_Especie(as.character(Especie))
        Mapa=readline("Quieres ver las localizaciones en un mapa? si/no ")
        }
  
      if (str_detect(Mapa,regex("si|yes|s|y",ignore_case = T))) {
        Crear_mapa(temp2)
        }
      Guardar1 <- readline("Quieres guardar un mapa de la especie? si/no ")
      if (str_detect(Guardar1,regex("si|yes|s|y",ignore_case = T))) {
          #mapshot(mapa, url = paste(temp2[1,1],".html",sep=""))
          ggsave(paste("map_",temp2[1,1],".png",sep = ""), dpi = 500)
          cat(paste("Mapa guardado en ", Directorio , sep=""))
      }
      Ver1 <- readline("Quieres ver una tabla de la especie? si/no ")
      if (str_detect(Ver1,regex("si|yes|s|y",ignore_case = T))) {
        View(temp2)
        cat(paste("Ya puedes ver la tabla de ", Especie, sep=""))
      }
      Guardar2 <- readline("Quieres guardar una tabla de la especie? si/no ")
      if (str_detect(Guardar2,regex("si|yes|s|y",ignore_case = T))) {
          write.csv2(temp2,file=paste(Especie,".csv",sep=""),row.names = F)
          cat(paste("Tabla guardada en ", Directorio , sep=""))
          Respuesta=readline("Quieres hacer algo mas? si/no ")
        } else {
          Respuesta=readline(paste("No he guardado nada. Quieres hacer algo mas? si/no ",sep=""))
        }
      break
    }
    else if (str_detect(Orden,regex("buscar por demarcacion", ignore_case=T))) {
    Demarcacion = readline("Que demarcacion o demarcaciones: \n
    NOR:Norte\n
    SUD:Sudatlantico\n
    LEBA:Levantino-Balear\n
    ESAL:Estrecho-Alboran\n
    CAN:Canaria\n
    *** Separar las demarcaciones con comas ***")
    Buscar_Demarcacion(toupper(Demarcacion))
    Mapa=readline("Quieres ver las localizaciones en un mapa? si/no ")
    if (str_detect(Mapa,regex("si|yes|s|y",ignore_case = T))) {
      Crear_mapa(Tabla)
    }
    Guardar1 <- readline("Quieres guardar un mapa de la/s demarcacion/es? si/no ")
    if (str_detect(Guardar1,regex("si|yes|s|y",ignore_case = T))) {
        ggsave(paste("map_",toupper(as.character(Demarcacion)),".png",sep = ""), width = 17.6, height = 10,units = "cm")
        cat(paste("Mapa guardado en ", Directorio , sep=""))
    }
    Ver1 <- readline("Quieres ver una tabla de la/s demarcacion/es? si/no ")
    if (str_detect(Ver1,regex("si|yes|s|y",ignore_case = T))) {
      View(Tabla)
      cat(paste("Ya puedes ver la tabla de ", Demarcacion , sep=""))
    }
    Guardar2 <- readline("Quieres guardar una tabla de la/s demarcacion/es? si/no ")
    if (str_detect(Guardar2,regex("si|yes|s|y",ignore_case = T))) {
        write.csv2(Tabla,file=paste(toupper(as.character(Demarcacion)),".csv",sep=""),row.names = F)
        cat(paste("Tabla guardada en ", Directorio , sep=""))
        Respuesta=readline("Quieres hacer algo mas? si/no ")
    } else {
        Respuesta=readline(paste("No he guardado nada. Quieres hacer algo mas? si/no ",sep=""))
    }
    break
  }
    else {
    Orden=readline(paste("No entendi la orden: ", toupper(as.character(Orden)), ", puedes repetir que quieres hacer:\n","***** -- Buscar una especie o Buscar por demarcacion -- *****",sep=""))
    next
      }
        }
  if (str_detect(Respuesta,regex("si|yes|s|y",ignore_case = T))){
    next
    } else {
    cat("Hasta la proxima!!!!")
    break
  }
      }
  } else {
#  non-interactive
cat("a string please: ");
a <- readLines("stdin",n=1);
cat("You entered")
str(a);
cat( "\n" )
}

