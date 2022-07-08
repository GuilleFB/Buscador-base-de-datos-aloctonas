# Script Buscador en la Base de Datos
# Guillermo Follana Berna
# Este script busca dentro de la base de datos por especie o demarcacion y crea una taba y/o mapa de la especie buscada

# Inicio del Script
rm(list=ls(all=TRUE)) # Borrar todo los objetos
invisible(capture.output(gc())) # Limpiar la memoria

# Instalacion y carga de paquetes de R en bucle

paquetes=c("stringi",
           "mapview",
           "stringr",
           "rgdal",
           "sf",
           "ggplot2",
           "RColorBrewer",
           
)

for (i in 1:length(paquetes)) {
  if (!require(paquetes[i], character.only = TRUE)) install.packages(paquetes[i], character.only = TRUE) 
  library(paquetes[i], character.only = TRUE)
}


Archivo=file.choose()
BD_EAI=read.csv2(Archivo,sep = ";", dec = ",")
Directorio=paste(strsplit(Archivo,"[\\]")[[1]][1:(length(strsplit(Archivo,"[\\]")[[1]])-1)], collapse ="\\")
setwd(Directorio)
load("mapas_obj.RData")

# Formulas a usar ####
Buscar_Especie = function(x){
  temp=which(str_detect(BD_EAI$scientificname,regex(as.character(x), ignore_case=T))|str_detect(BD_EAI$Specie,regex(as.character(x), ignore_case=T)))
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
  coord_sf(xlim = c(-19, -13), ylim = c(27,30))+ # zoom hacia la zona del mapa
  geom_point(data=x[temp_CAN,], aes(x=Longitud, y=Latitud), 
             color = "black", size=0.75, shape = 24, fill = "darkred")+
  scale_y_continuous(breaks=c(27,29,31))+ # modificacion de los ejes
  scale_x_continuous(breaks=c(-19,-16,-13))+
  theme_minimal()+
  theme(axis.text.x =element_text(size = 3),
        axis.title.x =element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_text(size = 3),
        plot.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey10",size = 0.05),
        legend.position="none")

Esp = ggplot(data = Spain) +
  geom_sf(size = 0.1)+
  geom_sf(aes(fill= Demarcaciones, alpha=0.05), data = demarcaciones, size= 0.50, color = NA)+
  coord_sf(xlim = c(-15, 6), ylim = c(35,44))+
  geom_point(data=x[temp_Otras,], aes(x=Longitud, y=Latitud), 
             color = "black", size=0.75, shape = 24, fill = "darkred")+
  theme_minimal()+
  theme(axis.text.x =element_text(size = 8),
        axis.title.x =element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_text(size = 8),
        plot.background = element_rect(fill = 'white'),
        legend.position="none")

a=Esp+annotation_custom(grob = ggplotGrob(Can), xmin = -15, xmax = -8, ymin = 34.25, ymax = 38)+
  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3, "in"),
                         width = unit(0.3, "in")) +
  ggtitle(unique(x$scientificname)) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x =element_text(size = 4),
        axis.title.x =element_text(size = 5),
        axis.title.y =element_text(size = 5),
        axis.text.y=element_text(size = 4),
        plot.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey10",size = 0.05),
        legend.position="none")
print(a)
}


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
        # mapa=mapview(temp2[which(!is.na(temp2$Latitud)&!is.na(temp2$Longitud)),], xcol = "Longitud", ycol = "Latitud", crs = 4326, map.types = "CartoDB.Positron", grid = F)
        # mapa=mapa+mapview(demarcaciones, zcol = "Demarcaciones", col.regions = brewer.pal(5, "Dark2"), alpha.regions = 0.25 ,alpha=0,
        #          crs = 4326, grid = F)
        # print(mapa)
        }
      Guardar1 <- readline("Quieres guardar un mapa de la especie? si/no ")
      if (str_detect(Guardar1,regex("si|yes|s|y",ignore_case = T))) {
          #mapshot(mapa, url = paste(temp2[1,1],".html",sep=""))
          ggsave(paste("map_",temp2[1,1],".png",sep = ""), dpi = 500)
          cat(paste("Mapa guardado en ", Directorio , sep=""))
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
      # mapa=mapview(Tabla[which(!is.na(Tabla$Latitud)&!is.na(Tabla$Longitud)),], xcol = "Longitud", ycol = "Latitud", crs = 4326, map.types = "CartoDB.Positron", grid = F)
      # mapa=mapa+mapview(demarcaciones, zcol = "Demarcaciones", col.regions = brewer.pal(5, "Dark2"), alpha.regions = 0.25 ,alpha=0,
      #                   crs = 4326, grid = F)
      # print(mapa)
    }
    Guardar1 <- readline("Quieres guardar un mapa de la/s demarcacion/es? si/no ")
    if (str_detect(Guardar1,regex("si|yes|s|y",ignore_case = T))) {
        #mapshot(mapa, url = paste(toupper(as.character(Demarcacion)),".html",sep=""))
        ggsave(paste("map_",toupper(as.character(Demarcacion)),".png",sep = ""), dpi = 500)
        cat(paste("Mapa guardado en ", Directorio , sep=""))
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









