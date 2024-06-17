########
library(readxl)
library(plotly)
library(stringr)
library(dplyr)
library(data.table)
library(lubridate)
library(qwraps2)
library(tidyr)
library(shinythemes)
file.name<-"Maquines"
Schau<-read_xlsx(paste0(here::here(),"/Data/",file.name,".xlsx"),sheet = "TOTAL Schauer")
Nedap<-read_xlsx(paste0(here::here(),"/Data/",file.name,".xlsx"),sheet = "TOTAL Nedap")
Exafan<-read_xlsx(paste0(here::here(),"/Data/",file.name,".xlsx"),sheet = "Consum TOTAL EXAFAN")
Rotec<-read_xlsx(paste0(here::here(),"/Data/",file.name,".xlsx"),sheet = "Consumo TOTAL Rotecna")
Exafan.peso<-read_xlsx(paste0(here::here(),"/Data/",file.name,".xlsx"),sheet = "Peso TOTAL EXAFAN")

Schau<-Schau[!is.na(Schau$`Transponder-UID`),]
if("...8" %in% colnames(Schau)){
  Schau$...8<-NULL
  Schau$...11<-NULL
}
Schau$Fecha<-as.Date(Schau$Fecha)
names(Schau)[c(2,9,10,11,13,7,8)]<-c("Corral","Duracion","Consumido","Peso","Animal","entrada","salida")
Schau$Animal<-as.character(as.numeric(Schau$Animal)%%1000)
Schau<-Schau[!Schau$Consumido>5000,]
Schau$Duracion[Schau$Duracion==0]<-1
Schau$Maquina<-"Schauer"

Nedap$Fecha<-as.Date(Nedap$visit_time)
names(Nedap)[c(1,4,6,9,8,5)]<-c("Animal","Corral","Duracion","Consumido","Peso","entrada")
Nedap$salida<-Nedap$entrada+Nedap$Duracion
Nedap$Animal<-as.character(as.numeric(Nedap$Animal)%%1000)
Nedap$Maquina<-"Nedap"

Exafan$time_OUT <- as.POSIXct(Exafan$time_OUT, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
Exafan$time_IN <- as.POSIXct(Exafan$time_IN, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
Exafan$Consumido<-rowSums(Exafan[,9:12])
Exafan$time_OUT <- ifelse(Exafan$time_OUT < as.POSIXct("2010-01-01", tz="GMT"), Exafan$time_IN, Exafan$time_OUT)
Exafan$time_IN <- ifelse(Exafan$time_IN < as.POSIXct("2010-01-01", tz="GMT"), Exafan$time_OUT, Exafan$time_IN)
Exafan$time_OUT <- as.POSIXct(Exafan$time_OUT, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
Exafan$time_IN <- as.POSIXct(Exafan$time_IN, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
Exafan$Fecha<-as.Date(Exafan$time_OUT) 
Exafan$Duracion<-as.numeric(Exafan$time_OUT- Exafan$time_IN)
Exafan$Duracion[Exafan$Duracion==0]<-1
names(Exafan)[c(3,4,6,5)]<-c("entrada","salida","Animal","Corral")
Exafan$Corral<-as.numeric(substr(Exafan$Corral,6,7)) 
Exafan$Animal<-as.character(as.numeric(Exafan$Animal)%%1000)
Exafan$Maquina<-"Exafan"
Exafan$Peso<-NA

Exafan$Id.corral<-paste0(Exafan$Maquina,"-",Exafan$Corral)
Nedap$Id.corral<-paste0(Nedap$Maquina,"-",Nedap$Corral)
Schau$Id.corral<-paste0(Schau$Maquina,"-",Schau$Corral)

Exafan.peso$Animal<-as.character(as.numeric(Exafan.peso$ID_tag)%%1000)
Exafan.peso$Corral<-as.numeric(substr(Exafan.peso$feeder,6,7)) 
names(Exafan.peso)[c(1,7)]<-c("Fecha","Peso")
#merge(Exafan,Exafan.peso,by=c("Animal","Fecha"))


Schau.tot<-Schau[,c("Animal","Corral","Fecha","Maquina","entrada","salida","Duracion","Consumido","Peso")]
Exafan.tot<-Exafan[,c("Animal","Corral","Fecha","Maquina","entrada","salida","Duracion","Consumido","Peso")]
Nedap.tot<-Nedap[,c("Animal","Corral","Fecha","Maquina","entrada","salida","Duracion","Consumido","Peso")]

maquines.tot<-rbind(Schau.tot,Exafan.tot,Nedap.tot)
maquines.tot$Id.corral<-paste0(maquines.tot$Maquina,"-",maquines.tot$Corral)

maquines.tot$Consumido<-maquines.tot$Consumido/1000
Exafan.peso$Maquina<-"Exafan"
Exafan.peso$Id.corral<-paste0(Exafan.peso$Maquina,"-",Exafan.peso$Corral)
Schau$Id.corral<-paste0(Schau$Maquina,"-",Schau$Corral)
Nedap$Id.corral<-paste0(Nedap$Maquina,"-",Nedap$Corral)
Schau2<-subset(Schau,Schau$Peso>=10)
Exafan.peso<-subset(Exafan.peso,Exafan.peso$Peso>=10)
Nedap2<-subset(Nedap,Nedap$Peso>=10)
Peso.e<-aggregate(Peso~Animal+Id.corral+Maquina+Fecha,Exafan.peso,mean)
Peso.s<-aggregate(Peso~Animal+Id.corral+Maquina+Fecha,Schau2,mean)
Peso.s$Peso<-Peso.s$Peso/1000
Peso.n<-aggregate(Peso~Animal+Id.corral+Maquina+Fecha,Nedap2,mean)
Peso.n$Peso<-Peso.n$Peso/1000
Pesos<-rbind(Peso.e,Peso.s,Peso.n)

consumo.dia<-aggregate(Consumido~Animal+Fecha+Id.corral+Maquina,data = maquines.tot,sum)
visit.dia<-aggregate(Consumido~Animal+Fecha+Id.corral+Maquina,data = maquines.tot,length)
maquines.totm<-subset(maquines.tot,maquines.tot$Consumido>0)
meal.dia<-aggregate(Consumido~Animal+Fecha+Id.corral+Maquina,data = maquines.totm,length)
Exat0<-subset(maquines.tot,Duracion>0) #filtratge per elimar els temps 0 que son les dades erronies
time.day<-aggregate(Duracion~Animal+Fecha+Id.corral+Maquina,data = Exat0,sum)
time.day$Duracion<-time.day$Duracion/60
Exafanfr<-subset(Exat0,Exat0$Consumido>0)
Exafanfr$Duracion<-Exafanfr$Duracion/60
Exafanfr$FR<-Exafanfr$Consumido/Exafanfr$Duracion
Pesos2<-aggregate(Peso~Animal+Fecha+Id.corral+Maquina,Pesos,mean)
datos_filtradosmin <- Pesos2 %>%
  group_by(Animal) %>%
  arrange(Fecha) %>%
  slice(1)
datos_filtradosmax <- Pesos2 %>%
  group_by(Animal) %>%
  arrange(desc(Fecha)) %>%
  slice(1)
data <- Pesos %>%
  mutate(Fecha = as.Date(Fecha)) %>%
  group_by(Animal) %>%
  arrange(Fecha)
first_entry <- data %>% slice(1)
last_entry <- data %>% slice(n())
last_entry$GMD <- (last_entry$Peso - first_entry$Peso) / as.numeric(difftime(last_entry$Fecha, first_entry$Fecha, units = "days"))
maquines.tot$Hour<-as.numeric(hour(maquines.tot$entrada))
consu.h<-aggregate(Consumido~Animal+Id.corral+Maquina+Fecha+Hour,data = maquines.tot,sum)
hora.max <- consu.h %>%
  group_by(Animal, Fecha) %>%
  filter(Consumido == max(Consumido)) %>%
  ungroup()
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Consu.tot<-aggregate(Consumido~Animal+Id.corral+Maquina,data = consumo.dia,sum)
Consu.mean<-aggregate(Consumido~Animal+Id.corral+Maquina,data = consumo.dia,mean)
pesi <- aggregate(Peso~Animal+Id.corral+Maquina, datos_filtradosmin, min)
pesf <- aggregate(Peso~Animal+Id.corral+Maquina, datos_filtradosmax, max)
GMD <- aggregate(GMD~Animal+Id.corral+Maquina, last_entry, mean)
TV<-aggregate(Consumido~Animal+Id.corral+Maquina,data = visit.dia,mean)
TM<-aggregate(Consumido~Animal+Id.corral+Maquina,data = meal.dia,mean)
TD<-aggregate(Duracion~Animal+Id.corral+Maquina,data = time.day,mean)
VS<-aggregate(Consumido~Animal+Id.corral+Maquina,data = maquines.tot,mean)
MS<-aggregate(Consumido~Animal+Id.corral+Maquina,data = maquines.totm,mean)
FR<-aggregate(FR~Animal+Id.corral+Maquina,data = Exafanfr,mean)


resultado <- merge(Consu.mean, Consu.tot, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".men",".tot"))
resultado <- merge(resultado, pesi, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".a",".b"))
resultado <- merge(resultado, pesf, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".c",".d"))
resultado <- merge(resultado, GMD, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".e",".f"))
resultado <- merge(resultado, TV, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".g",".h"))
resultado <- merge(resultado, TM, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".i",".j"))
resultado <- merge(resultado, TD, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".t",".r"))
resultado <- merge(resultado, VS, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".q",".z"))
resultado <- merge(resultado, MS, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".m",".x"))
resultado <- merge(resultado, FR, by = c("Animal", "Id.corral", "Maquina"),suffixes = c(".y",".u"))

names(resultado)[4:14]<-c("consumo.dia","consumo.tot","peso.ini","peso.fin","GMD","TV","TM","TD","VS","MS","FR")
resultado$ganancia<-resultado$peso.fin-resultado$peso.ini
resultado$IC<-resultado$consumo.dia/resultado$GMD

morts<-c(427,196,497,508,393)
fin<-resultado[!resultado$Animal%in%morts,]
fin$consumo.dia<-fin$consumo.dia*1000
fin$GMD<-fin$GMD*1000
fin$VS<-fin$VS*1000
fin$MS<-fin$MS*1000
fin$FR<-fin$FR*1000