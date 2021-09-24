#Ejemplo: Data mining sobre datos publicados por organismos gubernamentales
#Instalacion de paquetes
#install.packages("sqldf", dep = TRUE)
#install.packages("ggplot2", dep = TRUE)

#Si hay algun error con la libreria sqldf
options(sqldf.driver = "SQLite") 
#options(gsubfn.engine = "R") 

library(sqldf)
library(ggplot2)

#Descargo el csv: todos los casos registrados de covid19 en argentina
#https://datos.gob.ar/dataset/salud-covid-19-casos-registrados-republica-argentina

#Leo el csv y lo guardo en un dataframe
covid_arg <- read.csv("C:/Users/COHEN/Desktop/Facultad/AnalisisNumerico/Semana 4/Covid19Casos.csv", encoding = "UTF-8")
head(covid_arg)
table(covid_arg$clasificacion_resumen)


sqldf()
covid_confirmados <- sqldf("SELECT *
                    FROM covid_arg 
                    where clasificacion_resumen = 'Confirmado'")


#A traves de consultas SQL se pueden filtrar y agrupar los datos
#Consulta 1: Cantidad de confirmados por dia

sqldf()
casos_por_dia <- sqldf("SELECT fecha_apertura as Fecha, count(1) as Cantidad 
                          FROM covid_confirmados
                          group by fecha_apertura")

#Grafico Cantidad de confirmados por dia
graf_por_dia<-ggplot(data=casos_por_dia, aes(x=Fecha, y=Cantidad)) +
  geom_bar(stat="identity",fill="#f70388") 
graf_por_dia

#Consulta 2: Cantidad de confirmados y fallecidos por grupo etario
sqldf()
casos_por_grupo <- sqldf( "SELECT CASE WHEN edad_años_meses = 'Meses' OR (edad_años_meses = 'Años' AND EDAD<10) THEN '0 A 9' 
                          WHEN EDAD<20 THEN '10 A 19' 
                          WHEN EDAD<30 THEN '20 A 29'
                          WHEN EDAD<40 THEN '30 A 39' 
                          WHEN EDAD<50 THEN '40 A 49'
                          WHEN EDAD<60 THEN '50 A 59' 
                          WHEN EDAD<70 THEN '60 A 69'
                          WHEN EDAD<80 THEN '70 A 79' 
                          WHEN EDAD<90 THEN '80 A 89' 
                          WHEN EDAD>=90 THEN '80+'
                          ELSE 'SIN DATOS' END Grupo,
                          COUNT(1) as Cantidad,
                          SUM(CASE WHEN FALLECIDO = 'SI' THEN 1 ELSE 0 END) Fallecidos
                          FROM covid_confirmados
                          WHERE edad is not null 
                          group by Grupo
                          order by 1")

#Grafico Cantidad de confirmados y fallecidos por grupo etario
graf_cant_edad<-ggplot(data=casos_por_grupo, aes(x=Grupo, y=Cantidad,fill=Grupo, group =1)) +
  geom_bar(stat="identity") +
  geom_line(aes(x=Grupo, y=Fallecidos),stat="identity")+
  geom_text(aes(label=Cantidad), vjust=1.6, color="white", size=3.5)
graf_cant_edad

#Grafico Cantidad de fallecidos por grupo etario
graf_fallecidos<-ggplot(data=casos_por_grupo, aes(x=Grupo, y=Fallecidos,fill=Grupo)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Fallecidos), vjust=1.6, color="black", size=3.5)
graf_fallecidos

#Consulta 3: Cantidad de confirmados y fallecidos por provincia (de las 15 prov con mas casos)
sqldf()
casos_por_provincia <- sqldf( "SELECT carga_provincia_nombre as Provincia,
                          COUNT(1) as Cantidad,
                          SUM(CASE WHEN FALLECIDO = 'SI' THEN 1 ELSE 0 END) Fallecidos,
                          carga_provincia_nombre || ' - ' || COUNT(1) as Confirmados
                          FROM covid_confirmados
                          group by Provincia
                          order by Cantidad desc
                          limit 15")

#Grafico Cantidad de confirmados por provincia (de las 15 prov con mas casos)
graf_prov<-ggplot(data=casos_por_provincia, aes(x=reorder(Provincia, -Cantidad), y=Cantidad,fill=Confirmados)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Cantidad), vjust=1.8, color="black", size=3.5)
graf_prov

#Grafico Cantidad de fallecidos por provincia (de las 15 prov con mas casos)
graf_prov2<-ggplot(data=casos_por_provincia, aes(x=reorder(Provincia, -Fallecidos), y=Fallecidos,fill=Provincia)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Fallecidos), vjust=1.8, color="black", size=3.5)
graf_prov2