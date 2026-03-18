#install.packages("openxlsx")

#libreria para manipulación de datos
#install.packages("dplyr")

#install.packages("tidyverse")


library("dplyr")
library("openxlsx")
library("tools")

library(stringr)
library(stringi)

#Cargar la base de datos (xlsx) a una variable
xlsx_prueba <- read.xlsx("data/Base para prueba.xlsx")

# convertimos xlsx_prueba a un dataframa para poder hacer uso del manejo de datos
df<- data.frame(xlsx_prueba)

# str para conocer la estructura del dataset, también ayuda hacer una revisión visual directa el arcivho xlsx
str(df)

#Descripción de las variables o campos, para tenerlos a la vista

names(df)
#[1] "Fuente"           "Nombre"           "Primer.apellido"  "Segundo.apellido"
#[5] "Email"            "asistencia_ses1"  "asistencia_ses2"  "asistencia_ses3" 
#[9] "asistencia_ses4"  "asistencia_ses5"

#Se realiza el conteo de valores nulos de las variables del df

nulos_df= df%>% summarise_all(~sum(is.na(.)))
print(nulos_df) 
#Fuente Nombre Primer.apellido Segundo.apellido Email asistencia_ses1 asistencia_ses2
#1      0      0               0                0     0              12              10
#asistencia_ses3 asistencia_ses4 asistencia_ses5
#1               5              12              10


#eliminar los posibles espacio que estan al inicio y final del nombre

df<- df%>%mutate("Nombre"= trimws(Nombre))
df<- df%>%mutate("Primer.apellido"= trimws(Primer.apellido))
df<- df%>%mutate("Segundo.apellido"= trimws(Segundo.apellido))
df<- df%>%mutate("Email"= trimws(Email))


#Concatenar columnas
# nombre+Primer.apellido+Segundo.apellido

#df<- df%>%mutate("N_completo"=paste(Nombre,Primer.apellido,Segundo.apellido))

df <- df %>%
  mutate(
    N_completo = paste(Nombre, Primer.apellido, Segundo.apellido) %>%
      str_squish() %>%                              # limpia espacios extra
      str_to_lower() %>%                            # todo a minúsculas
      stringi::stri_trans_general("Latin-ASCII") %>%# quita acentos
      str_to_title()                                # Title Case
  )


# cambiar NA por ceros, para evitar sesgos en el conteo de participación
df[is.na(df)]<-0

#Se crea un dataframe final que agrupa por email, para evitar duplicados, dado que un participante puede 
#aparecer en más de una fuente, summarise ayuda a dejar las columnas que realemnte se usarán para el calculo final

df_final <- df %>%
  group_by(Email) %>%
  summarise(
    N_completo = first(N_completo),
    
    #como los participantes puden estar en más de una base, se sintetiza la asistencia por sesión, 
    #se busca obtener el máximo valor (1) para el posterior conteo de las asistencias
    asistencia_ses1 = max(asistencia_ses1),
    asistencia_ses2 = max(asistencia_ses2),
    asistencia_ses3 = max(asistencia_ses3),
    asistencia_ses4 = max(asistencia_ses4),
    asistencia_ses5 = max(asistencia_ses5)
  )

#En este punto ya se puede hacer una columna extra asistencia_total= Asistencia_ses1+...+Asistencia_ses5

df_final<-df_final%>% mutate(Asistencia_total=
                   asistencia_ses1+
                   asistencia_ses2+
                   asistencia_ses3+
                   asistencia_ses4+
                   asistencia_ses5
                   )

#ordenar los datos por Asistencia_total de menor a mayor, como ayuda visual

df_final<-df_final %>% arrange(Asistencia_total)

#Respondiendo a la pregunta del peor desempeño (menor cantidad de asistencias)

peor_desempeno <- df_final %>%
  filter(Asistencia_total == min(Asistencia_total)) %>%
  select(Email, N_completo, Asistencia_total)

print((peor_desempeno))

# es el total de personas con el peor desempeño
n_peor_desempeno= nrow(peor_desempeno)
print(n_peor_desempeno)  #126

#crear archivos finales para presentar
write.csv(df_final, "output/dataset_final.csv", row.names = FALSE)

#un archivo particular con los participantes con el peor desempeño 
write.csv(peor_desempeno, "output/peor_desempeno.csv", row.names = FALSE)



#nrow(df_final)
#any(duplicated(df_final$Email))
