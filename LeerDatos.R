# El 16-4 cumplimos un mes. 
# Post que estaria genial es uno con resumen de cuanta gente lleno el form de pre'inscripcion, 
# de donde son (cantidad de ciudades, provincias, paises)
# a cuantos dimos cursos y a cuantos daremos... un 
# resumen sencillito, pero poderoso para publicar al mes y 
# contando que arrancamos el 16, pero recien el 23 abrimos el formulario y recien el ... dimos el primer curso.


library(tidyverse)
library(readxl)

Encuesta_fin_cursos <- read_excel("Encuesta_fin_cursos.xlsx",
                                  skip = 1,
                                  col_names = c("marca_temporal",
                                                "positivo",
                                                "a_mejorar"))


Pre_Inscripciones <- read_excel("Pre-Inscripciones.xlsx", 
                                sheet = "Respuestas de formulario 1", 
                                skip = 1,
                                col_names = c("marca_temporal",
                                              "email",
                                              "nombre_apellido",
                                              "institucion",
                                              "pais",
                                              "provincia_mayor_cantidad_horas_clase",
                                              "nivel_educativo",
                                              "da_clase",
                                              "anios_dando_clase",
                                              "gestion",
                                              "creencias_docencia",
                                              "a_cargo_clase",
                                              "inicio_clase",
                                              "cantidad_estudiantes",
                                              "disciplinas_docencia",
                                              "celular",
                                              "tablet",
                                              "laptop_computadora_portatil",
                                              "google_drive",
                                              "google_forms",
                                              "google_docs",
                                              "whatsapp",
                                              "telegram",
                                              "slack",
                                              "campus_virtual_moodle",
                                              "google_classroom",
                                              "zoom",
                                              "skype",
                                              "plataforma_comunicación_online",
                                              "facebook",
                                              "twitter",
                                              "instagram",
                                              "youtube",
                                              "acceso_internet",
                                              "barrera_tecnológica",
                                              "habilidad_teclado_compu",
                                              "discapacidad_impedimento",
                                              "cursos_selecionados",
                                              "franja_horaria",
                                              "interes_otros_cursos",
                                              "curso_para_docencia_online_otro_tema",
                                              "contribuir_desarrollo_cursos",
                                              "noticias",
                                              "otra_info",
                                              "semana_maniana",
                                              "semana_tarde",
                                              "sabado_maniana",
                                              "sabado_tarde",
                                              "otro",
                                              "mail_pra_el_curso_del",
                                              "mail_enviado_para_el_curso_del",
                                              "aviso_que_no_asiste",
                                              "asistio",
                                              "no_asistio_razon",
                                              "mailchimp"
))



# Es necesario limpiar las columnas:

# 1) Pais: ver si a partir del dato en la columna de provincia podemos completar a que país pertenecen, 
# si es NA vamos a asumir que es Argentina, ya que al inicio el formulario no tenia el país como dato

# 2) Provincia: terminado el arreglo del pais borrar de la provincia aquellas que no pertenezcan a Argentina

# 3) Se deben transformar las columnas que tienen mas de una opción como respuesta.  Buena excusa para usar pivot_longer y pivot_wider


Paises <- Pre_Inscripciones %>%
  group_by(pais) %>%
  summarise(cantidad = n()) %>%
  select(pais, cantidad)%>%
  arrange(desc(cantidad))

Ciudades <- Pre_Inscripciones %>%
  group_by(provincia_mayor_cantidad_horas_clase) %>%
  summarise(cantidad = n()) %>%
  select(provincia_mayor_cantidad_horas_clase, cantidad) %>%
  arrange(desc(cantidad))
  
alumnos <- Pre_Inscripciones %>%
  group_by(asistio) %>%
  summarise(cantidad = n()) 

fechas <- Pre_Inscripciones %>%
  group_by(mail_enviado_para_el_curso_del) %>%
  summarise(cantidad = n())    
