library(purrr)
library(dplyr)
library(tidyr)
library(wordcloud2)
library(tidytext)
library(cld2)
library(cld3)
library(lubridate)


#preparo un listado de stopwords en varios idiomas para sacarlos de la nube de palabras
# No tengo idea si esto se puede hacer de otra manera mÃ¡s sencilla, pero funciona

idiomas <- list('spanish', 'portuguese', 'french', 'danish', 'dutch', 'finnish', 'german', 'hungarian', 'italian', 'norwegian', 'russian', 'swedish')

variosIdiomas_stop_words <- idiomas %>% 
  map(tm::stopwords) %>%
  flatten_dfc() %>% 
  gather(var, word) %>%
  bind_rows(stop_words) %>%
  select(word, lexicon)


names <- Encuesta_fin_cursos %>% 
  select(positivo) %>%
  flatten_dfc() %>% 
  gather(variable, titulo) %>%
  unnest_tokens(word, titulo) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>2)

wordcloud2(names, size = 1, minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-light", backgroundColor = "grey")


names_amejorar <- Encuesta_fin_cursos %>% 
  select(a_mejorar) %>%
  flatten_dfc() %>% 
  gather(variable, titulo) %>%
  unnest_tokens(word, titulo) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>2)


wordcloud2(names_amejorar, size = 1, minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-light", backgroundColor = "grey")



#intento de forma de R (funciona instalando el paquete desde github y no desde CRAN)

names <- nombres[!is_ok] %>% 
  flatten_dfc() %>% 
  gather(variable, titulo) %>%
  unnest_tokens(word, titulo) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>9)

#Quiero una paleta de colores personalizada
#Aqui tengo los cuatro colores de base
colorlist <- c('#f7e4be', '#f0f4bc', '#9a80a4', '#848da6')

#Tengo que repetirlos por la cantidad de palabras que se deben graficar

colores <- rep(list(colorlist), 68) #A manopla para probar. TODO: realizar el calculo por la cantida de palabras
colorlist <- unlist(colores)

letterCloud(names,  word = "M", color='random-light', backgroundColor="#223564")

letterCloud(names,  word = "MetaDocencia", color=colorlist, backgroundColor="#223564")


wordcloud2(names, figPath = "rlogo1.png", size = 0.4, color = "skyblue")
