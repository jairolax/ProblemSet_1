## PROBLEM SET 1 
## Punto 1

# Obtención de los datos

rm(list = ls())
require(pacman)
p_load(tidyverse)
p_load(rvest)
p_load(rio, tidyverse, skimr, caret) 

geih1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>%
  html_table()
base1 <- as.data.frame(geih1)

geih2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html") %>%
  html_table()
base2 <- as.data.frame(geih2)

geih3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html") %>%
  html_table()
base3 <- as.data.frame(geih3)

geih4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html") %>%
  html_table()
base4 <- as.data.frame(geih4)

geih5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html") %>%
  html_table()
base5 <- as.data.frame(geih5)

geih6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html") %>%
  html_table()
base6 <- as.data.frame(geih6)

geih7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html") %>%
  html_table()
base7 <- as.data.frame(geih7)

geih8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html") %>%
  html_table()
base8 <- as.data.frame(geih8)

geih9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html") %>%
  html_table()
base9 <- as.data.frame(geih9)

geih10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html") %>%
  html_table()
base10 <- as.data.frame(geih10)

#Creación base de datos conjunta

geih_fil <- do.call("rbind", list(base1, base2, base3, base4, base5, base6, base7, base8, base9, base10))
