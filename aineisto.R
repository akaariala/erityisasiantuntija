# Aineiston lataaminen ja muokkaus ----------------------------------------
# 19.9.2021

setwd("~/GitHub/erityisasiantuntija/erityisasiantuntija")

library(tidyverse)
library(lubridate)
library(pxweb)



# Ladataan aineisto TK:n rajapinnasta -------------------------------------

# pxweb_get komennolle annettava kysely

vuodet <- as.character(c(2000:(year(Sys.Date()) - 1))) # poimittavat vuodet

query_vaesto <- 
  list("Alue" = c("*"),
       "Ikä" = c("*"),
       "Sukupuoli" = c("SSS"),
       "Vuosi" = vuodet,
       "Tiedot" = c("vaesto"))

# aineiston lataaminen

px_data <- 
  pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_11re.px",
            query = query_vaesto)

px_tibble <- as.data.frame(px_data, 
                           column.name.type = "text", 
                           variable.value.type = "text") %>% as_tibble()



# Aineiston muokkaus ------------------------------------------------------

# Maakuntatiedon liittämiseen tarvittava kunta-maakunta-avain vuodelta 2021
# löytyy Tilastokeskuksen sivustolta:
# https://www2.stat.fi/fi/luokitukset/corrmaps/kunta_1_20210101%23maakunta_1_20210101/
# Kunta-maakunta-datasta tarvitaan kahta muuttujaa:
# kuntaa(alue_kunta) ja maakuntaa (alue_mk)

kunta_maakunta <- read.csv("kunta_maakunta_2021.csv", sep = ";") %>% as_tibble()

# tarkastelun ikäryhmät 

ika_labs1 <- c(paste0("0\u2013", "14 v."),
              paste0("15\u2013", "17 v."), 
              paste0("18\u2013", "20 v."), 
              "Yli 20 v.")

ika_labs2 <- c(paste0("0\u2013", "11 v."),
              paste0("12\u2013", "14 v."),
              paste0("15\u2013", "17 v."), 
              paste0("18\u2013", "20 v."), 
              "Yli 20 v.")

# aineiston kasaaminen ja muokkaus

vaesto_data <- px_tibble %>%
  filter(Ikä != "Yhteensä") %>% 
  rename(alue_kunta = Alue) %>% 
  left_join(kunta_maakunta, by = "alue_kunta") %>% 
  rename(vaesto = `Väestö 31.12.`, 
         maakunta = alue_mk,
         ika = Ikä,
         vuosi = Vuosi) %>% 
  mutate(ika = str_replace(ika, " -", "")) %>% 
  mutate(vuosi = as.integer(vuosi), 
         ika = as.integer(ika),
         maakunta = as.factor(maakunta)) %>% 
  count(maakunta, vuosi, ika, wt = vaesto, name = "vaesto") %>% 
  mutate(ika_lk1 = cut(ika, 
                      breaks = c(min(ika), 14, 17, 20, max(ika)), 
                      include.lowest = TRUE,
                      labels = ika_labs1)) %>% 
  mutate(ika_lk2 = cut(ika, 
                      breaks = c(min(ika), 11, 14, 17, 20, max(ika)), 
                      include.lowest = TRUE,
                      labels = ika_labs2)) %>% 
  arrange(maakunta, vuosi, ika) 

saveRDS(vaesto_data, file = "vaesto_data.rds")



