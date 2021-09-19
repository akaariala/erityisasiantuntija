# Valmistellaan esityksen kuviot ------------------------------------------
# 19.9.2021

setwd("~/GitHub/erityisasiantuntija/erityisasiantuntija")

library(tidyverse)
library(lubridate)

# aineiston lataaminen

vaesto_data <- read_rds("vaesto_data.rds")




# Kuviot ------------------------------------------------------------------

# kuvioiden yl. asetukset

lahde <- expression(paste(italic("Lähde:"), "Tilastokeskus"))
font_size <- 14
p_width <- 9
p_height <- 5



# Aluekuvio koko maan kaikista ikäryhmistä --------------------------------

p_kokovaesto <- vaesto_data %>% 
  count(maakunta, ika_lk1, vuosi, wt = vaesto, name = "vaesto") %>% 
  filter(maakunta == "Koko Suomi") %>% 
  ggplot(aes(x = vuosi, y = vaesto)) +
  geom_area(aes(fill = fct_rev(ika_lk1)), alpha = 0.6, color = "white")+
  theme_minimal() +
  theme(plot.margin = margin(1, 1, 1, 1, unit = 'pt'), 
        text = element_text(size = font_size)) +
  labs(x = NULL, 
       y = "Väestö, miljoonaa henkilöä",
       caption = lahde) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 5600000),
                     breaks = seq(1000000, 5600000, 1000000),
                     labels = scales::number_format(scale = 1 / 1000000, 
                                                    accuracy = 1, 
                                                    big.mark = " ")) +
  scale_fill_brewer(direction = -1, name = "Ikä:") 

ggsave("kokovaesto.png", width = p_width, height = p_height)



# Aluekuvio rikosvastuuikäisen väestön jakautumisesta maakunnittain -------

n_pop <- vaesto_data %>% 
  filter(maakunta == "Koko Suomi" & ika_lk1 != "0–14 v.") %>% 
  count(vuosi, vuosi, wt = vaesto, name = "n_pop")

vaesto_data %>% 
  filter(maakunta != "Koko Suomi" & ika_lk1 != "0–14 v.") %>% 
  count(maakunta, vuosi, wt = vaesto, name = "vaesto") %>% 
  left_join(n_pop, by = "vuosi") %>% 
  mutate(pop_osuus = vaesto / n_pop) %>%
  ggplot(aes(x = vuosi, y = pop_osuus)) +
  geom_area(aes(fill = fct_reorder(maakunta, vaesto)), 
            alpha = 0.7, color = "white")+
  theme_minimal() +
  theme(plot.margin = margin(1, 1, 1, 1, unit = 'pt'), 
        text = element_text(size = font_size),
        panel.grid.major = element_line(size = 1),
        legend.title = element_blank()) +
  labs(x = NULL, 
       y = "Osuus väestöstä, %",
       caption = lahde) +
  scale_y_continuous(expand = c(0, 0.01),
                     breaks = seq(0, 1, 0.2),
                     minor_breaks = seq(0, 1, 0.1),
                     labels = scales::percent_format(suffix = "")) +
  scale_fill_viridis(option="cividis", discrete = TRUE, direction = -1)

ggsave("mk_rvpopdist.png", width = p_width, height = p_height)



# Rikosvastuuikäisen väestön määrän muutokset maakunnittain ---------------

# Kuvioissa piirretään havainnot kolmelle vuodelle: 
# seurannan alkuvuodelle, puoliväliin ja loppuun 
# eli vuonna 2021 vuodet ovat: 2000, 2010, 2020

p_vuodet <- c(2000, 
              2000 + floor(((year(Sys.Date()) - 1) - 2000) / 2),
              year(Sys.Date()) - 1)

# rajataan maakuntakuvioiden aineisto

p_data <- vaesto_data %>% 
  filter(maakunta != "Koko Suomi" & vuosi %in% p_vuodet) %>% 
  count(maakunta, ika_lk1, vuosi, wt = vaesto, name = "vaesto") 

# koko rikosvastuuikäinen väestö eli 15 +

rvpop_data <- p_data %>% 
  filter(ika_lk1 != "0–14 v.") %>% 
  count(maakunta, vuosi, wt = vaesto)
  
mk_rvpop <- rvpop_data %>% 
  ggplot(aes(fct_reorder(maakunta, n), n)) +
  geom_point(aes(color = as_factor(vuosi)), size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(size = 1),
        legend.justification = c(1, 0),
        legend.position = c(1, 0.05),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(1, 15, 1, 1, unit = 'pt'),
        text = element_text(size = font_size)) +
  labs(x = NULL, y = NULL,
       caption = lahde) +
  scale_y_continuous(expand = c(0, 10000),
                     limits = c(0, max(rvpop_data$n) + 100000),
                     breaks = seq(0, max(rvpop_data$n) + 100000, 250000),
                     minor_breaks = seq(0, max(rvpop_data$n)+ 100000, 125000),
                     labels = scales::number_format(big.mark = " ")) +
  scale_color_brewer(palette = "BuPu")

ggsave("mk_rvpop.png", width = p_width, height = p_height)

# aikuiset eli yli 20 v 

aikuisten_data <- p_data %>%  
  filter(ika_lk1 == "Yli 20 v.") %>% 
  count(maakunta, vuosi, wt = vaesto) 

mk_aik <- aikuisten_data %>% 
  ggplot(aes(fct_reorder(maakunta, n), n)) +
  geom_point(aes(color = as_factor(vuosi)), size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(size = 1),
        legend.justification = c(1, 0),
        legend.position = c(1, 0.05),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(1, 15, 1, 1, unit = 'pt'),
        text = element_text(size = font_size)) +
  labs(x = NULL, y = NULL,
       caption = lahde) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(aikuisten_data$n + 100000)),
                     breaks = seq(0, max(aikuisten_data$n) + 100000, 250000),
                     minor_breaks = seq(0, max(aikuisten_data$n)+ 100000, 125000),
                     labels = scales::number_format(big.mark = " ")) +
  scale_color_brewer(palette = "BuPu")

ggsave("mk_aik.png", width = p_width, height = p_height)

# nuoret eli 15 - 20 v

nuorten_data <- p_data %>%
  filter(ika_lk1 %in% c("15–17 v.", "18–20 v.")) %>% 
  count(maakunta, vuosi, wt = vaesto) 

mk_nuoret <- nuorten_data %>% 
  ggplot(aes(fct_reorder(maakunta, n), n)) +
  geom_point(aes(color = as_factor(vuosi)), size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(size = 1),
        legend.justification = c(1, 0),
        legend.position = c(1, 0.05),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(1, 15, 1, 1, unit = 'pt'),
        text = element_text(size = font_size)) +
  labs(x = NULL, y = NULL,
       caption = lahde) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(nuorten_data$n) + 10000),
                     breaks = seq(0, max(nuorten_data$n) + 10000, 20000),
                     minor_breaks = seq(0, max(nuorten_data$n)+ 10000, 10000),
                     labels = scales::number_format(big.mark = " ")) +
  scale_color_brewer(palette = "BuPu")

ggsave("mk_nuoret.png", width = p_width, height = p_height)



# Ennuste 2023: alaikäisten rikosvastuuikäisten määrän muutos --------------

ennuste_data <- vaesto_data %>% 
  filter(vuosi == year(Sys.Date()) - 1) %>% 
  filter(maakunta != "Koko Suomi" & ika_lk2 %in% c("12–14 v.", "15–17 v.")) %>% 
  count(maakunta, ika_lk2, wt = vaesto)
  
ennuste <- ennuste_data %>% 
  ggplot(aes(fct_reorder(maakunta, n), n)) +
  geom_point(aes(color = as_factor(ika_lk2)), alpha = 0.7, size = 4) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(size = 1),
        legend.justification = c(1, 0),
        legend.position = c(1, 0.05),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(1, 15, 1, 1, unit = 'pt'),
        text = element_text(size = font_size)) +
  labs(x = NULL, y = NULL,
       caption = lahde) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(ennuste_data$n) + 5000),
                     breaks = seq(0, max(ennuste_data$n) + 10000, 10000),
                     minor_breaks = seq(0, max(ennuste_data$n)+ 10000, 5000),
                     labels = scales::number_format(big.mark = " ")) +
  scale_color_brewer(labels = c("Ennuste 2023", "2000"), 
                     guide = guide_legend(reverse = TRUE),
                     palette = "Set1") 

ggsave("ennuste.png", width = p_width, height = p_height)
