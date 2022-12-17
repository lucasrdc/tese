library(sf)
library(raster)
library(geosphere)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggspatial)
library(purrr)
library(sp)
library(RColorBrewer)
library(ggpubr)
library(geobr)
library(cowplot)
library(rcartocolor)

options(scipen=999)

base <- readRDS("resultado/dados.RDS")

uf <- read_state()
mg <- read_state(code_state = 31)
seat <- read_municipal_seat() %>% 
  filter(code_muni == 3149309)
quadrado = st_as_sfc(st_bbox(mg))

shp <- read_sf("../dados/shape/pl.shp") %>% 
  mutate(`Census sectors` = ifelse(TIPO == "RURAL","Rural (19)","Urban (63)"))

# plotando uns mapinhas ---------------------------------------------------

# distrito

ggm1 <- ggplot() + 
  geom_sf(data = uf, fill = "white") + 
  geom_sf(data = quadrado, fill = NA, color = "red", size = 1.2) +
  geom_sf(data = seat, color = "red", size = 2) + 
  theme_void()

ggm2 <- shp %>% 
  rename(Distrito = NM_DISTRIT) %>% 
  mutate(Distrito = case_when(Distrito == "DOUTOR LUND" ~ "Doutor Lund",
                              Distrito == "FIDALGO" ~ "Fidalgo",
                              Distrito == "PEDRO LEOPOLDO" ~ "Pedro Leopoldo",
                              Distrito == "VERA CRUZ DE MINAS" ~ "Vera Cruz de Minas",
                              TRUE ~ "Lagoa de Santo Antônio")) %>% 
  ggplot(.) + 
  geom_sf(aes(fill = Distrito)) +
  theme_bw()

gg_inset_map1 = ggdraw() +
  draw_plot(ggm2) +
  draw_plot(ggm1, x = 0.684, y = 0.65, width = 0.3, height = 0.3)

ggsave(filename = "mapas/pedro_leopoldo.png", plot = gg_inset_map1, dpi = 300)

# liraa

shp_plot <- shp %>% 
  mutate(CD_GEOCODI = as.numeric(CD_GEOCODI)) %>% 
  full_join(base,by=c("CD_GEOCODI"="setor")) %>% 
  mutate(liraa = replace_na(liraa,"satisfatório")) %>% 
  filter(!is.na(periodo2)) %>% 
  rename(`1 semana` = sinan1,
         `2 semanas` = sinan2,
         `4 semanas` = sinan4,
         `6 semanas` = sinan6,
         `LIRAa` = liraa)

# LIRAa

paleta <- setNames(c("grey90","yellow","red"),                         
                   c("satisfatório","alerta","alto risco"))

p <- ggplot(shp_plot) +
  geom_sf(aes(fill = LIRAa), size = 0.08) +
  scale_fill_manual(values = paleta) +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/liraa.png", dpi = 300, width = 10, height = 8)

p <- ggplot(shp_plot) +
  geom_sf(aes(fill = LIRAa), size = 0.08) +
  scale_fill_manual(values = paleta) +
  coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/liraa_zoom.png", dpi = 300, width = 10, height = 8)

# SINAN

# 1 semana
p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `1 semana`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan1.png", dpi = 300, width = 10, height = 8)

p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `1 semana`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan1_zoom.png", dpi = 300, width = 10, height = 8)

# 2 semanas
p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `2 semanas`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan2.png", dpi = 300, width = 10, height = 8)

p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `2 semanas`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan2_zoom.png", dpi = 300, width = 10, height = 8)

# 4 semanas
p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `4 semanas`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan4.png", dpi = 300, width = 10, height = 8)

p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `4 semanas`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan4_zoom.png", dpi = 300, width = 10, height = 8)

# 6 semanas
p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `6 semanas`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan6.png", dpi = 300, width = 10, height = 8)

p <- ggplot(shp_plot) +
  geom_sf(aes(fill = `6 semanas`), size = 0.08) +
  scale_fill_gradient(low="cornsilk", high="darkred") +
  coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
  facet_wrap(.~periodo2) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ggsave("maps/sinan6_zoom.png", dpi = 300, width = 10, height = 8)

# juntos
for(i in 1:12) {
  shp1 <- shp_plot %>% 
    filter(periodo2 == i)
  
  p1 <- ggplot(shp1) +
    geom_sf(aes(fill = LIRAa), size = 0.08) +
    scale_fill_manual(values = paleta) +
    coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  
  shp1 %<>% 
    pivot_longer(`1 semana`:`6 semanas`,names_to="sinan",values_to="Casos") %>% 
    st_as_sf(.)
  
  p2 <- ggplot(shp1) +
    geom_sf(aes(fill = Casos), size = 0.08) +
    scale_fill_gradient(low="cornsilk", high="darkred") +
    coord_sf(xlim = c(-44.10, -43.98), ylim = c(-19.66, -19.55), expand = FALSE) +
    facet_wrap(.~sinan) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  
  p <- ggarrange(p1,p2)
  ggsave(paste0("maps/juntos_",i,".png"), dpi = 300)
}




sinan <- read_csv("../dados/sinan/sinan_fixed.csv") %>% 
  mutate(`Data da notificação` = ymd(dt_notific),
         classi_fin = as.integer(classi_fin)) %>% 
  filter(classi_fin != 5 & classi_fin != 8) %>% 
  group_by(`Semana epidemiológica` = floor_date(`Data da notificação`, "1 week")) %>% 
  summarise(`Casos confirmados` = n()) %>% 
  filter(!is.na(`Semana epidemiológica`))


ggplot(sinan,) +
  geom_line(aes(x = `Semana epidemiológica`, y = `Casos confirmados`)) +
  geom_vline(xintercept = as_date("2015-01-12"):as_date("2015-01-15"), color = 'red') +
  geom_vline(xintercept = as_date("2015-01-15"):(as_date("2015-01-15") + weeks(2)), color = 'pink', alpha = .2) +
  geom_vline(xintercept = (as_date("2015-01-15") + weeks(2)):(as_date("2015-01-15") + weeks(4)), color = 'pink', alpha = .1) +
  geom_vline(xintercept = as_date("2015-03-03"):as_date("2015-03-06"), color = 'red') +
  geom_vline(xintercept = as_date("2015-03-06"):(as_date("2015-03-06") + weeks(2)), color = 'pink', alpha = .2) +
  geom_vline(xintercept = (as_date("2015-03-06") + weeks(2)):(as_date("2015-03-06") + weeks(4)), color = 'pink', alpha = .1) +
  geom_vline(xintercept = as_date("2015-10-19"):as_date("2015-10-23"), color = 'red') +
  geom_vline(xintercept = as_date("2015-10-23"):(as_date("2015-10-23") + weeks(2)), color = 'pink', alpha = .2) +
  geom_vline(xintercept = (as_date("2015-10-23") + weeks(2)):(as_date("2015-10-23") + weeks(4)), color = 'pink', alpha = .1) +
  geom_vline(xintercept = as_date("2016-06-18"):as_date("2016-06-20"), color = 'blue') +
  geom_vline(xintercept = as_date("2016-06-20"):(as_date("2016-06-20") + weeks(2)), color = 'cyan', alpha = .2) +
  geom_vline(xintercept = (as_date("2016-06-20") + weeks(2)):(as_date("2016-06-20") + weeks(4)), color = 'cyan', alpha = .1) +
  geom_vline(xintercept = as_date("2016-10-18"):as_date("2016-10-21"), color = 'blue') +
  geom_vline(xintercept = as_date("2016-10-21"):(as_date("2016-10-21") + weeks(2)), color = 'cyan', alpha = .2) +
  geom_vline(xintercept = (as_date("2016-10-21") + weeks(2)):(as_date("2016-10-21") + weeks(4)), color = 'cyan', alpha = .1) +
  geom_vline(xintercept = as_date("2017-01-11"):as_date("2017-01-13"), color = 'green') +
  geom_vline(xintercept = as_date("2017-01-13"):(as_date("2017-01-13") + weeks(2)), color = 'darkolivegreen1', alpha = .2) +
  geom_vline(xintercept = (as_date("2017-01-13") + weeks(2)):(as_date("2017-01-13") + weeks(4)), color = 'darkolivegreen1', alpha = .1) +
  geom_vline(xintercept = as_date("2017-03-06"):as_date("2017-03-10"), color = 'green') +
  geom_vline(xintercept = as_date("2017-03-10"):(as_date("2017-03-10") + weeks(2)), color = 'darkolivegreen1', alpha = .2) +
  geom_vline(xintercept = (as_date("2017-03-10") + weeks(2)):(as_date("2017-03-10") + weeks(4)), color = 'darkolivegreen1', alpha = .1) +
  geom_vline(xintercept = as_date("2017-10-16"):as_date("2017-10-20"), color = 'green') +
  geom_vline(xintercept = as_date("2017-10-20"):(as_date("2017-10-20") + weeks(2)), color = 'darkolivegreen1', alpha = .2) +
  geom_vline(xintercept = (as_date("2017-10-20") + weeks(2)):(as_date("2017-10-20") + weeks(4)), color = 'darkolivegreen1', alpha = .1) +
  geom_vline(xintercept = as_date("2018-01-08"):as_date("2018-01-10"), color = 'darkorange') +
  geom_vline(xintercept = as_date("2018-01-10"):(as_date("2018-01-10") + weeks(2)), color = 'darkgoldenrod1', alpha = .2) +
  geom_vline(xintercept = (as_date("2018-01-10") + weeks(2)):(as_date("2018-01-10") + weeks(4)), color = 'darkgoldenrod1', alpha = .1) +
  geom_vline(xintercept = as_date("2018-04-09"):as_date("2018-04-13"), color = 'darkorange') +
  geom_vline(xintercept = as_date("2018-04-13"):(as_date("2018-04-13") + weeks(2)), color = 'darkgoldenrod1', alpha = .2) +
  geom_vline(xintercept = (as_date("2018-04-13") + weeks(2)):(as_date("2018-04-13") + weeks(4)), color = 'darkgoldenrod1', alpha = .1) +
  geom_vline(xintercept = as_date("2018-08-06"):as_date("2018-08-09"), color = 'darkorange') +
  geom_vline(xintercept = as_date("2018-08-09"):(as_date("2018-08-09") + weeks(2)), color = 'darkgoldenrod1', alpha = .2) +
  geom_vline(xintercept = (as_date("2018-08-09") + weeks(2)):(as_date("2018-08-09") + weeks(4)), color = 'darkgoldenrod1', alpha = .1) +
  geom_vline(xintercept = as_date("2018-10-22"):as_date("2018-10-24"), color = 'darkorange') +
  geom_vline(xintercept = as_date("2018-10-24"):(as_date("2018-10-24") + weeks(2)), color = 'darkgoldenrod1', alpha = .2) +
  geom_vline(xintercept = (as_date("2018-10-24") + weeks(2)):(as_date("2018-10-24") + weeks(4)), color = 'darkgoldenrod1', alpha = .1) +
  theme_bw()

ggsave("graficos/casos_liraa.png", dpi = 300)


# vendo os zeros dos casos de dengue pra justificar o ZINB ----------------

base %>% 
  mutate(`até 2 semanas` = sinan1+sinan2,
         `3 a 4 semanas` = sinan3+sinan4) %>%
  select(periodo2, `até 2 semanas`,`3 a 4 semanas`) %>% 
  pivot_longer(!periodo2, values_to = "Casos", names_to = "Período") %>% 
  ggplot(., aes(x = `Casos`, fill = `Período`)) +
    geom_density(alpha = .5) +
    ylab("Densidade") +
    theme_bw() + 
    theme(legend.position = "bottom")

ggsave("graficos/zeros.png", dpi = 300)
