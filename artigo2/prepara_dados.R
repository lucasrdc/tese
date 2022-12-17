library(sf)
library(raster)
library(geosphere)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggspatial)
library(purrr)
library(sp)
library(haven)

options(scipen=999)

# Abrindo dados do CENSO 2010 (Domicílio e Entorno) -----------------------

domicilio <- read_csv2("../dados/censo/Domicilio01_MG.csv", na = "X") %>% 
  filter(Cod_setor >= 314930900000000 & Cod_setor <= 314930999999999)

entorno <- read_csv2("../dados/censo/Entorno01_MG.csv", na = "X") %>% 
  filter(Cod_setor >= 314930900000000 & Cod_setor <= 314930999999999)

basico <- read_csv2("../dados/censo/Basico_MG.csv")

# Calculando as proporções das variáveis de interesse (% esgoto, a --------

censo <- domicilio %>% 
  as_tibble() %>% 
  transmute(
  setor = Cod_setor,
  domicilio = V002,
  moradores = basico$V002,
  renda = basico$V005,
  alugados = V008/domicilio,
  agua = V012/domicilio,
  esgoto = V017/domicilio,
  lixo = V036/domicilio,
  energia = V044/domicilio,
  pavimentacao = (entorno$V014+entorno$V016+entorno$V018)/entorno$V001,
  arborizacao = (entorno$V044+entorno$V046+entorno$V048)/entorno$V001
)

rm(list=setdiff(ls(), "censo"))

# Abrindo o arquivo principal com os dados do SINAN -----------------------

sinan <- read_csv("../dados/sinan/sinan_fixed.csv") %>% 
  mutate(dt_notific = ymd(dt_notific),
         classi_fin = as.integer(classi_fin)) %>% 
  filter(!is.na(x) & !is.na(y)) %>% 
  filter(classi_fin != 5 & classi_fin != 8)

p <- sinan %>% 
  group_by(nu_ano) %>% 
  summarise(casos = n()) %>% 
  ggplot(., aes(x = as.factor(nu_ano), y = casos)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = casos), position = position_stack(vjust = .5), size = 5) +
  xlab("Ano") +
  ylab("Casos confirmados") +
  theme_bw()

ggsave("graficos/casos.png", dpi = 300)

shp <- read_sf("../dados/shape/pl.shp") %>% 
  mutate(`Census sectors` = ifelse(TIPO == "RURAL","Rural (19)","Urban (63)"))

# encontrando o setor de cada ponto ---------------------------------------

sinan %<>% left_join({st_as_sf(sinan, coords = c('x', 'y'), crs = st_crs(shp)) %>% 
    select(v1,geometry)},by="v1")

sinan %<>% mutate(intersection = as.integer(st_intersects(geometry, shp)),
                  setor = if_else(is.na(intersection), '', shp$CD_GEOCODI[intersection])) %>% 
  filter(!is.na(intersection)) %>% 
  mutate(setor = as.numeric(setor)) %>% 
  group_by(dt_notific, setor) %>% 
  summarise(n = n())

# abrindo os dados do LIRAa -----------------------------------------------

liraa_data <- tribble(~periodo2,~inicio, ~fim,
                      1,  "2015-01-12", "2015-01-15",
                      2,  "2015-03-03", "2015-03-06",
                      3,  "2015-10-19", "2015-10-23",
                      4,  "2016-07-18", "2016-07-20",
                      5,  "2016-10-18", "2016-10-21",
                      6,  "2017-01-11", "2017-01-13",
                      7,  "2017-03-06", "2017-03-10",
                      8,  "2017-10-16", "2017-10-20",
                      9,  "2018-01-08", "2018-01-10",
                      10, "2018-04-09", "2018-04-13",
                      11, "2018-08-06", "2018-08-09",
                      12, "2018-10-22", "2018-10-24") %>% 
  mutate(inicio = ymd(inicio),
         fim = ymd(fim))


liraa <- read_csv("../dados/liraa/liraa_sem_rep.csv") %>% 
  group_by(periodo2, cd_geocodi) %>% 
  summarise(positivo = sum(positivo),
            visitados = n(),
            a1 = sum(a1),
            a2 = sum(a2),
            b = sum(b),
            c = sum(c),
            d1 = sum(d1),
            d2 = sum(d2),
            e = sum(e)) %>% 
  left_join(liraa_data,by="periodo2") %>% 
  rename(setor = cd_geocodi)

# juntando todo mundo -----------------------------------------------------

shp %<>% mutate(CD_GEOCODI = as.numeric(CD_GEOCODI))

base <- censo %>% tidyr::expand(setor, 1:12) %>% 
  rename(periodo2 = `1:12`) %>% 
  left_join(censo,by="setor") %>% 
  arrange(periodo2) %>% 
  left_join(liraa,by=c('setor','periodo2')) %>% 
  left_join({shp %>% 
      st_drop_geometry() %>% 
      dplyr::select(CD_GEOCODI, long, lat, NM_DISTRIT)},by=c("setor"="CD_GEOCODI")) %>% 
  dplyr::select(-inicio,-fim) %>% 
  left_join(liraa_data, by="periodo2") %>% 
  mutate(across(renda:visitados, ~replace_na(.x,0))) %>% 
  mutate(iip = positivo/visitados*100,
         liraa = ifelse(iip < 1, "satisfatório",ifelse(iip>3.9,"alto risco","alerta")))

# calculando as lags dos casos --------------------------------------------

for(i in 1:nrow(base)) {
  cod_setor <- base$setor[i]
  comeco <- base$inicio[i]
  final <- base$fim[i]
  base$sinan1[i] <- sinan %>% 
    ungroup() %>% 
    dplyr::filter((dt_notific >= {comeco %m+% weeks(1)} & dt_notific <= {final %m+% weeks(2)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
  base$sinan10[i] <- sinan %>% 
    ungroup() %>% 
    dplyr::filter((dt_notific >= {comeco %m+% days(10)} & dt_notific <= {final %m+% days(17)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
  base$sinan2[i] <- sinan %>% 
    ungroup() %>% 
    filter((dt_notific >= {comeco %m+% weeks(2)} & dt_notific <= {final %m+% weeks(3)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
  base$sinan3[i] <- sinan %>% 
    ungroup() %>% 
    filter((dt_notific >= {comeco %m+% weeks(3)} & dt_notific <= {final %m+% weeks(4)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
  base$sinan4[i] <- sinan %>% 
    ungroup() %>% 
    filter((dt_notific >= {comeco %m+% weeks(4)} & dt_notific <= {final %m+% weeks(5)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
  base$sinan5[i] <- sinan %>% 
    ungroup() %>% 
    filter((dt_notific >= {comeco %m+% weeks(5)} & dt_notific <= {final %m+% weeks(6)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
  base$sinan6[i] <- sinan %>% 
    ungroup() %>% 
    filter((dt_notific >= {comeco %m+% weeks(6)} & dt_notific <= {final %m+% weeks(7)}) & setor == cod_setor) %>% 
    summarise(n = sum(n, na.rm = TRUE)) %>% 
    pull(n)
}

# Dados de chuva por ponto (CHIRPS) ---------------------------------------

base$chuva_antes <- NULL
for(i in 1:nrow(base)) {
  dt_notific <- base$inicio[i]
  days <- seq(dt_notific %m-% weeks(2),dt_notific, by='days') %>% 
    format("%Y.%m.%d")
  chuva <- 0
  for(day in days) {
    myras <- raster(paste0("../dados/chuva/",substr(day,0,4),"/chirps-v2.0.",day,".tif"))
    chuva <- chuva + raster::extract(x = myras, y = coordinates(base[i,22:23]),
                                     fun=mean,
                                     small = TRUE)
  }
  base$chuva_antes[i] = chuva/15
}

base$chuva_depois <- NULL
for(i in 1:nrow(base)) {
  dt_notific <- base$fim[i]
  days <- seq(dt_notific,dt_notific %m+% weeks(2), by='days') %>% 
    format("%Y.%m.%d")
  chuva <- 0
  for(day in days) {
    myras <- raster(paste0("../dados/chuva/",substr(day,0,4),"/chirps-v2.0.",day,".tif"))
    chuva <- chuva + raster::extract(x = myras, y = coordinates(base[i,22:23]),
                                     fun=mean,
                                     small = TRUE)
  }
  base$chuva_depois[i] = chuva/15
}

# Adicionando temperatura -------------------------------------------------

temp <- read_csv("../dados/temperatura/temperatura.csv") %>% 
  mutate(data = ymd(paste(ano,mes,dia,sep="-")))

base$temp_max_antes <- NULL
base$temp_gap_antes <- NULL
for(i in 1:nrow(base)) {
  dt_notific <- base$inicio[i]
  days <- seq(dt_notific %m-% weeks(2),dt_notific, by='days') 
  dif <- 0
  top <- 0
  for(day in days) {
    max <- temp %>% filter(data == day) %>% 
      pull(max)
    min <- temp %>% filter(data == day) %>% 
      pull(min)
    dif <- dif + (max-min)
    top <- top + max
  }
  base$temp_gap_antes[i] = dif/15
  base$temp_max_antes[i] = top/15
}

base$temp_max_depois <- NULL
base$temp_gap_depois <- NULL
for(i in 1:nrow(base)) {
  dt_notific <- base$inicio[i]
  days <- seq(dt_notific,dt_notific %m+% weeks(2), by='days') 
  dif <- 0
  top <- 0
  for(day in days) {
    max <- temp %>% filter(data == day) %>% 
      pull(max)
    min <- temp %>% filter(data == day) %>% 
      pull(min)
    dif <- dif + (max-min)
    top <- top + max
  }
  base$temp_gap_depois[i] = dif/15
  base$temp_max_depois[i] = top/15
}

# Adicionando o numero de focos em PE no setor ----------------------------

pe <- read_csv('../dados/pe/pe.csv') %>% 
  st_as_sf(., coords = c('x', 'y'), crs = st_crs(shp)) %>% 
  mutate(intersection = as.integer(st_intersects(geometry, shp)),
         setor = ifelse(is.na(intersection), NA, shp$CD_GEOCODI[intersection])) %>% 
  group_by(setor,ciclo) %>% 
  summarise(focos = sum(focos))

ciclos <- as_tibble(data.frame("periodo" = c(1:12),
                               "ano" = as.numeric(c("2015","2015","2015","2016","2016","2017","2017","2017","2018","2018","2018","2018")), 
                               "ciclo" = as.numeric(c("2","5","20","14","20","2","5","20","2","7","15","20"))))

base$pe <- NULL
base$pe2 <- NULL
base$pe_qtd <- NULL
for(i in 1:nrow(base)) {
  periodo <- base$periodo2[i]
  setores <- base$setor[i]
  focos <- pe %>% 
    filter((ciclo == periodo | ciclo == periodo-1) & setor == setores)
  if(nrow(focos) == 0) {
    base$pe[i] = 0
    base$pe2[i] = 0
    base$pe_qtd[i] = NA
  } else {
    base$pe[i] = 1
    base$pe2[i] = nrow(focos %>% 
                         group_by(setor) %>% 
                         summarise(n = n()))
    base$pe_qtd[i] <- focos %>% 
      ungroup() %>% 
      summarise(focos = sum(focos, na.rm = TRUE)) %>% 
      pull(focos)
  }
}

tmp <- shp %>% 
  mutate(intersection = st_touches(geometry, shp))

base$pe_vizinho <- NULL
base$pe2_vizinho <- NULL
base$pe_qtd_vizinho <- NULL
for(i in 1:nrow(base)) {
  setor <- base$setor[i]
  ciclos <- base$periodo2[i]
  vizinhos <- shp %>% slice(unlist(tmp %>% filter(CD_GEOCODI == setor) %>% pull(intersection))) %>% 
    pull(CD_GEOCODI)
  focos <- pe %>% filter(setor %in% vizinhos & (ciclo == ciclos | ciclo == ciclos -1))
  if(nrow(focos) == 0){
    base$pe_vizinho[i] = 0
    base$pe2_vizinho[i] = 0
    base$pe_qtd_vizinho[i] = NA
  } else {
    base$pe_vizinho[i] = 1
    base$pe2_vizinho[i] = nrow(focos %>% 
      group_by(setor) %>% 
      summarise(n = n()))
    base$pe_qtd_vizinho[i] <- focos %>% 
      ungroup() %>% 
      summarise(focos = sum(focos, na.rm = TRUE)) %>% 
      pull(focos)
  }
}

# Salvando a criança ------------------------------------------------------
saveRDS(base,"resultado/dados.RDS", compress = TRUE)
base %>% 
  rename(longitude = long,
         latitute = lat) %T>% 
  write_dta(.,"resultado/dados.dta")
