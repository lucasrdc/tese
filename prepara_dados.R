suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(geosphere))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggspatial))

# Abrindo o arquivo principal com os dados do LIRAa -----------------------

dados <- read_csv("dados/liraa/liraa_sem_rep.csv")
dados$v1 <- c(1:nrow(dados))

pe <- read_csv("dados/pe/pe.csv") %>% 
  distinct(local,x,y)

shp <- st_read("buffer/data/pl.shp") %>% 
  mutate(`Census sectors` = ifelse(TIPO == "RURAL","Rural (19)","Urban (63)"))

point <- dados %>% 
  select(x, y, positivo) %>% 
  mutate(`Visited households` = ifelse(positivo == 1,"Positive (369)","Negative (12,334)"))

p <- ggplot(shp) + 
  geom_sf(aes(fill = `Census sectors`)) +
  scale_fill_manual(values = alpha(c("#FDF5F0", "#EA785A"), .3)) +
  geom_point(data = point, aes(x=x,y=y, colour=`Visited households`)) +
  geom_point(data = subset(point, positivo == 1), aes(x=x,y=y, colour = `Visited households`)) +
  xlab("Longitude") +
  ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw(base_size=14) +
  theme(
    legend.position = c(.95, .30),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
ggsave("points.png", dpi = 300, width = 10, height = 10)

p <- ggplot(shp) + 
  geom_sf(aes(fill = `Census sectors`)) +
  scale_fill_manual(values = alpha(c("#FDF5F0", "#EA785A"), .3)) +
  geom_point(data = pe, aes(x=x,y=y), colour = "#01BFC4") +
  xlab("Longitude") +
  ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw(base_size=14) +
  theme(
    legend.position = c(.95, .15),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
ggsave("pe.png", dpi = 300, width = 10, height = 10)


# Abrindo dados do CENSO 2010 (Domicílio e Entorno) -----------------------

domicilio <- read_csv2("dados/censo/Domicilio01_MG.csv", na = "X") %>% 
  filter(Cod_setor >= 314930900000000 & Cod_setor <= 314930999999999)

entorno <- read_csv2("dados/censo/Entorno01_MG.csv", na = "X") %>% 
  filter(Cod_setor >= 314930900000000 & Cod_setor <= 314930999999999)

basico <- read_csv2("dados/censo/Basico_MG.csv", na = "X") %>% 
  filter(Cod_setor >= 314930900000000 & Cod_setor <= 314930999999999)


# Calculando as proporções das variáveis de interesse (% esgoto, a --------

censo <- domicilio %>% as_tibble() %>% transmute(
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

# Pegando a área dos setores ----------------------------------------------

area <- as_tibble(read.dbf("dados/shape/pl.dbf", as.is=TRUE)) %>%
  dplyr::select(CD_GEOCODI,area_km,area_m) %>% 
  mutate(CD_GEOCODI = as.numeric(CD_GEOCODI)) 
censo %<>% left_join(area, by=c("setor"="CD_GEOCODI")) %>% 
  mutate(dens_demografica = moradores/area_km)

# Juntando com os dados do CENSO ------------------------------------------

dados %<>% left_join(censo, by=c("cd_geocodi" = "setor"))

# Dados de chuva por ponto (CHIRPS) ---------------------------------------

datas_liraa <- c(dmy('12-01-2015'),dmy('03-03-2015'),dmy('19-10-2015'),
                 dmy('18-07-2016'),dmy('18-10-2016'),dmy('11-01-2017'),
                 dmy('06-03-2017'),dmy('16-10-2017'),dmy('08-01-2018'),
                 dmy('09-04-2018'),dmy('06-08-2018'),dmy('22-10-2018'))

xy <- dados %>% dplyr::select(x,y)
tmp <- dados %>% dplyr::select(id,periodo2)

coords <- SpatialPointsDataFrame(coords= xy, data=tmp,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

chuva_acumulada <- tribble(
  ~id, ~periodo2, ~x, ~y, ~chuva10, ~chuva15, ~chuva30
)

for(i in c(1:12)) {
  subbase <- subset(coords, periodo2 == i)
  chuva <- 0
  for(janela in c(10,15,30)) {
    if (janela == 10) {
      backup = as_tibble(subbase)    
    }
    for (j in 1:janela) {
      tmp <- datas_liraa[i]-ddays(j)
      myras <- raster::raster(paste0("dados/chuva/",year(tmp),"/chirps-v2.0.",format(tmp, "%Y.%m.%d"),".tif"))
      chuva <- chuva + raster::extract(x = myras, y = subbase, fun=mean)
    }
    varname <- paste0("chuva",janela)
    backup %<>% mutate(
      !!varname := chuva
    )
  }
  chuva_acumulada %<>% tibble::add_row(backup)
}

dados %<>% left_join(chuva_acumulada, by=c("id","periodo2","x","y"))

# Adicionando temperatura -------------------------------------------------

temp <- read_csv("dados/temperatura/temperatura.csv")

aux <- data.frame(periodo2=integer(),
                 temp_max_10=double(),
                 temp_min_10=double(),
                 temp_mean_10=double(),
                 temp_gap_10=double(),
                 temp_max_15=double(),
                 temp_min_15=double(),
                 temp_mean_15=double(),
                 temp_gap_15=double(),
                 temp_max_30=double(),
                 temp_min_30=double(),
                 temp_mean_30=double(),
                 temp_gap_30=double())

for(i in c(1:12)) {
  t_max <- 0
  t_min <- 0
  t_mean <- 0
  t_gap <- 0
  aux[i,1] = i
  for(janela in c(10,15,30)) {
    t_max <- 0
    t_min <- 0
    t_mean <- 0
    for (j in 1:janela) {
      tmp <- datas_liraa[i]-ddays(j)
      t_max <- t_max + subset(temp, ano == year(tmp) & mes == month(tmp) & dia == day(tmp))$max
      t_min <- t_min + subset(temp, ano == year(tmp) & mes == month(tmp) & dia == day(tmp))$min
      t_mean <- t_mean + subset(temp, ano == year(tmp) & mes == month(tmp) & dia == day(tmp))$mean
      t_gap <- t_gap + (subset(temp, ano == year(tmp) & mes == month(tmp) & dia == day(tmp))$max-subset(temp, ano == year(tmp) & mes == month(tmp) & dia == day(tmp))$min)
    }
    if(janela == 10) {
      aux[i,2] = t_max/janela
      aux[i,3] = t_min/janela
      aux[i,4] = t_mean/janela
      aux[i,5] = t_gap/janela
    }
    if(janela == 15) {
      aux[i,6] = t_max/janela
      aux[i,7] = t_min/janela
      aux[i,8] = t_mean/janela 
      aux[i,9] = t_gap/janela
    }
    if(janela == 30) {
      aux[i,10] = t_max/janela
      aux[i,11] = t_min/janela
      aux[i,12] = t_mean/janela 
      aux[i,13] = t_gap/janela
    }  
  }
}

dados %<>% left_join(aux,by="periodo2")

# Crianddo as variáveis de pontos estratégicos ----------------------------

pe <- read_csv('dados/pe/pe.csv')

ciclos <- as_tibble(data.frame("periodo" = c(1:12),"ano" = as.numeric(c("2015","2015","2015","2016","2016","2017","2017","2017","2018","2018","2018","2018")), "ciclo" = as.numeric(c("2","5","20","14","20","2","5","20","2","7","15","20"))))

dados$dist_pe = 1000000000
dados$dist_pe_positivo = 1000000000
dados$tem_ponto = 0
dados$tem_ponto_positivo = 0
for(i in c(1:nrow(dados))){
  
  perio <- dados %>% 
    slice(i) %>% 
    select(periodo2) %>% 
    pull()
  
  year <- ciclos %>% 
    filter(periodo == perio) %>% 
    pull(ano)

  cicle <- ciclos %>% 
    filter(periodo == perio) %>% 
    pull(ciclo)
  
  pe_select <- pe %>% filter(
    ciclo == cicle | ciclo == cicle -1,
    ano == year
  ) %>% group_by(local,x,y) %>% 
    summarize(
      focos = sum(focos)
    )
  
  for(j in c(1:nrow(pe_select))){
    ponto = c(pe_select$x[j],pe_select$y[j])
    casa = c(dados$x[i],dados$y[i])
    dist = distHaversine(ponto,casa)/1000
    if(dist <= dados$dist_pe[i]){
      dados$dist_pe[i] = dist
    }
    if(dist <= 0.150) {
      dados$tem_ponto[i] = dados$tem_ponto[i]+1
    }
  }
  pe_select <- pe_select %>% 
    filter(focos != 0)
  if(nrow(pe_select) == 0) {
    dados$dist_pe_positivo[i] = 1000000000
  } else {
    for(j in c(1:nrow(pe_select))){  
      ponto = c(pe_select$x[j],pe_select$y[j])
      casa = c(dados$x[i],dados$y[i])
      dist = distHaversine(ponto,casa)/1000
      if(dist <= dados$dist_pe_positivo[i]){
        dados$dist_pe_positivo[i] = dist
      }
      if(dist <= 0.150) {
        dados$tem_ponto_positivo[i] = dados$tem_ponto_positivo[i]+1
      }
    }
  }
}
dados$dist_pe_positivo[dados$dist_pe_positivo == 1000000000] <- NA

# criando variáveis do inverso da distancia -------------------------------

dados$dist_pe2 = 1/(dados$dist_pe)^2
dados$dist_pe_positivo2 = 1/(dados$dist_pe_positivo)^2
dados$dist_pe_positivo2[is.na(dados$dist_pe_positivo2)] <- 0

# calculando o buffer -----------------------------------------------------

pl_shp <- st_read("dados/shape/pl.shp")
pe_shp <- st_read("dados/shape/pe.shp")

result <- pe %>%
  arrange(local, ano, ciclo) %>% 
  select(-aegypti,-albopctus,-area) %>% 
  pivot_wider(names_from=c(ano,ciclo),
              values_from=focos,
              names_glue = "foco_{ano}_{ciclo}")

pe_shp %<>% left_join(result,by="id") %>% 
  arrange(id) 

dados_spatial <- st_as_sf(dados, coords=c("x","y"), crs = 4326)

dados_km = st_sf(st_transform(dados_spatial, "+proj=utm +zone=23 +south +datum=WGS84 +units=km"))
shape_km = st_sf(st_transform(pl_shp, "+proj=utm +zone=23 +south +datum=WGS84 +units=km"))
pe_km = st_sf(st_transform(pe_shp, "+proj=utm +zone=23 +south +datum=WGS84 +units=km"))

for(i in c(1:12)){
  subbase <- dados_km %>% filter(periodo2 == i)
  ano <- ciclos %>% 
    filter(periodo == i) %>% 
    pull(ano)
  rodada <- ciclos %>% 
    filter(periodo == i) %>% 
    pull(ciclo)
  subresult <- pe_km %>% 
    select(id,x,y,paste0("foco_",ano,"_",rodada),paste0("foco_",ano,"_",rodada-1))
  subresult$focos = subresult[[4]] + subresult[[5]]
  
  subresult1 <- subresult %>% filter(focos == 0)
  
  buffer <- subbase %>% st_buffer(.2)
  intersecao = st_intersection(buffer,subresult1)
  intersecao$area_pe_todos = st_area(intersecao)

  intersecao %<>% 
    select(v1,area_pe_todos) %>% 
    group_by(v1) %>% 
    summarise(area_pe_todos = sum(area_pe_todos)) %>%       
    ungroup()
  
  subresult %<>% filter(focos != 0)
  
  if(nrow(subresult) != 0) {
    intersecao_positivo = st_intersection(buffer,subresult)
    intersecao_positivo$area_pe_positivo = st_area(intersecao_positivo)*intersecao_positivo$focos
    intersecao_positivo %<>% select(v1, area_pe_positivo)
    
    intersecao_positivo %<>% 
      select(v1,area_pe_positivo) %>% 
      group_by(v1) %>% 
      summarise(area_pe_positivo = sum(area_pe_positivo)) %>% 
      ungroup()
    
    intersecao %<>% as_tibble(intersecao) %>% 
      select(v1, area_pe_todos) %>% 
      left_join(intersecao_positivo, by='v1') %>% 
      mutate(area_pe_positivo = replace_na(area_pe_positivo,0))
  }
  
  if(i == 1) {
    areas <- intersecao
  } else {
    areas %<>% bind_rows(intersecao)
  }
}

areas %<>%
  mutate(area_pe_positivo = replace_na(area_pe_positivo,0)) %>% 
  select(-geometry)

dados %<>% 
  left_join(areas, by="v1") %>% 
  select(-geometry) %>% 
  mutate(area_pe_todos = replace_na(area_pe_todos,0),
         area_pe_positivo = replace_na(area_pe_positivo,0))

# remomendo observações com NA do censo -----------------------------------
dados <- dados[!is.na(dados$agua),]

# Salvando a criança ------------------------------------------------------
write.csv(dados,"resultado/dados.csv", row.names = FALSE)