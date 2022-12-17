library(tidyverse)
library(geobr)
library(sp)
library(spdep)
library(spatialprobit)
library(ggspatial)
library(ggrepel)

options(scipen=999)

shp <- read_census_tract(code_tract = 3149309) %>% 
  rename(`Tipo do setor` = zone) %>% 
  mutate(`Tipo do setor` = ifelse(`Tipo do setor`  == "RURAL", "Rural", "Urbano"))

# visitas -----------------------------------------------------------------

dados <- read_csv("resultado/dados.csv") %>% 
  mutate(X1 = 1:n(),
         positivo = ifelse(positivo == 1, "Positivo", "Negativo"),
         positivo = as.factor(positivo)) %>% 
  select(-tmp) %>% 
  rename(`Imóvel visitado` = positivo)

p <- ggplot(shp) +
  geom_sf(aes(fill = `Tipo do setor`)) +
  scale_fill_manual(values = c("#FEFCFA","#F4DAD4")) +
  geom_point(data = dados, aes(x = x, y = y, color = `Imóvel visitado`)) +
  geom_point(data = subset(dados, `Imóvel visitado` == "Positivo"), aes(x = x, y = y, color = `Imóvel visitado`)) +
  scale_alpha_manual(values = list("Negativo" = 0.2, "Positivo" = 1)) +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.2))
ggsave("figures/points.png", dpi = 300, width = 7.92)
ggsave("figures/points.eps", dpi = 300, width = 7.92)

# pontos estratégicos -----------------------------------------------------

pe <- read_csv('../dados/pe/pe.csv') %>% 
  distinct(x,y, .keep_all = TRUE)

p <- ggplot(shp) +
  geom_sf(aes(fill = `Tipo do setor`)) +
  scale_fill_manual(values = c("#FEFCFA","#F4DAD4")) +
  geom_point(data = pe, aes(x = x, y = y, color = "Ponto estratégico")) +
  scale_color_manual(values=c("#55bdc2")) +
  labs(x="Longitude", y="Latitude", color = "Imóvel") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15))
ggsave("figures/pe.png", dpi = 300, width = 7.92)
ggsave("figures/pe.eps", dpi = 300, width = 7.92)

# loglik ------------------------------------------------------------------

dados <- read_csv("resultado/dados.csv") %>% 
  mutate(X1 = 1:n()) %>% 
  select(-tmp)

tabela <- data.frame(matrix(data=0, nrow=30, ncol=2))
x <- c("vizinho","loglik")
colnames(tabela) <- x

for(v in seq(5,35)) {
  
  peso <- matrix(0, nrow=nrow(dados),ncol=nrow(dados))
  comeco <- 0
  
  for(k in 1:12) {
    tmp <- dados %>% 
      filter(periodo2 == k)
    coordinates(tmp) <- ~x+y
    coords <- coordinates(tmp)
    knn <- knn2nb(knearneigh(coords,k=v, longlat=TRUE))
    knn <- nb2mat(knn)
    
    for(i in 1:nrow(tmp)) {
      for(j in 1:ncol(tmp)) {
        peso[i+comeco,j+comeco] <- knn[i,j]
      }
    }
    comeco <- comeco + nrow(knn)
  }
  
  peso2 <- mat2listw(peso, row.names = dados$X1, style = "M")
  w <- as(as_dgRMatrix_listw(peso2), "CsparseMatrix")
  
  tabela[v-4,1] <- v
  tabela[v-4,2] <- logLik(spatialprobit::sarprobit(dados$positivo~dados$dens_demografica+dados$renda+dados$alugados+dados$agua+dados$esgoto+dados$lixo+dados$energia+dados$pavimentacao+dados$arborizacao+dados$chuva15+dados$temp_gap_15+dados$area_pe_todos+dados$area_pe_positivo, w, computeMarginalEffects=TRUE, showProgress=FALSE))
}

p <- ggplot(data=tabela, aes(x= vizinho, y= loglik, group=1)) +
  geom_line(linetype = "dashed") +
  geom_vline(xintercept = 7, color = 'red') +
  labs(y="Máxima verossimilhança", x="Número de vizinhos") +
  geom_point() +
  theme_bw()
ggsave("figures/loglik.png", dpi = 300, width = 7.92)
ggsave("figures/loglik.eps", dpi = 300, width = 7.92)
