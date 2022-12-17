library(spatialprobit)
library(tidyverse)
library(sp)
library(xtable)
library(geosphere)
library(magrittr)
library(spatialreg)

options(scipen=999)

dados <- read_csv("resultado/dados.csv") %>% 
  mutate(X1 = 1:n()) %>% 
  select(-tmp)

# Escolhendo o melhor número de vizinhos ----------------------------------

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
  tabela[v-4,2] <- logLik(sarprobit(dados$positivo~dados$dens_demografica+dados$renda+dados$alugados+dados$agua+dados$esgoto+dados$lixo+dados$energia+dados$pavimentacao+dados$arborizacao+dados$chuva15+dados$temp_gap_15+dados$area_pe_todos+dados$area_pe_positivo,w, computeMarginalEffects=FALSE, showProgress=FALSE))
}

vizinho <- tabela$vizinho[match(max(tabela$loglik),tabela$loglik)]

# Descritivo da matriz de peso --------------------------------------------

mean = 0
for(i in 1:12) {
  tmp <- dados %>% filter(
    periodo2 == i)
  for(j in 1:nrow(tmp)) {
    dist = 0
    for(k in 1:nrow(tmp)) {
      dist[k] = distHaversine(c(tmp$x[j],tmp$y[j]),c(tmp$x[k],tmp$y[k])) 
    }
    mean <- mean + dist %>% 
      as_tibble() %>% 
      filter(dist != 0) %>% 
      top_n(-vizinho) %>% 
      group_by() %>% 
      summarise_all(sum)/vizinho
  }
}
mean/nrow(dados)

# Rodando o modelo --------------------------------------------------------

vizinho <- 7

peso <- matrix(0, nrow=nrow(dados),ncol=nrow(dados))
comeco <- 0

for(k in 1:12) {
  tmp <- dados %>% 
    filter(periodo2 == k)
  coordinates(tmp) <- ~x+y
  coords <- coordinates(tmp)
  knn <- knn2nb(knearneigh(coords,k=vizinho, longlat=TRUE))
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

fit_sarprobit <- spatialprobit::sarprobit(dados$positivo~dados$dens_demografica+dados$renda+dados$alugados+dados$agua+dados$esgoto+dados$lixo+dados$energia+dados$pavimentacao+dados$arborizacao+dados$chuva15+dados$temp_gap_15+dados$area_pe_todos+dados$area_pe_positivo, w, computeMarginalEffects=TRUE, showProgress=FALSE)

summary(fit_sarprobit)
logLik(fit_sarprobit)


fit_probit <- glm(dados$positivo~dados$dens_demografica+dados$renda+dados$alugados+dados$agua+dados$esgoto+dados$lixo+dados$energia+dados$pavimentacao+dados$arborizacao+dados$chuva15+dados$temp_gap_15+dados$area_pe_todos+dados$area_pe_positivo, family = binomial(link = "probit"))

summary(fit_probit)
confint(fit_probit)
