library(geobr)
library(tidyverse)
library(sf)
library(magrittr)

sede_vigilancia <- tribble(~COORD_X, ~COORD_Y,
                           -44.04385794994415,-19.62004318512604)

liraa <- read_csv("../dados/liraa/liraa_sem_rep.csv") %>% 
  group_by(periodo, cd_geocodi) %>% 
  summarise(n = n(),
            x = mean(x, na.rm = TRUE),
            y = mean(y, na.rm = TRUE))

final <- tribble(~origem,
                 ~periodo,
                 ~distancia,
                 ~tempo)  
for(i in 1:nrow(liraa)) {
  origem <- liraa$cd_geocodi[i]
  periodo <- liraa$periodo[i]
  origem_lon <- liraa$x[i]
  origem_lat <- liraa$y[i]
  destino_lon = sede_vigilancia$COORD_X
  destino_lat = sede_vigilancia$COORD_Y
  result <- httr::content(httr::GET(paste0("http://router.project-osrm.org/route/v1/driving/",origem_lon,",",origem_lat,";",destino_lon,",",destino_lat,"?overview=false")))
  if(names(result)[1] == "code") {
    if(result$code != "Ok"){
      tmp <- tibble(origem = origem,
                    periodo = periodo,
                    distancia = NA,
                    tempo = NA)
      final %<>% bind_rows(tmp)
    } else {
      tmp <- tibble(origem = origem,
                    periodo = periodo,
                    distancia = result$routes[[1]]$distance,
                    tempo = result$routes[[1]]$duration)
      final %<>% bind_rows(tmp)
    }
  } else {
      tmp <- tibble(origem = origem,
                    periodo = periodo,
                    distancia = -404,
                    tempo = -404)
      final %<>% bind_rows(tmp)    
  }
}

pessoas <- tribble(~periodo, ~dias, ~pessoas,
                   "jan/15", 5, 21,
                   "mar/15", 4, 21,
                   "out/15", 5, 20,
                   "jul/16", 3, 20,
                   "out/16", 4, 15,
                   "jan/17", 3, 20,
                   "mar/17", 5, 20,
                   "out/17", 5, 15,
                   "jan/18", 5, 20,
                   "abr/18", 5, 20,
                   "ago/18", 5, 20,
                   "out/18", 5, 20)

ppp <- tribble(~ano, ~ppp,
               2015, 0.57810961,
               2016, 0.53486940,
               2017, 0.51686552,
               2018, 0.50168144,
               2019, 0.48332528)

gasolina <- tribble(~ano,~gasolina,~salario,
                    2015, 2.902, 979.12,
                    2016, 3.571, 1114.12,
                    2017, 3.731, 1339.23,
                    2018, 4.263, 1373.85)

final2 <- final %>% 
  group_by(periodo) %>% 
  summarise(distancia = sum(distancia)/1000) %>% 
  mutate(ano = 2000+as.numeric(substr(periodo,5,6))) %>% 
  left_join(pessoas, by = "periodo") %>% 
  left_join(gasolina, by = "ano") %>% 
  mutate(valor_transporte = distancia/10*gasolina,
         valor_agente = salario/31*dias*pessoas,
         valor_liraa = valor_transporte + valor_agente) %>% 
  left_join(ppp, by = "ano") %>% 
  mutate(valor_liraa = valor_liraa*ppp,
         valor_transporte = valor_transporte*ppp,
         valor_agente = valor_agente*ppp)

final2 %>% 
  arrange(ano) %>% 
  select(ano,periodo, valor_transporte,valor_agente,valor_liraa) %T>%
  write_excel_csv2(., "tabela/custo_liraa.csv")


