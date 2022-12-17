library(tidyverse)
library(read.dbc)
library(lubridate)
library(magrittr)
library(waffle)

# variáveis ---------------------------------------------------------------

years <- seq(10,19)

months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

city <- 314930

cids <- c("A90","A91")

procedimentos <- c("0303010010","0303010029")

privado <- tribble(~ano,~privado,
                   2010, 0.316872565572811,
                   2011, 0.321116581298589,
                   2012, 0.352006180449398,
                   2013, 0.342881831224729,
                   2014, 0.329888905059914,
                   2015, 0.300691225762995,
                   2016, 0.291593723143521,
                   2017, 0.275317905378722,
                   2018, 0.269247048864224,
                   2019, 0.26706402315665)

gasolina <- tribble(~ano,~gasolina,
                    2010, 2.405,
                    2011, 2.499,
                    2012, 2.761,
                    2013, 2.74,
                    2014, 2.859,
                    2015, 2.902,
                    2016, 3.571,
                    2017, 3.731,
                    2018, 4.263,
                    2019, 4.459)

salario_minimo <- tribble(~ano, ~salario,
                          2010, 17,
                          2011, 18.16667,
                          2012, 20.73333,
                          2013, 22.6,
                          2014, 24.13333,
                          2015, 26.26667,
                          2016, 29.33333,
                          2017, 31.23333,
                          2018, 31.8,
                          2019, 33.26667)

salario_rais <- tribble(~ano, ~salario,
                        2010, 1143,
                        2011, 1269,
                        2012, 1420,
                        2013, 1570,
                        2014, 1729,
                        2015, 1842,
                        2016, 2015,
                        2017, 2065,
                        2018, 2106,
                        2019, 2124)

outras_doencas <- tribble(~ano, ~n,
                          2010, 39,
                          2011, 27,
                          2012, 10,
                          2013, 21,
                          2014, 55,
                          2015, 63,
                          2016, 117,
                          2017, 113,
                          2018, 150,
                          2019, 347)

# 2010 é FNS, o resto é SIOPS
vigilancia <- tribble(~ano, ~vigilancia,
                      2010, 367778.86,
                      2011, 344309.77,
                      2012, 390387.40,
                      2013, 383852.70,
                      2014, 1188000.00,
                      2015, 896501.00,
                      2016, 1288225.84,
                      2017, 978027.50,
                      2018, 1345083.71,
                      2019, 2116996.30)

pop <- tribble(~ano, ~pop,
               2015,	62353,
               2016,	62834,
               2017,	63305,
               2018,	63789,
               2019,	64258)

casos <- tribble(~ano, ~notificados, ~confirmados,
                 2015,	1015,	1012,
                 2016,	1486,	1471,
                 2017,	5,	4,
                 2018,	7,	4,
                 2019,	972,	949)

sinan <- read_csv("../dados/sinan/sinan_fixed.csv")

# hospitalar --------------------------------------------------------------

list_files <- NULL
for(year in years) {
  for(month in months) {
    list_files <- append(list_files,paste0("/Users/lucas/Documents/UFMG/Academia/DATASUS/AIH/RDMG",year,month,".dbc"))
  }
}
aih <- list_files %>%
  setNames(nm = .) %>% 
  map_dfr(~read.dbc(., as.is = TRUE) %>% 
            mutate(data = ymd(DT_INTER),
                   MUNIC_RES = as.integer(as.character(MUNIC_RES)),
                   MUNIC_MOV = as.integer(as.character(MUNIC_MOV))) %>% 
            filter(MUNIC_MOV == city | MUNIC_RES == city) %>% 
            filter(PROC_REA %in% procedimentos | DIAG_PRINC %in% cids) %>% 
            filter(data >= as.Date("2010-01-01") & data <= as.Date("2019-12-31")), 
          .id = "file_name") 

hospitalar <- aih %>% 
  mutate(MUNIC_MOV = ifelse(MUNIC_MOV != city, "Fora", "Pedro Leopoldo"),
         MUNIC_RES = ifelse(MUNIC_RES != city, "Fora", "Pedro Leopoldo"),
         ANO_CMPT = as.integer(as.character(ANO_CMPT))) %>% 
  group_by(ANO_CMPT, MUNIC_RES, MUNIC_MOV) %>% 
  summarise(n = n(),
            valor = sum(VAL_TOT+VAL_UTI, na.rm = TRUE)) %>% 
  left_join(privado, by = c("ANO_CMPT"="ano")) %>% 
  mutate(valor = valor*(privado+1)) %>% 
  filter(MUNIC_MOV != "Fora") %>% 
  group_by(ANO_CMPT) %>% 
  summarise(valor = sum(valor, na.rm = TRUE)) %>% 
  rename(ano = ANO_CMPT,
         hospitalar = valor) %T>% 
  write_excel_csv2(.,"tabela/hospitalar.csv")

# ambulatorial ------------------------------------------------------------

list_files <- NULL
for(year in years) {
  for(month in months) {
    list_files <- append(list_files,paste0("/Users/lucas/Documents/UFMG/Academia/DATASUS/SIA/PAMG",year,month,".dbc"))
  }
}
sia <- list_files %>%
  setNames(nm = .) %>% 
  map_dfr(~read.dbc(., as.is = TRUE) %>% 
            mutate(data = ym(as.character(PA_CMP)),
                   PA_MUNPCN = as.integer(as.character(PA_MUNPCN)),
                   PA_UFMUN = as.integer(as.character(PA_UFMUN))) %>% 
            filter(PA_UFMUN == city | PA_MUNPCN == city) %>% 
            filter(PA_PROC_ID %in% procedimentos | PA_CIDPRI %in% cids) %>%
            filter(data >= as.Date("2010-01-01") & data <= as.Date("2020-02-29")) %>% 
            select(-PA_CMP), 
          .id = "file_name")

ambulatorial <- sia %>% 
  mutate(PA_MUNPCN = ifelse(PA_MUNPCN != city, "Fora", "Pedro Leopoldo"),
         PA_UFMUN = ifelse(PA_UFMUN != city, "Fora", "Pedro Leopoldo")) %>% 
  group_by(year(data), PA_MUNPCN, PA_UFMUN) %>% 
  summarise(n = n(),
            valor = sum(PA_VALAPR, na.rm = TRUE)) %>% 
  filter(PA_UFMUN != "Fora") %>% 
  group_by(`year(data)`) %>% 
  summarise(valor = sum(valor, na.rm = TRUE)) %>% 
  rename(ano = `year(data)`,
         ambulatorial = valor) %T>% 
  write_excel_csv2(.,"tabela/ambulatorial.csv")

# testes ------------------------------------------------------------------

testes <- tribble(~ano, ~teste,
                  2015,	49722.17,
                  2016,	47752.63,
                  2017,	40622.78,
                  2018,	74611.02,
                  2019,	167633.85,
                  2020,	155537.93,
                  2021,	164978.98)

# consulta ----------------------------------------------------------------

consultas <- sinan %>% 
  mutate(id_municip = ifelse(id_municip != city, "Fora", "Pedro Leopoldo"),
         id_mn_resi = ifelse(id_mn_resi != city, "Fora", "Pedro Leopoldo")) %>% 
  group_by(nu_ano, id_mn_resi, id_municip) %>% 
  summarise(n = n()*10) %>% 
  filter(id_municip != "Fora") %>% 
  group_by(nu_ano) %>% 
  summarise(valor = sum(n, na.rm = TRUE)) %>% 
  rename(ano = nu_ano,
         consultas_1 = valor) %>% 
  mutate(ano = as.integer(ano),
         contulas_2 = consultas_1*2) %T>% 
  write_excel_csv2(.,"tabela/consultas.csv") # valor da consulta 10

# hemograma ---------------------------------------------------------------

# já está incluído no teste

# medicamentos ------------------------------------------------------------

medicamentos <- tribble(~ano, ~medicamento,
                        2015,	145320.3,
                        2016,	190525.2,
                        2017,	206870.8,
                        2018,	261198.36,
                        2019,	232006.81)

# transportes -------------------------------------------------------------

distancias <- read_csv("/Users/lucas/Documents/UFMG/Academia/distancias/distancia/result/stacked.csv") %>% 
  filter(origem >= 310000 & origem < 320000) %>% 
  filter(destino >= 310000 & destino < 320000)

transportes <- left_join(sinan %>% 
  filter(id_municip != id_mn_resi) %>% 
  mutate(ano = as.integer(nu_ano)) %>%
  left_join(distancias, by = c("id_municip" = "origem", "id_mn_resi" = "destino")) %>% 
  group_by(ano) %>% 
  summarise(distancia_sinan = sum(distancia, na.rm = TRUE)),
  aih %>% 
    filter(MUNIC_MOV != MUNIC_RES) %>% 
    rename(ano = ANO_CMPT) %>% 
    left_join(distancias, by = c("MUNIC_MOV" = "origem", "MUNIC_RES" = "destino")) %>% 
    group_by(ano) %>% 
    summarise(distancia_aih = sum(distancia, na.rm = TRUE)) %>% 
    mutate(ano = as.integer(ano)), 
by = "ano") %>% 
  transmute(ano = as.integer(ano),
            distancia = distancia_sinan + distancia_aih) %>% 
  left_join(gasolina, by = "ano") %>% 
  transmute(ano = as.integer(ano),
            transporte = distancia*gasolina/10) %>% 
  write_excel_csv2(.,"tabela/transportes.csv")

# passagem de onibus intra-municipal --------------------------------------

# vale a pena inserir esse custo?

# acompanhante ----------------------------------------------------------------

acompanhante <- aih %>% 
  filter(MUNIC_MOV == city) %>%
  filter(IDADE >= 18 | IDADE >= 65) %>% 
  mutate(ANO_CMPT = as.integer(as.character(ANO_CMPT))) %>% 
  group_by(ANO_CMPT) %>% 
  summarise(n = n()) %>% 
  left_join(salario_minimo, by = c("ANO_CMPT"="ano")) %>% 
  mutate(acompanhante = n*salario) %>% 
  select(ANO_CMPT, acompanhante) %>% 
  rename(ano = ANO_CMPT) %T>% 
  write_excel_csv2(.,"tabela/acompanhante.csv")
  
# absenteismo -------------------------------------------------------------

absenteismo <- sinan %>% 
  mutate(nu_idade_n = ifelse(nu_idade_n > 4000, nu_idade_n-4000,0),
         idade = ifelse(nu_idade_n >= 18, "adulto", "crianca"),
         nu_ano = as.integer(nu_ano)) %>% 
  group_by(nu_ano, idade) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = idade, values_from = n) %>% 
  left_join(salario_rais, by = c("nu_ano"="ano")) %>% 
  rename(ano = nu_ano) %>% 
  mutate(crianca = crianca * 31.18 * 6.07,
         adulto = adulto * (salario/31) * 6.07,
         absenteismo = adulto + crianca) %>% 
  select(ano, absenteismo) %T>% 
  write_excel_csv2(.,"tabela/absenteismo.csv")

# perda de produtividade em casos fatais (DALY) ---------------------------

# Tivemos só 3 óbitos entre 2010 e 2019. Vale a pena?

# vigilancia --------------------------------------------------------------

vigilancia_epi <- sinan %>% 
  mutate(nu_ano = as.integer(nu_ano)) %>% 
  group_by(nu_ano) %>% 
  summarise(dengue = n()) %>% 
  left_join(outras_doencas, by = c("nu_ano" = "ano")) %>% 
  mutate(proporcao = dengue/(n+dengue)) %>% 
  select(nu_ano, proporcao) %>% 
  left_join(vigilancia, by = c("nu_ano" = "ano")) %>% 
  transmute(ano = nu_ano,
            vigilancia = vigilancia,
            proporcao1 = proporcao,
            proporcao2 = mean(proporcao),
            proporcao3 = 0.82374855,
            proporcao4 = median(proporcao),
            vigilancia_normal = vigilancia * proporcao1,
            vigilancia_media = vigilancia * proporcao2,
            vigilancia_top3 = vigilancia * proporcao3,
            vigilancia_mediana = vigilancia * proporcao4) %>% 
  ungroup() %>% 
  select(ano, vigilancia_media, vigilancia_top3, vigilancia_mediana) %T>% 
  write_excel_csv2(.,"tabela/vigilancia.csv")

# inseticida --------------------------------------------------------------

# não temos os dados por município. Só UF

# suprimentos domésticos --------------------------------------------------

# Segundo POF 2017-2018 - RMBH sem BH = 3287949 de repelente
# Vou ratear pela população = PL corresponde a 63789/2811904 da RMBH sem BH

repelente <- 3287949*0.0226853406090677

# recursos extras ---------------------------------------------------------

## nào ttem

# PPP ---------------------------------------------------------------------

ppp <- tribble(~ano, ~ppp,
               2015, 0.57810961,
               2016, 0.53486940,
               2017, 0.51686552,
               2018, 0.50168144,
               2019, 0.48332528)

# juntando todo mundo -----------------------------------------------------

custos <- ppp %>% 
  left_join(pop, by = "ano") %>% 
  left_join(casos, by = "ano") %>% 
  left_join(hospitalar, by = "ano") %>% 
  left_join(ambulatorial, by = "ano") %>% 
  left_join(testes, by = "ano") %>% 
  left_join(consultas, by = "ano") %>% 
  left_join(medicamentos, by = "ano") %>% 
  left_join(transportes, by = "ano") %>% 
  left_join(acompanhante, by = "ano") %>% 
  left_join(absenteismo, by = "ano") %>% 
  left_join(vigilancia_epi, by = "ano") %>% 
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  mutate(`Cuidado ambulatorial` = ambulatorial + teste + consultas_1,
         `Cuidado hospitalar` = hospitalar + acompanhante + transporte,
         `Medicamentos` = medicamento,
         `Absenteísmo` = absenteismo,
         `Vigilância` = vigilancia_media,
         `Insumos domésticos` = repelente) %>% 
  select(ano, ppp, pop, notificados, confirmados, `Cuidado ambulatorial`:`Insumos domésticos`) %>%
  mutate(across(`Cuidado ambulatorial`:`Insumos domésticos`, ~.x*ppp)) %>% 
  select(-ppp) %T>%
  write_excel_csv2(., "tabela/custo_total.csv")


tmp <- custos %>% 
  select(ano, `Cuidado ambulatorial`:`Insumos domésticos`) %>% 
  pivot_longer(!ano, names_to = "componente", values_to = "valor") %>% 
  mutate(n = valor/sum(valor)*100) %>% 
  arrange(desc(n))

p <- ggplot(tmp, aes(values = valor, fill = componente)) +
  geom_waffle(size = 0.5, colour = "white", flip = TRUE, make_proportional = TRUE, radius = unit(4, "pt")) +
  coord_equal() +
  facet_wrap(~ano) +
  labs(fill = NULL, colour = NULL) +
  theme_minimal() +
  theme_enhance_waffle() +
  theme(legend.position = "bottom")

ggsave("figuras/percentual.png", dpi = 300, width = 8.5, height = 6.2)


# cenários ----------------------------------------------------------------


custos <- ppp %>% 
  left_join(pop, by = "ano") %>% 
  left_join(casos, by = "ano") %>% 
  left_join(hospitalar, by = "ano") %>% 
  left_join(ambulatorial, by = "ano") %>% 
  left_join(testes, by = "ano") %>% 
  left_join(consultas, by = "ano") %>% 
  left_join(medicamentos, by = "ano") %>% 
  left_join(transportes, by = "ano") %>% 
  left_join(acompanhante, by = "ano") %>% 
  left_join(absenteismo, by = "ano") %>% 
  left_join(vigilancia_epi, by = "ano") %>% 
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  mutate(Baseline = ambulatorial + teste + consultas_1 + hospitalar + acompanhante + transporte + medicamento + absenteismo + vigilancia_media + repelente,
         `Cenário 1` = ambulatorial + teste + consultas_1 + hospitalar + acompanhante + transporte + medicamento + absenteismo + vigilancia_mediana + repelente,
         `Cenário 2` = ambulatorial + teste + consultas_1 + hospitalar + acompanhante + transporte + medicamento + absenteismo + vigilancia_top3 + repelente,
         `Cenário 3` = ambulatorial + teste + contulas_2 + hospitalar + acompanhante + transporte + medicamento + absenteismo + vigilancia_media + repelente,
         `Cenário 4` = ambulatorial + teste + contulas_2 + hospitalar + acompanhante + transporte + medicamento + absenteismo + vigilancia_mediana + repelente,
         `Cenário 5` = ambulatorial + teste + contulas_2 + hospitalar + acompanhante + transporte + medicamento + absenteismo + vigilancia_top3 + repelente) %>% 
  select(ano, ppp, pop, notificados, confirmados, Baseline:`Cenário 5`) %>%
  mutate(across(Baseline:`Cenário 5`, ~.x*ppp)) %>% 
  select(-ppp) %T>% 
  write_excel_csv2(., "tabela/cenario.csv")





