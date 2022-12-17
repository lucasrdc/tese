library(tidyverse)
library(magrittr)
library(pscl)

options(scipen=999)

dados <- readRDS("resultado/dados.RDS") %>%
  filter(visitados != 0) %>% 
  mutate(pe = replace_na(pe,0),
         pe_vizinho = replace_na(pe_vizinho,0),
         sinan_1 = sinan1+sinan2,
         sinan_2 = sinan3+sinan4,
         chuva = chuva_antes + chuva_depois,
         Distrito = case_when(NM_DISTRIT == "DOUTOR LUND" ~ "Dr. Lund",
                              NM_DISTRIT == "FIDALGO" | NM_DISTRIT == "VERA CRUZ DE MINAS" ~ "Outros",
                              NM_DISTRIT == "PEDRO LEOPOLDO" ~ "Pedro Leopoldo",
                              TRUE ~ "Lagoa Santo Antônio"))

m1 <- zeroinfl(sinan_1 ~ chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito) | chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito),
               data = dados, dist = "negbin")
summary(m1)

m2 <- zeroinfl(sinan_2 ~ chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito) | chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito),
               data = dados, dist = "negbin")
summary(m2)
expCoef <- exp(coef(m2))

tipo <- dados %>% 
  select(., setor, periodo2, a1, a2, b, c, d1, d2, e) %>% 
  pivot_longer(!c(setor, periodo2), names_to = "tipo", values_to = "focos") %>% 
  mutate(focos = ifelse(focos == 0, NA, focos)) %>% 
  group_by(setor, periodo2) %>% 
  filter(focos == max(focos, na.rm = TRUE)) %>% 
  select(-focos) %>% 
  pivot_wider(names_from = "tipo", values_from = "tipo") %>% 
  mutate(a1 = ifelse(is.na(a1), "", a1),
         a2 = ifelse(is.na(a2), "", a2),
         b = ifelse(is.na(b), "", b),
         c = ifelse(is.na(c), "", c),
         d1 = ifelse(is.na(d1), "", d1),
         d2 = ifelse(is.na(d2), "", d2),
         e = ifelse(is.na(e), "", e)) %>% 
  mutate(tipo = paste0(a1,a2,b,c,d1,d2,e)) %>% 
  mutate(tipo = case_when(tipo == "a1a2" ~ "a1",
                          tipo == "a1a2be" ~ "a1",
                          tipo == "a1a2c" ~ "a1",
                          tipo == "a1a2d2" ~ "a1",
                          tipo == "a1b" ~ "a1",
                          tipo == "a1c" ~ "a1",
                          tipo == "a2b" ~ "b",
                          tipo == "a2bcd1" ~ "b",
                          tipo == "a2c" ~ "outros",
                          tipo == "a2cd1" ~ "a2",
                          tipo == "a2cd2" ~ "a2",
                          tipo == "a2d1d2" ~ "a2",
                          tipo == "a2d2" ~ "a2",
                          tipo == "a2e" ~ "a2",
                          tipo == "bc" ~ "b",
                          tipo == "bcd1d2e" ~ "b",
                          tipo == "bce" ~ "b",
                          tipo == "bd1" ~ "b",
                          tipo == "bd2" ~ "b",
                          tipo == "a1" ~ "a1",
                          tipo == "a2" ~ "a2",
                          tipo == "b" ~ "b",
                          tipo == "c" ~ "outros",
                          tipo == "d1" ~ "outros",
                          tipo == "d2" ~ "outros",
                          tipo == "e" ~ "outros")) %>% 
  select(setor, periodo2, tipo)

base <- left_join(dados, tipo, by = c("setor", "periodo2")) %>% 
  mutate(tipo = as.factor(ifelse(is.na(tipo), "sem foco", tipo)), 
         tipo = relevel(tipo, ref = 5))

m1 <- zeroinfl(sinan_1 ~ chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito) + factor(tipo) | chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito) + factor(tipo),
               data = base, dist = "negbin")
summary(m1)

m2 <- zeroinfl(sinan_2 ~ chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito) + factor(tipo) | chuva + iip + factor(pe) + factor(pe_vizinho) + factor(Distrito) + factor(tipo),
               data = base, dist = "negbin")
summary(m2)
