library(tidyverse)
library(lubridate)

dengue <- read_csv2("dengue.csv") %>% 
  mutate(ano = dmy(paste0("01-01-", ano)))

ggplot(dengue, aes(x = factor(year(ano)), y = casos_confirmados)) + 
  geom_bar(stat = 'identity', fill = "steelblue") + 
  geom_text(aes(label = casos_confirmados), vjust = 1.6, color = "white", size = 3.5) +
  annotate("rect", xmin = 8.5, xmax = 12.5, ymin = -10, ymax = 1780, alpha = .2, color = "tomato", fill = "tomato") +
  annotate("text", x = 10.5, y = 1650, label = "Anos do estudo", color = "tomato") +
  xlab("Ano") + 
  ylab("Casos confirmados") +
  theme_bw() 

ggsave("graficos/dengue_historico.png", dpi = 300)
