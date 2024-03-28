install.packages("DALEX")
library(DALEX)
install.packages("dplyr")
library(dplyr)

data(apartments)
View(apartments)

#evleri yapılma yıllarına göre aralıklara ayırma
ev_durumu <- mutate(apartments, evDurumu = case_when(
  construction.year <= 1950 ~ "Tarihi Eser",
  construction.year > 1950 & construction.year <= 1980 ~ "Çok Eski",
  construction.year > 1980 & construction.year <= 2000 ~ "Eski",
  construction.year > 2000 ~ "Yeni"))
View(arrange(ev_durumu, construction.year))

#evleri durumlarına göre gruplayarak bu grupların maksimumdaki ve minimumdaki liderlerini bulma
kategori_lideri <- function(df) {
  df %>%
    mutate(evDurumu = case_when(
      construction.year <= 1950 ~ "Tarihi Eser",
      construction.year > 1950 & construction.year <= 1980 ~ "Çok Eski",
      construction.year > 1980 & construction.year <= 2000 ~ "Eski",
      construction.year > 2000 ~ "Yeni")) %>%
    mutate(evDurumu = factor(evDurumu, levels = c("Tarihi Eser", "Çok Eski", "Eski", "Yeni"))) %>%
    group_by(evDurumu) %>%
    summarize(enYuksekMetrekareYili = construction.year[which.max(m2.price)],
              enYuksekMetrekare = max(m2.price),
              enDusukMetrekareYili = construction.year[which.min(m2.price)],
              enDusukMetrekare = min(m2.price))
}

kategori_lideri(apartments)

