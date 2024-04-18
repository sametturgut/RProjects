# Dr.Öğr.Üy. İSMAİL YENİLMEZ'in ders notlarından faydalanılmıştır.

#---
#veri Kaynağı

#veriyi toplama, Kaynağın doğrulanması

#veri satın alma, veriyi toplama, verinin açık bir yerden temini gibi aşamalar
#---

# Rastgele veri oluşturma için kütüphane

install.packages("MASS")
library(MASS)

# Veri setini oluşturma
set.seed(789) # Tekrarlanabilirlik için seed belirleme
n <- 200 # Gözlem sayısı

# Değişkenleri oluşturma
yas <- rnorm(n, mean = 35, sd = 10)  # Yaş değişkeni. Ortalama yaş 35, standart sapma 10.
cinsiyet <- sample(c("Erkek", "Kadın"), size = n, replace = TRUE)  # Cinsiyet değişkeni. Erkek ve Kadın değerlerinden örneklem oluşturur ve replace ile bir kez seçilen cinsiyet tekrar seçilebilir olur.
gelir <- rnorm(n, mean = 50000, sd = 10000)  # Gelir değişkeni. Ortalama 50K, standart sapma 10K.
alisverisMiktari <- rnorm(n, mean = 20, sd = 18) # Alışveriş sayısı. Ortalama 20, standart sapma 18.

# Tamsayı haline getirme ve yuvarlama
yas <- floor(yas)
alisverisMiktari <- floor(alisverisMiktari)
gelir <- round(gelir)

alisverisMiktari <- pmax(alisverisMiktari, 0) # Eksi değerleri sıfır yapma

# Veri setini oluşturma
data <- data.frame(yas, cinsiyet, gelir, alisverisMiktari)

# Oluşturulan veri setini gösterme
head(data)

# Eksik veri tespiti
is_na_yas <- is.na(yas)
is_na_cinsiyet <- is.na(cinsiyet)
is_na_gelir <- is.na(gelir)
is_na_alisverisMiktari <- is.na(alisverisMiktari)

# Eksik verileri ortalama ile doldurma
mean_yas <- mean(yas, na.rm = TRUE)
yas_filled <- ifelse(is.na(yas), mean_yas, yas)

# Z-puanı hesaplama
z_scores <- scale(yas)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3

# Kantillerden yararlanma
summary(data)

# Korelasyon katsayısını hesaplama
correlation <- cor(yas, gelir)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)

# corrplot paketini yükleme
install.packages("corrplot")
library(corrplot)
# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(yas, gelir, alisverisMiktari))
print(correlation_matrix)
# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Veriyi standartlaştırma
yas_standardized <- scale(yas)
gelir_standardized <- scale(gelir)
alisveriMiktari_standardized <- scale(alisverisMiktari)

# Standartlaştırılmış veriyi kontrol etme
summary(yas_standardized)
summary(gelir_standardized)
summary(alisveriMiktari_standardized)

# Veri setini test ve eğitim kümelerine ayırma
install.packages("caTools")
library(caTools)
split <- sample.split(data$yas, SplitRatio = 0.7)  # 70% eğitim, 30% test
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Veri setini keşfetme ve görselleştirme
summary(train_data)
summary(test_data)

# Histogram ve kutu grafikleri
hist(train_data$gelir, main = "Gelir Dağılımı (Training Data)", col = "lightgreen")
boxplot(train_data$yas, main = "Yaş Dağılımı (Training Data)")
hist(test_data$gelir, main = "Gelir Dağılımı (Test Data)", col = "lightgreen")
boxplot(test_data$yas, main = "Yaş Dağılımı (Test Data)")

# Sonuçları inceleme
dim(train_data)
dim(test_data)

# MASS paketi içerisindeki Animals verisetini çekme ve inceleme.
data(Animals)
head(Animals)

# Eksik veri kontrolü
is_na_body <- is.na(Animals$body)
is_na_brain <- is.na(Animals$brain)

# Eksik verileri ortalama ile doldurma
mean_body <- mean(Animals$body, na.rm = TRUE)
body_filled <- ifelse(is.na(Animals$body), mean_body, Animals$body)
mean_brain <- mean(Animals$brain, na.rm = TRUE)
brain_filled <- ifelse(is.na(Animals$brain), mean_brain, Animals$brain)

# Z-skor hesaplama
z_scores <- scale(Animals)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3

# Betimsel istatistiklere bakma
summary(Animals)

# Korelasyon katsayısını hesaplama
correlation <- cor(Animals$body, Animals$brain)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)

# corrplot paketini yükleme
install.packages("corrplot")
library(corrplot)
# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(Animals$body, Animals$brain))
print(correlation_matrix)
# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Veriyi standartlaştırma
body_standardized <- scale(Animals$body)
brain_standardized <- scale(Animals$brain)

# Standartlaştırılmış veriyi kontrol etme
summary(body_standardized)
summary(brain_standardized)


# Veri setini test ve eğitim kümelerine ayırma
install.packages("caTools")
library(caTools)
split <- sample.split(Animals$body, SplitRatio = 0.7)  # 70% eğitim, 30% test
train_data <- subset(Animals$body, split == TRUE)
test_data <- subset(Animals$body, split == FALSE)

train_data <- data.frame(train_data)

# Veri setini keşfetme ve görselleştirme
summary(train_data)
summary(test_data)

# Ggplot paketini yükleme
install.packages("ggplot2")
library(ggplot2)

# Bilimsel gösterimi kapatma
options(scipen = 999)

# İlk aralık için histogram
hist_1 <- ggplot(data.frame(train_data), aes(x = train_data)) +
  geom_histogram(binwidth = 75, fill = "lightgreen") + # Histogram genişlikleri 75 birim
  xlim(0, 10000) +   # 10000'den sonra 80000'lere kadar bir veri bulunmuyor, bu sebeple daha okunaklı bir grafik için arada kalan kısım kesilmiştir.
  ggtitle("Kilo Dağılımı (0-10000)")

# İkinci aralık için histogram
hist_2 <- ggplot(data.frame(train_data), aes(x = train_data)) +
  geom_histogram(binwidth = 200, fill = "lightgreen") +
  xlim(85000, 90000) +
  ggtitle("Kilo Dağılımı (80000 ve üzeri)")

# Grafikleri yan yana gösterme
library(gridExtra)
grid.arrange(hist_1, hist_2, ncol = 2)

# Sonuçları inceleme
dim(train_data)
dim(test_data)
