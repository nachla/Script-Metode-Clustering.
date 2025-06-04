install.packages("readxl")
library(readxl)

data <- read_excel("C:/Users/nachla/ANMUL/DATASET_ANMUL_KEL_3.xlsx")
str(data)
names(data)
head(data)

# cek missing values
sum(is.na(data))  
data <- na.omit(data)  

# cek outlier dgn boxplot
kolom_kesehatan <- c("Perawat", 
                     "Bidan", 
                     "Tenaga Kefarmasian", 
                     "Tenaga Kesehatan Masyarakat", 
                     "Tenaga Kesehatan Lingkungan", 
                     "Tenaga Gizi")

boxplot(data[ , kolom_kesehatan],
        main = "Boxplot Seluruh Jenis Tenaga Kesehatan",
        col = rainbow(length(kolom_kesehatan)),
        las = 2)  

# Normalisasi data
data_normalized <- scale(data[, c("Perawat", 
                                  "Bidan", 
                                  "Tenaga Kefarmasian", 
                                  "Tenaga Kesehatan Masyarakat", 
                                  "Tenaga Kesehatan Lingkungan", 
                                  "Tenaga Gizi")])
boxplot(data_normalized,
        main = "Boxplot Data Ternormalisasi - Semua Jenis Tenaga Kesehatan",
        col = rainbow(length(kolom_kesehatan)),
        las = 2)
head(data_normalized)


# PCA untuk reduksi dimensi
pca_result <- prcomp(data_normalized, center = TRUE, scale. = TRUE)
summary(pca_result)
biplot(pca_result)

# Menghitung statistik dasar
mean_values <- colMeans(data_normalized)
median_values <- apply(data_normalized, 2, median)
sd_values <- apply(data_normalized, 2, sd)
data_stats <- data.frame(Mean = mean_values, Median = median_values, SD = sd_values)
print(data_stats)

# Visualisasi data dengan histogram
install.packages("ggplot2") 
library(ggplot2)
library(tidyr)

data_long <- data %>%
  pivot_longer(cols = c("Perawat", "Bidan", "Tenaga Kefarmasian", 
                        "Tenaga Kesehatan Masyarakat", "Tenaga Kesehatan Lingkungan", 
                        "Tenaga Gizi"), 
               names_to = "Kategori", values_to = "Jumlah")

ggplot(data_long, aes(x = Jumlah)) + 
  geom_histogram(bins = 30, fill = "lightblue", color = "black") + 
  facet_wrap(~Kategori) +
  theme_minimal()

# K-Means
# Menentukan jumlah klaster optimal menggunakan Elbow Method
install.packages("factoextra") 
library(factoextra)             

fviz_nbclust(data_normalized, kmeans, method = "wss")

# K-Means dengan jumlah klaster 3
set.seed(123)
kmeans_result <- kmeans(data_normalized, centers = 3, nstart = 25)
fviz_cluster(kmeans_result, data = data_normalized)

# Hierarchical 
# Menghitung jarak antar data
dist_data <- dist(data_normalized)
hclust_result <- hclust(dist_data, method = "ward.D2")
plot(hclust_result)
rect.hclust(hclust_result, k = 3, border = 2:4)

# DBSCAN
install.packages("dbscan")
library(dbscan)

dbscan_result <- dbscan(data_normalized, eps = 0.5, minPts = 5)
fviz_cluster(dbscan_result, data = data_normalized)

# Menambahkan hasil klasterisasi ke data
data$Cluster_KMeans <- kmeans_result$cluster
data$Cluster_Hierarchical <- cutree(hclust_result, k = 3)
data$Cluster_DBSCAN <- dbscan_result$cluster
head(data)

# Visualisasi hasil klasterisasi (contoh K-Means)
ggplot(data, aes(x = Perawat, y = Bidan, color = as.factor(Cluster_KMeans))) + 
  geom_point() + 
  labs(title = "K-Means Clustering - Jumlah Perawat vs Jumlah Bidan")

# Kesimpulan berdasarkan klaster
summary(data$Cluster_KMeans)
