install.packages("readxl")
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
install.packages("dbscan")
install.packages("e1071")
install.packages("tidyr")
install.packages("dplyr")

library(readxl)
library(ggplot2)
library(cluster)
library(factoextra)
library(dbscan)
library(e1071)
library(tidyr)
library(dplyr)

data <- read_excel("C:/Users/nachla/ANMUL/DATASET_ANMUL_KEL_3.xlsx")
head(data)


# cek missing values
sum(is.na(data))
data[is.na(data)] <- apply(data, 2, function(x) mean(x, na.rm = TRUE))
data_normalized <- scale(data[, -1])  


# menghitung statistik (mean, median, dan standar deviasi) untuk setiap kategori
summary_values <- data.frame(
  Kategori = c("Perawat", "Bidan", "Tenaga Kefarmasian", "Tenaga Kesehatan Masyarakat", 
               "Tenaga Kesehatan Lingkungan", "Tenaga Gizi"),
  Mean = c(mean(data$Perawat), mean(data$Bidan), mean(data$`Tenaga Kefarmasian`), 
           mean(data$`Tenaga Kesehatan Masyarakat`), mean(data$`Tenaga Kesehatan Lingkungan`), 
           mean(data$`Tenaga Gizi`)),
  Median = c(median(data$Perawat), median(data$Bidan), median(data$`Tenaga Kefarmasian`), 
             median(data$`Tenaga Kesehatan Masyarakat`), median(data$`Tenaga Kesehatan Lingkungan`), 
             median(data$`Tenaga Gizi`)),
  Standar_Deviasi = c(sd(data$Perawat), sd(data$Bidan), sd(data$`Tenaga Kefarmasian`), 
                      sd(data$`Tenaga Kesehatan Masyarakat`), sd(data$`Tenaga Kesehatan Lingkungan`), 
                      sd(data$`Tenaga Gizi`))
)
print(summary_values)


# visualisasi boxplot
# Perawat
ggplot(data, aes(x = "Perawat", y = Perawat)) + 
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Distribusi Jumlah Perawat per Kabupaten/Kota", x = "Perawat", y = "Jumlah") +
  theme_minimal()

# Bidan
ggplot(data, aes(x = "Bidan", y = Bidan)) + 
  geom_boxplot(fill = "lightgreen", color = "green") +
  labs(title = "Distribusi Jumlah Bidan per Kabupaten/Kota", x = "Bidan", y = "Jumlah") +
  theme_minimal()

# Tenaga Kefarmasian
ggplot(data, aes(x = "Tenaga Kefarmasian", y = `Tenaga Kefarmasian`)) + 
  geom_boxplot(fill = "orange", color = "darkorange") +
  labs(title = "Distribusi Jumlah Tenaga Kefarmasian per Kabupaten/Kota", x = "Tenaga Kefarmasian", y = "Jumlah") +
  theme_minimal()

# Tenaga Kesehatan Masyarakat
ggplot(data, aes(x = "Tenaga Kesehatan Masyarakat", y = `Tenaga Kesehatan Masyarakat`)) + 
  geom_boxplot(fill = "lightpink", color = "pink") +
  labs(title = "Distribusi Jumlah Tenaga Kesehatan Masyarakat per Kabupaten/Kota", x = "Tenaga Kesehatan Masyarakat", y = "Jumlah") +
  theme_minimal()

# Tenaga Kesehatan Lingkungan
ggplot(data, aes(x = "Tenaga Kesehatan Lingkungan", y = `Tenaga Kesehatan Lingkungan`)) + 
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Distribusi Jumlah Tenaga Kesehatan Lingkungan per Kabupaten/Kota", x = "Tenaga Kesehatan Lingkungan", y = "Jumlah") +
  theme_minimal()

# Tenaga Gizi
ggplot(data, aes(x = "Tenaga Gizi", y = `Tenaga Gizi`)) + 
  geom_boxplot(fill = "lightyellow", color = "yellow") +
  labs(title = "Distribusi Jumlah Tenaga Gizi per Kabupaten/Kota", x = "Tenaga Gizi", y = "Jumlah") +
  theme_minimal()

# gabungin
data_long <- gather(data, key = "Kategori", value = "Jumlah_Tenaga_Kesehatan", 
                    'Perawat', 'Bidan', 'Tenaga Kefarmasian', 'Tenaga Kesehatan Masyarakat', 
                    'Tenaga Kesehatan Lingkungan', 'Tenaga Gizi')
head(data_long, 20)

# visualisasi boxplot gabungan
ggplot(data_long, aes(x = Kategori, y = Jumlah_Tenaga_Kesehatan, fill = Kategori)) + 
  geom_boxplot() +
  labs(title = "Distribusi Jumlah Tenaga Kesehatan per Kabupaten/Kota", 
       x = "Kategori Tenaga Kesehatan", y = "Jumlah Tenaga Kesehatan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# K-Means
# Menentukan jumlah klaster yang optimal menggunakan Elbow Method
wss <- (nrow(data_normalized)-1)*sum(apply(data_normalized, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(data_normalized, centers = i)$tot.withinss)
plot(1:15, wss, type = "b", xlab = "Jumlah Klaster", ylab = "Within-Cluster Sum of Squares")

set.seed(123)
kmeans_result <- kmeans(data_normalized, centers = 4)
data$KMeans_Cluster <- kmeans_result$cluster


# Hierachical
# Menghitung jarak antar kabupaten/kota
dist_matrix <- dist(data_normalized)
hclust_result <- hclust(dist_matrix)
plot(hclust_result)

cutree_result <- cutree(hclust_result, k = 4)
data$HCluster <- cutree_result


# DBSCAN
dbscan_result <- dbscan(data_normalized, eps = 0.5, minPts = 5)
data$DBSCAN_Cluster <- dbscan_result$cluster

ggplot(data, aes(x = Perawat, y = Bidan, color = factor(DBSCAN_Cluster))) +
  geom_point() +
  labs(title = "Hasil Klasterisasi DBSCAN", x = "Jumlah Perawat", y = "Jumlah Bidan")

table(data$DBSCAN_Cluster)
outliers_dbscan <- data[data$DBSCAN_Cluster == 0, ]
print(outliers_dbscan)


# Fuzzy C-Means
set.seed(123)
fcm_result <- cmeans(data_normalized, centers = 4, iter.max = 100)
fuzzy_membership <- as.data.frame(fcm_result$membership)
head(fuzzy_membership)

data$Fuzzy_Cluster <- max.col(fcm_result$membership)

ggplot(data, aes(x = Perawat, y = Bidan, color = factor(Fuzzy_Cluster))) +
  geom_point() +
  labs(title = "Hasil Klasterisasi Fuzzy C-Means", x = "Jumlah Perawat", y = "Jumlah Bidan")

table(data$Fuzzy_Cluster)

# EVALUASI
# 1. silhouette score
# k-means
fviz_silhouette(silhouette_kmeans) +
  labs(title = "Silhouette Plot for K-Means")

#DBSCAN
fviz_silhouette(silhouette_dbscan) +
  labs(title = "Silhouette Plot for DBSCAN")


# RINGKASAN KLASTER
set.seed(123)

# K-Means 
wss <- (nrow(data_normalized)-1)*sum(apply(data_normalized, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(data_normalized, centers = i)$tot.withinss)
plot(1:15, wss, type = "b", xlab = "Jumlah Klaster", ylab = "Within-Cluster Sum of Squares")

kmeans_result <- kmeans(data_normalized, centers = 4)
data$KMeans_Cluster <- kmeans_result$cluster

# Hierarchical 
dist_matrix <- dist(data_normalized)
hclust_result <- hclust(dist_matrix)
plot(hclust_result)

cutree_result <- cutree(hclust_result, k = 4)
data$HCluster <- cutree_result

# DBSCAN
dbscan_result <- dbscan(data_normalized, eps = 0.5, minPts = 5)
data$DBSCAN_Cluster <- dbscan_result$cluster

# Fuzzy C-Means
fcm_result <- cmeans(data_normalized, centers = 4, iter.max = 100)
fuzzy_membership <- as.data.frame(fcm_result$membership)

# memilih klaster dengan keanggotaan tertinggi
data$Fuzzy_Cluster <- max.col(fcm_result$membership)

# Visualisasi Fuzzy C-Means
ggplot(data, aes(x = Perawat, y = Bidan, color = factor(Fuzzy_Cluster))) +
  geom_point() +
  labs(title = "Hasil Klasterisasi Fuzzy C-Means", x = "Jumlah Perawat", y = "Jumlah Bidan")

# Cek ringkasan klaster
summary_kmeans <- data %>%
  group_by(KMeans_Cluster) %>%
  summarise(
    Mean_Perawat = mean(Perawat, na.rm = TRUE), 
    Mean_Bidan = mean(Bidan, na.rm = TRUE),
    Mean_Tenaga_Kesehatan = mean(`Tenaga Kefarmasian`, na.rm = TRUE)
  ) %>%
  mutate(Method = "K-Means")

summary_hclust <- data %>%
  group_by(HCluster) %>%
  summarise(
    Mean_Perawat = mean(Perawat, na.rm = TRUE), 
    Mean_Bidan = mean(Bidan, na.rm = TRUE),
    Mean_Tenaga_Kesehatan = mean(`Tenaga Kefarmasian`, na.rm = TRUE)
  ) %>%
  mutate(Method = "Hierarchical")

summary_dbscan <- data %>%
  group_by(DBSCAN_Cluster) %>%
  summarise(Count = n()) %>%
  mutate(
    Mean_Perawat = NA, 
    Mean_Bidan = NA, 
    Mean_Tenaga_Kesehatan = NA, 
    Method = "DBSCAN"
  )

summary_fcm <- data %>%
  group_by(Fuzzy_Cluster) %>%
  summarise(
    Mean_Perawat = mean(Perawat, na.rm = TRUE), 
    Mean_Bidan = mean(Bidan, na.rm = TRUE),
    Mean_Tenaga_Kesehatan = mean(`Tenaga Kefarmasian`, na.rm = TRUE)
  ) %>%
  mutate(Method = "Fuzzy C-Means")

# menggabungkan ringkasan
combined_summary <- bind_rows(
  summary_kmeans, 
  summary_dbscan, 
  summary_hclust, 
  summary_fcm
)
print(combined_summary)

