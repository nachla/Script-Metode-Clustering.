library(readxl)
library(ggplot2)
library(cluster)
library(factoextra)
library(dbscan)
library(e1071)
library(tidyr)
library(dplyr)
library(corrplot)
library(psych)

data <- read_excel("C:/Users/DATASET ANMUL KEL 3.xlsx")

# Membersihkan data: pilih kolom yang relevan dan hapus baris total "Lampung"
data <- data %>%
  select(`Kabupaten/Kota`, `Year`, 
         `Tenaga Kesehatan - Perawat`, 
         `Tenaga Kesehatan - Bidan`, 
         `Tenaga Kesehatan - Tenaga Kefarmasian`, 
         `Tenaga Kesehatan - Tenaga Kesehatan Masyarakat`, 
         `Tenaga Kesehatan - Tenaga Kesehatan Lingkungan`, 
         `Tenaga Kesehatan - Tenaga Gizi`) %>%
  filter(`Kabupaten/Kota` != "Lampung")

# Cek missing values
print("Jumlah missing values:")
print(sum(is.na(data)))

# Mengisi missing values dengan rata-rata per kolom (jika ada)
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Fungsi untuk analisis klaster per tahun atau seluruh tahun
perform_clustering <- function(data_subset, period_name) {
  cat("\n=== Analisis Klaster untuk", period_name, "===\n")
  
  # Pilih kolom numerik untuk analisis
  data_numeric <- data_subset %>%
    select(`Tenaga Kesehatan - Perawat`, 
           `Tenaga Kesehatan - Bidan`, 
           `Tenaga Kesehatan - Tenaga Kefarmasian`, 
           `Tenaga Kesehatan - Tenaga Kesehatan Masyarakat`, 
           `Tenaga Kesehatan - Tenaga Kesehatan Lingkungan`, 
           `Tenaga Kesehatan - Tenaga Gizi`)
  
  # Analisis Statistik Deskriptif
  summary_stats <- data.frame(
    Kategori = colnames(data_numeric),
    Mean = sapply(data_numeric, mean, na.rm = TRUE),
    Median = sapply(data_numeric, median, na.rm = TRUE),
    Min = sapply(data_numeric, min, na.rm = TRUE),
    Max = sapply(data_numeric, max, na.rm = TRUE),
    SD = sapply(data_numeric, sd, na.rm = TRUE)
  )
  print("Statistik Deskriptif:")
  print(summary_stats)
  
  # Visualisasi: Boxplot
  data_long <- pivot_longer(data_subset, 
                            cols = c("Tenaga Kesehatan - Perawat", 
                                     "Tenaga Kesehatan - Bidan", 
                                     "Tenaga Kesehatan - Tenaga Kefarmasian", 
                                     "Tenaga Kesehatan - Tenaga Kesehatan Masyarakat", 
                                     "Tenaga Kesehatan - Tenaga Kesehatan Lingkungan", 
                                     "Tenaga Kesehatan - Tenaga Gizi"),
                            names_to = "Kategori", 
                            values_to = "Jumlah")
  boxplot <- ggplot(data_long, aes(x = Kategori, y = Jumlah, fill = Kategori)) + 
    geom_boxplot() +
    labs(title = paste("Distribusi Tenaga Kesehatan -", period_name), 
         x = "Kategori", y = "Jumlah") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # Save boxplot to file to avoid interactive device conflicts
  ggsave(paste0("boxplot_", period_name, ".png"), plot = boxplot, width = 8, height = 6)
  # Optionally display in RStudio (comment out if causing issues)
  # print(boxplot)
  
  # Visualisasi: Heatmap Korelasi
  # Clear all graphics devices to prevent invalid state
  while (!is.null(dev.list())) dev.off()
  # Initialize PNG device
  png(paste0("corrplot_", period_name, ".png"), width = 800, height = 600)
  tryCatch({
    corr_matrix <- cor(data_numeric, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper", 
             title = paste("Heatmap Korelasi -", period_name), 
             tl.cex = 0.6, mar = c(0, 0, 4, 0))
  }, error = function(e) {
    cat("Error in corrplot for", period_name, ":", conditionMessage(e), "\n")
  }, finally = {
    dev.off() # Ensure device is closed even if error occurs
  })
  
  # Pra-pemrosesan: Normalisasi
  data_normalized <- scale(data_numeric)
  
  # PCA untuk reduksi dimensi (opsional untuk visualisasi)
  pca_result <- prcomp(data_normalized, scale. = TRUE)
  print("Varian PCA:")
  print(summary(pca_result))
  pca_plot <- fviz_pca_ind(pca_result, geom = "point", 
                           title = paste("PCA Plot -", period_name)) +
    theme_minimal()
  ggsave(paste0("pca_plot_", period_name, ".png"), plot = pca_plot, width = 8, height = 6)
  
  # K-Means Clustering
  set.seed(123)
  wss <- numeric(10)
  for (i in 1:10) wss[i] <- sum(kmeans(data_normalized, centers = i, nstart = 25)$tot.withinss)
  # Save elbow plot
  png(paste0("elbow_plot_", period_name, ".png"), width = 600, height = 400)
  plot(1:10, wss, type = "b", 
       xlab = "Jumlah Klaster", 
       ylab = "Within-Cluster Sum of Squares",
       main = paste("Elbow Method -", period_name))
  dev.off()
  
  kmeans_result <- kmeans(data_normalized, centers = 4, nstart = 25)
  data_subset$KMeans_Cluster <- kmeans_result$cluster
  silhouette_kmeans <- silhouette(kmeans_result$cluster, dist(data_normalized))
  kmeans_sil_score <- mean(silhouette_kmeans[, 3])
  print(paste("Silhouette Score K-Means:", kmeans_sil_score))
  silhouette_kmeans_plot <- fviz_silhouette(silhouette_kmeans) + 
    labs(title = paste("Silhouette Plot K-Means -", period_name))
  ggsave(paste0("silhouette_kmeans_", period_name, ".png"), plot = silhouette_kmeans_plot, width = 6, height = 4)
  
  # Hierarchical Clustering
  dist_matrix <- dist(data_normalized, method = "euclidean")
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  # Save dendrogram
  png(paste0("dendrogram_", period_name, ".png"), width = 600, height = 400)
  plot(hclust_result, main = paste("Dendrogram Hierarchical -", period_name))
  dev.off()
  data_subset$HCluster <- cutree(hclust_result, k = 4)
  silhouette_hclust <- silhouette(data_subset$HCluster, dist_matrix)
  hclust_sil_score <- mean(silhouette_hclust[, 3])
  print(paste("Silhouette Score Hierarchical:", hclust_sil_score))
  silhouette_hclust_plot <- fviz_silhouette(silhouette_hclust) + 
    labs(title = paste("Silhouette Plot Hierarchical -", period_name))
  ggsave(paste0("silhouette_hclust_", period_name, ".png"), plot = silhouette_hclust_plot, width = 6, height = 4)
  
  # DBSCAN
  # Save kNN distance plot
  png(paste0("knn_plot_", period_name, ".png"), width = 600, height = 400)
  kNNdistplot(data_normalized, k = 5)
  abline(h = 0.7, col = "red")
  dev.off()
  dbscan_result <- dbscan(data_normalized, eps = 0.7, minPts = 3)
  data_subset$DBSCAN_Cluster <- dbscan_result$cluster
  print("Distribusi klaster DBSCAN:")
  print(table(dbscan_result$cluster))
  non_outlier_indices <- which(dbscan_result$cluster != 0)
  num_clusters <- length(unique(dbscan_result$cluster[dbscan_result$cluster != 0]))
  if (num_clusters > 1 && length(non_outlier_indices) > 1) {
    silhouette_dbscan <- silhouette(dbscan_result$cluster[non_outlier_indices], 
                                    dist(data_normalized[non_outlier_indices, ]))
    dbscan_sil_score <- mean(silhouette_dbscan[, 3])
    print(paste("Silhouette Score DBSCAN:", dbscan_sil_score))
    silhouette_dbscan_plot <- fviz_silhouette(silhouette_dbscan) + 
      labs(title = paste("Silhouette Plot DBSCAN -", period_name))
    ggsave(paste0("silhouette_dbscan_", period_name, ".png"), plot = silhouette_dbscan_plot, width = 6, height = 4)
  } else {
    print("Tidak cukup klaster non-outlier untuk silhouette plot DBSCAN")
    dbscan_sil_score <- NA
  }
  
  # Fuzzy C-Means
  set.seed(123)
  fcm_result <- cmeans(data_normalized, centers = 4, iter.max = 100, m = 2)
  data_subset$Fuzzy_Cluster <- max.col(fcm_result$membership)
  silhouette_fcm <- silhouette(data_subset$Fuzzy_Cluster, dist_matrix)
  fcm_sil_score <- mean(silhouette_fcm[, 3])
  print(paste("Silhouette Score Fuzzy C-Means:", fcm_sil_score))
  silhouette_fcm_plot <- fviz_silhouette(silhouette_fcm) + 
    labs(title = paste("Silhouette Plot Fuzzy C-Means -", period_name))
  ggsave(paste0("silhouette_fcm_", period_name, ".png"), plot = silhouette_fcm_plot, width = 6, height = 4)
  
  # Visualisasi Klaster (menggunakan PCA untuk 2D)
  kmeans_cluster_plot <- fviz_cluster(list(data = data_normalized, cluster = kmeans_result$cluster), 
                                      geom = "point", main = paste("K-Means Clustering -", period_name))
  ggsave(paste0("cluster_kmeans_", period_name, ".png"), plot = kmeans_cluster_plot, width = 6, height = 4)
  
  hclust_cluster_plot <- fviz_cluster(list(data = data_normalized, cluster = data_subset$HCluster), 
                                      geom = "point", main = paste("Hierarchical Clustering -", period_name))
  ggsave(paste0("cluster_hclust_", period_name, ".png"), plot = hclust_cluster_plot, width = 6, height = 4)
  
  dbscan_cluster_plot <- fviz_cluster(list(data = data_normalized, cluster = dbscan_result$cluster), 
                                      geom = "point", main = paste("DBSCAN Clustering -", period_name))
  ggsave(paste0("cluster_dbscan_", period_name, ".png"), plot = dbscan_cluster_plot, width = 6, height = 4)
  
  fcm_cluster_plot <- fviz_cluster(list(data = data_normalized, cluster = data_subset$Fuzzy_Cluster), 
                                   geom = "point", main = paste("Fuzzy C-Means Clustering -", period_name))
  ggsave(paste0("cluster_fcm_", period_name, ".png"), plot = fcm_cluster_plot, width = 6, height = 4)
  
  # Menampilkan daerah per klaster
  cluster_assignments <- data_subset %>%
    select(`Kabupaten/Kota`, KMeans_Cluster, HCluster, DBSCAN_Cluster, Fuzzy_Cluster)
  
  # K-Means
  cat("\nDaerah per Klaster K-Means:\n")
  kmeans_regions <- data_subset %>%
    group_by(KMeans_Cluster) %>%
    summarise(Daerah = paste(`Kabupaten/Kota`, collapse = ", "))
  print(kmeans_regions)
  
  # Hierarchical
  cat("\nDaerah per Klaster Hierarchical:\n")
  hclust_regions <- data_subset %>%
    group_by(HCluster) %>%
    summarise(Daerah = paste(`Kabupaten/Kota`, collapse = ", "))
  print(hclust_regions)
  
  # DBSCAN
  cat("\nDaerah per Klaster DBSCAN:\n")
  dbscan_regions <- data_subset %>%
    group_by(DBSCAN_Cluster) %>%
    summarise(Daerah = paste(`Kabupaten/Kota`, collapse = ", "))
  print(dbscan_regions)
  
  # Fuzzy C-Means
  cat("\nDaerah per Klaster Fuzzy C-Means:\n")
  fcm_regions <- data_subset %>%
    group_by(Fuzzy_Cluster) %>%
    summarise(Daerah = paste(`Kabupaten/Kota`, collapse = ", "))
  print(fcm_regions)
  
  # Simpan semua penugasan klaster ke CSV
  write.csv(cluster_assignments, paste0("cluster_assignments_", period_name, ".csv"), row.names = FALSE)
  
  # Ringkasan Klaster
  summary_kmeans <- data_subset %>%
    group_by(KMeans_Cluster) %>%
    summarise(
      Mean_Perawat = mean(`Tenaga Kesehatan - Perawat`, na.rm = TRUE),
      Mean_Bidan = mean(`Tenaga Kesehatan - Bidan`, na.rm = TRUE),
      Mean_Tenaga_Kefarmasian = mean(`Tenaga Kesehatan - Tenaga Kefarmasian`, na.rm = TRUE),
      Count = n()
    ) %>%
    mutate(Method = "K-Means", Silhouette_Score = kmeans_sil_score)
  
  summary_hclust <- data_subset %>%
    group_by(HCluster) %>%
    summarise(
      Mean_Perawat = mean(`Tenaga Kesehatan - Perawat`, na.rm = TRUE),
      Mean_Bidan = mean(`Tenaga Kesehatan - Bidan`, na.rm = TRUE),
      Mean_Tenaga_Kefarmasian = mean(`Tenaga Kesehatan - Tenaga Kefarmasian`, na.rm = TRUE),
      Count = n()
    ) %>%
    mutate(Method = "Hierarchical", Silhouette_Score = hclust_sil_score)
  
  summary_dbscan <- data_subset %>%
    group_by(DBSCAN_Cluster) %>%
    summarise(
      Mean_Perawat = mean(`Tenaga Kesehatan - Perawat`, na.rm = TRUE),
      Mean_Bidan = mean(`Tenaga Kesehatan - Bidan`, na.rm = TRUE),
      Mean_Tenaga_Kefarmasian = mean(`Tenaga Kesehatan - Tenaga Kefarmasian`, na.rm = TRUE),
      Count = n()
    ) %>%
    mutate(Method = "DBSCAN", Silhouette_Score = ifelse(exists("dbscan_sil_score"), dbscan_sil_score, NA))
  
  summary_fcm <- data_subset %>%
    group_by(Fuzzy_Cluster) %>%
    summarise(
      Mean_Perawat = mean(`Tenaga Kesehatan - Perawat`, na.rm = TRUE),
      Mean_Bidan = mean(`Tenaga Kesehatan - Bidan`, na.rm = TRUE),
      Mean_Tenaga_Kefarmasian = mean(`Tenaga Kesehatan - Tenaga Kefarmasian`, na.rm = TRUE),
      Count = n()
    ) %>%
    mutate(Method = "Fuzzy C-Means", Silhouette_Score = fcm_sil_score)
  
  combined_summary <- bind_rows(summary_kmeans, summary_hclust, summary_dbscan, summary_fcm)
  print("Ringkasan Klaster:")
  print(combined_summary)
  
  # Simpan ringkasan
  write.csv(combined_summary, paste0("ringkasan_klaster_", period_name, ".csv"), row.names = FALSE)
  
  return(data_subset)
}

# Analisis per tahun (2020-2024)
for (year in 2020:2024) {
  data_year <- data %>% filter(Year == year)
  perform_clustering(data_year, paste("Tahun", year))
}

# Analisis seluruh tahun
perform_clustering(data, "Seluruh Tahun")
