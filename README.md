# Analisis Klaster Tenaga Kesehatan di Lampung

## Deskripsi
Proyek ini berisi kode R untuk melakukan analisis klaster terhadap data tenaga kesehatan di berbagai kabupaten/kota di Provinsi Lampung dari tahun 2020 hingga 2024. Analisis ini menggunakan beberapa metode klastering, yaitu **K-Means**, **Hierarchical Clustering**, **DBSCAN**, dan **Fuzzy C-Means**, untuk mengelompokkan kabupaten/kota berdasarkan jumlah tenaga kesehatan di berbagai kategori (perawat, bidan, tenaga kefarmasian, tenaga kesehatan masyarakat, tenaga kesehatan lingkungan, dan tenaga gizi).

Kode ini juga menghasilkan visualisasi seperti boxplot, heatmap korelasi, plot PCA, elbow plot, dendrogram, dan silhouette plot untuk membantu memahami distribusi data dan hasil klastering. Selain itu, kode menyediakan statistik deskriptif dan ringkasan klaster untuk setiap metode.

## Prasyarat
Pastikan Anda memiliki perangkat lunak dan paket R berikut terinstal:
- **R** (versi 3.6 atau lebih baru)
- **RStudio** (opsional, untuk antarmuka yang lebih nyaman)
- Paket R:
  ```R
  install.packages(c("readxl", "ggplot2", "cluster", "factoextra", "dbscan", "e1071", "tidyr", "dplyr", "corrplot", "psych"))
  ```

## Struktur File
- `clustering_analysis.R`: Kode utama untuk analisis klaster.
- `DATASET ANMUL KEL 3.xlsx`: File data input (tidak disertakan di repositori; pastikan Anda memiliki file ini di direktori yang sesuai).
- File output (dihasilkan oleh kode):
  - `boxplot_*.png`: Boxplot distribusi tenaga kesehatan.
  - `corrplot_*.png`: Heatmap korelasi antar variabel.
  - `pca_plot_*.png`: Plot PCA untuk visualisasi data.
  - `elbow_plot_*.png`: Elbow plot untuk menentukan jumlah klaster optimal (K-Means).
  - `dendrogram_*.png`: Dendrogram untuk hierarchical clustering.
  - `silhouette_*.png`: Silhouette plot untuk setiap metode klastering.
  - `cluster_*.png`: Visualisasi hasil klastering menggunakan PCA.
  - `knn_plot_*.png`: Plot jarak kNN untuk DBSCAN.
  - `cluster_assignments_*.csv`: File CSV berisi penugasan klaster untuk setiap kabupaten/kota.
  - `ringkasan_klaster_*.csv`: File CSV berisi ringkasan statistik klaster.

## Cara Menjalankan
1. **Siapkan Data**:
   - Pastikan file `DATASET ANMUL KEL 3.xlsx` tersedia di direktori yang benar (misalnya, `C:/Users/nachla/ANMUL/DATASET ANMUL KEL 3.xlsx`). Sesuaikan path file di kode jika perlu.
   - Format data harus memiliki kolom: `Kabupaten/Kota`, `Year`, dan kolom tenaga kesehatan (Perawat, Bidan, Tenaga Kefarmasian, Tenaga Kesehatan Masyarakat, Tenaga Kesehatan Lingkungan, Tenaga Gizi).

2. **Jalankan Kode**:
   - Buka file `clustering_analysis.R` di R atau RStudio.
   - Pastikan semua paket yang diperlukan sudah terinstal.
   - Jalankan kode secara keseluruhan atau per baris untuk melihat hasilnya.
   - Kode akan menghasilkan analisis untuk setiap tahun (2020-2024) dan untuk seluruh periode data.

3. **Output**:
   - Visualisasi akan disimpan sebagai file PNG di direktori kerja Anda.
   - File CSV berisi penugasan klaster dan ringkasan klaster akan disimpan untuk setiap periode analisis.
   - Statistik deskriptif dan silhouette score akan ditampilkan di konsol.

## Penjelasan Kode
Kode ini terdiri dari beberapa bagian utama:
1. **Pemuatan dan Pembersihan Data**:
   - Membaca file Excel menggunakan `readxl`.
   - Memilih kolom yang relevan dan menghapus baris total "Lampung".
   - Menangani missing values dengan mengisi rata-rata per kolom.

2. **Fungsi `perform_clustering`**:
   - Melakukan analisis klaster untuk periode tertentu (per tahun atau seluruh tahun).
   - Menghasilkan statistik deskriptif, visualisasi (boxplot, heatmap, PCA, dll.), dan hasil klastering menggunakan K-Means, Hierarchical Clustering, DBSCAN, dan Fuzzy C-Means.
   - Menyimpan hasil visualisasi dan ringkasan ke file.

3. **Analisis per Tahun dan Seluruh Periode**:
   - Kode menjalankan analisis klaster untuk setiap tahun (2020-2024) dan untuk seluruh data secara keseluruhan.

## Contoh Output
- **Statistik Deskriptif**: Mean, median, min, max, dan standar deviasi untuk setiap kategori tenaga kesehatan.
- **Visualisasi**:
  - Boxplot menunjukkan distribusi jumlah tenaga kesehatan.
  - Heatmap korelasi menunjukkan hubungan antar variabel.
  - Plot PCA untuk visualisasi data dalam ruang dua dimensi.
  - Elbow plot untuk menentukan jumlah klaster optimal (K-Means).
  - Dendrogram untuk hierarchical clustering.
  - Silhouette plot untuk mengevaluasi kualitas klaster.
- **Penugasan Klaster**: Daftar kabupaten/kota dalam setiap klaster untuk setiap metode.
- **Ringkasan Klaster**: Statistik rata-rata per kategori tenaga kesehatan per klaster dan silhouette score.

## Catatan
- Kode ini menggunakan `set.seed(123)` untuk memastikan reproduktibilitas hasil klastering.
- Parameter DBSCAN (`eps` dan `minPts`) diatur secara manual. Anda mungkin perlu menyesuaikan nilai ini berdasarkan plot kNN yang dihasilkan.
- Pastikan direktori kerja Anda memiliki izin untuk menyimpan file output (PNG dan CSV).
- Jika terjadi masalah dengan perangkat grafis (misalnya, error di `corrplot` atau `ggsave`), kode sudah dilengkapi dengan penanganan error untuk memastikan eksekusi tetap berjalan.

## Kontribusi
Jika Anda ingin berkontribusi pada proyek ini:
1. Fork repositori ini.
2. Buat branch baru untuk perubahan Anda (`git checkout -b feature/nama-fitur`).
3. Commit perubahan Anda (`git commit -m "Menambahkan fitur X"`).
4. Push ke branch Anda (`git push origin feature/nama-fitur`).
5. Buat pull request untuk ditinjau.

## Lisensi
Proyek ini dilisensikan di bawah [MIT License](LICENSE). Silakan gunakan dan modifikasi sesuai kebutuhan, dengan tetap memberikan kredit kepada penulis asli.

## Kontak
Untuk pertanyaan atau dukungan, silakan buka isu di repositori ini atau hubungi melalui email (jika disediakan oleh kontributor).
