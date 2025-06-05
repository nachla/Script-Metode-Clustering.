````markdown
# **Analisis Klaster Tenaga Kesehatan - Dataset ANMUL**

## **Deskripsi Proyek**
Proyek ini bertujuan untuk melakukan analisis klaster terhadap data tenaga kesehatan di beberapa kabupaten/kota di Indonesia menggunakan berbagai metode klastering. Metode yang digunakan meliputi:
- K-Means
- Hierarchical Clustering
- DBSCAN
- Fuzzy C-Means

Analisis dilakukan berdasarkan beberapa kategori tenaga kesehatan, seperti perawat, bidan, tenaga kefarmasian, tenaga kesehatan masyarakat, tenaga kesehatan lingkungan, dan tenaga gizi, selama periode 2020-2024. Data ini dianalisis per tahun serta seluruh tahun secara keseluruhan.

## **Tujuan**
1. Mengelompokkan kabupaten/kota berdasarkan data tenaga kesehatan menggunakan teknik klastering.
2. Menyediakan visualisasi analisis klaster menggunakan boxplot, heatmap korelasi, PCA plot, dan grafik klaster.
3. Menghitung dan memvisualisasikan hasil klaster menggunakan metrik seperti Silhouette Score.

## **Persyaratan**
Pastikan Anda memiliki semua paket R berikut yang digunakan dalam proyek ini:
```R
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
````

## **Deskripsi Dataset**

Dataset yang digunakan adalah file Excel yang berisi data mengenai jumlah tenaga kesehatan di berbagai kabupaten/kota. Kolom-kolom utama dalam dataset meliputi:

* `Kabupaten/Kota`: Nama kabupaten/kota
* `Year`: Tahun data (2020-2024)
* `Tenaga Kesehatan - Perawat`, `Tenaga Kesehatan - Bidan`, dll.: Jumlah tenaga kesehatan dalam berbagai kategori.

## **Langkah-langkah Analisis**

1. **Membaca dan Memproses Data**

   * Membaca dataset dari file Excel.
   * Menghapus baris yang tidak relevan (misalnya: data untuk "Lampung").
   * Menangani nilai yang hilang dengan mengisi nilai missing dengan rata-rata kolom.

2. **Statistik Deskriptif dan Visualisasi**

   * Menghitung statistik deskriptif seperti mean, median, min, max, dan standar deviasi.
   * Membuat visualisasi berupa boxplot dan heatmap korelasi untuk memahami distribusi data.

3. **Pra-Pemrosesan dan Normalisasi**

   * Data dinormalisasi menggunakan metode `scale()` agar lebih cocok untuk analisis klaster.

4. **PCA (Principal Component Analysis)**

   * Menggunakan PCA untuk mereduksi dimensi data dan memvisualisasikan data dalam 2D.

5. **Analisis Klaster**

   * **K-Means Clustering**: Menentukan jumlah klaster optimal menggunakan metode Elbow, dan menghitung silhouette score untuk menilai kualitas klaster.
   * **Hierarchical Clustering**: Membuat dendrogram untuk memvisualisasikan struktur klaster.
   * **DBSCAN**: Menggunakan DBSCAN untuk mendeteksi klaster dengan densitas tinggi dan outlier.
   * **Fuzzy C-Means**: Menggunakan metode Fuzzy C-Means untuk klasterisasi dengan kemungkinan keanggotaan lebih dari satu klaster.

6. **Visualisasi Klaster**

   * Menyediakan visualisasi klaster untuk setiap metode (K-Means, Hierarchical, DBSCAN, Fuzzy C-Means) menggunakan PCA.
   * Menyimpan hasil visualisasi dalam format PNG.

7. **Hasil dan Ringkasan**

   * Menyimpan hasil klasterisasi dan ringkasan klaster dalam file CSV untuk analisis lebih lanjut.
   * Menampilkan daerah-daerah yang termasuk dalam setiap klaster untuk setiap metode klastering.

## **File yang Dihasilkan**

1. **Visualisasi**:

   * Boxplot distribusi tenaga kesehatan.
   * Heatmap korelasi antar kategori tenaga kesehatan.
   * Plot PCA untuk visualisasi data.
   * Elbow plot untuk memilih jumlah klaster optimal.
   * Silhouette plots untuk menilai kualitas klaster.
   * Dendrogram untuk hierarchical clustering.
   * Visualisasi klaster untuk K-Means, Hierarchical, DBSCAN, dan Fuzzy C-Means.

2. **Ringkasan Klaster**:

   * Hasil klasterisasi disimpan dalam file CSV, termasuk informasi tentang daerah yang termasuk dalam setiap klaster serta metrik silhouette untuk masing-masing metode.

3. **Output CSV**:

   * `cluster_assignments_{period}.csv`: Menyimpan penugasan klaster untuk setiap kabupaten/kota.
   * `ringkasan_klaster_{period}.csv`: Menyimpan ringkasan statistik klaster (mean, jumlah, dan silhouette score).

## **Cara Penggunaan**

1. **Persiapkan Data**: Letakkan file Excel yang berisi data di lokasi yang sesuai dengan path `C:/Users/nachla/ANMUL/DATASET ANMUL KEL 3.xlsx`.
2. **Jalankan Script**: Jalankan script di RStudio atau R terminal dengan paket yang telah diinstal.
3. **Analisis Klaster**: Script ini secara otomatis akan melakukan analisis klaster per tahun (2020-2024) serta analisis untuk seluruh data tahun tersebut.
4. **Cek Output**: Cek folder kerja Anda untuk hasil visualisasi dan file CSV yang dihasilkan.

## **Hasil yang Diharapkan**

* Analisis klaster berdasarkan kategori tenaga kesehatan.
* Visualisasi yang jelas tentang distribusi data dan klaster yang terbentuk.
* Menyediakan wawasan tentang pola-pola distribusi tenaga kesehatan di berbagai kabupaten/kota.




