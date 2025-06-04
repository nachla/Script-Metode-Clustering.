#Principal Component Analysis and Factor Analysis 

#import data
data <- read.csv("C:/Users/nachla/ANMUL/DataPCA_1.csv")
print(data)

#Pre-processing
sum(is.na(data))
p <- ncol(data)

#Check KMO
library(psych)
r <- cor(data)
KMO(r)

#Bartlett Test
bartlett.test(data)

#---------Principal Component Analysis-----------

##manual----

scale_data = scale(data)
r = cov(scale_data)
##Menghitung eigenvalue dan eigenvector
pc <- eigen(r)
pc$values

##Menghitung proporsi varians dan kumulatif
library(dplyr)
sumvar <- sum(pc$values)
propvar <- sapply(pc$values, function(x) x/sumvar)*100
cumvar <- data.frame(cbind(pc$values, propvar)) %>% mutate(cum = cumsum(propvar))
colnames(cumvar)[1] <- "value"
rownames(cumvar) <- paste0("PC",c(1:p))
print(cumvar)

#hasilPCA
pc$vectors
scores <- as.matrix(scale_data) %*% pc$vectors
head(scores)

##function in R----
#prcomp
PCA.mod <- prcomp(scale_data)
summary(PCA.mod)       #vs t(cumvar)
PCA.mod$rotation       #vs pc$vectors
head(PCA.mod$x)        #vs head(scores)

#using library FactoMineR
# https://rpubs.com/cahyaalkahfi/pca-with-r
library(FactoMineR)
pca_result <- PCA(scale_data, 
                  scale.unit = TRUE, 
                  graph = FALSE, 
                  ncp=p)     # ncp kita set agar menghasilkan output semua dimensi (default 5)

# menampilkan ringkasan hasil pca
pca_result$eig          # vs print(cumvar)
pca_result$svd$V        # vs pc$vectors
pca_result$ind['coord'] # vs head(scores)


#Visualisasi
library(factoextra)
# membuat scree plot
fviz_eig(pca_result, 
         addlabels = TRUE, 
         ncp = p, 
         barfill = "skyblue", 
         barcolor = "darkblue", 
         linecolor = "red")

#Biplot
fviz_pca_biplot(pca_result, 
                geom.ind = "point", 
                #col.ind = status.ipm, 
                #palette = c("#FC4E07","#E7B800", "#00AFBB"), 
                addEllipses = TRUE, 
                #legend.title = "Kategori"
                )

# correlation circle
contrib_circle <- fviz_pca_var(pca_result, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                               repel = TRUE) + 
  ggtitle("Kontribusi Variabel")
plot(contrib_circle)

#contribution
contrib_v_PC1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 5) + ggtitle("PC1")
contrib_v_PC2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 5) + ggtitle("PC2")
contrib_v_PC3 <- fviz_contrib(pca_result, choice = "var", axes = 3, top = 5) + ggtitle("PC3")

grid.arrange(contrib_v_PC1, contrib_v_PC2, contrib_v_PC3, ncol = 3)

#---------Factor Analysis-----------
varcov = cov(scale_data)
pc = eigen(varcov)
pc$values
pc$vectors
sp = sum(pc$values[1:3])

y = scale_data[,1]%*%t(pc$vectors[1,1])
L1 = sqrt(pc$values[1])*pc$vectors[,1]
L2 = sqrt(pc$values[2])*pc$vectors[,2]
L3 = sqrt(pc$values[3])*pc$vectors[,3]

L = cbind(L1,L2,L3)


# Perform factor analysis 
library(psych) 
fa <- fa(r = scale_data, 
         covar = TRUE,
         nfactors = 3, 
         rotate = "none") 
#summary(fa) 

load <- fa$loadings

plot(load[,1:2],type="n") # set up plot
text(load[,1:2],labels=names(data),cex=.7)

fa.diagram(load)
