# Analiza danych PCA

# Wczytanie danych

heart <- read.csv("heart.csv")

heart.pr <- prcomp(heart[c(2:length(colnames(heart)))], center = TRUE, scale = TRUE)

# wyznaczenie wariancji zawartej w dwóch największych składnikach wiodących
pc1_var <- as.double(summary(heart.pr)$importance[,1][2])
pc2_var <- as.double(summary(heart.pr)$importance[,2][2])
print(sprintf('Wariancja danych zawarta w pierwszym składniku wiodącym: %s%%', format(round(pc1_var, 2), nsmall = 2)))
print(sprintf('Wariancja danych zawarta w drugim składniku wiodącym: %s%%', format(round(pc2_var, 2), nsmall = 2)))


# Wykres wartości własnych dla 15 największych składników wiodących
pdf('docs/wartosci_wlasne.pdf')
screeplot(heart.pr, type = "l", npcs = 15, main = "Wykres wartości własnych dla 15 największych składników wiodących")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Wartość własna = 1"),
       col=c("red"), lty=5, cex=0.6)
dev.off()

# kumulacyjny wykres wariancji
cumpro <- cumsum(heart.pr$sdev^2 / sum(heart.pr$sdev^2))
pdf('docs/wariancja_kum.pdf')
plot(cumpro[0:45], xlab = "Składnik wiodący", ylab = "Wariancja wyjaśniona", main = "Kumulatywny wykres wariancji")
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)
dev.off()

x_label <- sprintf('składnik wiodący 1: (%s%%)', format(round(pc1_var, 2), nsmall = 2))
y_label <- sprintf('składnik wiodący 2: (%s%%)', format(round(pc2_var, 2), nsmall = 2))
plot_title <- 'Dane po redukcji rozmiaru do dwóch wymiarów'
plot(heart.pr$x[,1],heart.pr$x[,2], xlab=x_label, ylab = y_label, main = plot_title)


# to wrzuććie do konsoli, ze skryptu mi się nie rysuje a ładne jest
library("factoextra")
pdf('docs/klasy.pdf')
fviz_pca_ind(heart.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = heart$OVERALL_DIAGNOSIS, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnoza") +
  ggtitle("Dane z zaznaczonymi klasami") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

