library(factoextra)
library(FactoMineR)
library(leaps)
require(MASS)
library("PerformanceAnalytics")
library(caret)
library("xlsx")
library("RcmdrMisc")  #pacote para usar colPercent e rowPercent
library(gridExtra)
library(corrplot)
#setwd("C:/Users/USER/Desktop/Pasta3(Peixes_WebDesign)/Peixes_abrolhos/pca_diogo/pca")
setwd("C:/Users/USER/Desktop/Marcelo_BIG")
rm(list=ls())
library(readxl)
ex2 <-read_excel("family_4spp_leo.xlsx",sheet='Planilha1')
ex2
summary(ex2)
#row.names(ex2)<-ex2$Code
row.names(ex2)<-ex2$Rio
ex2 <- ex2[,-1]
summary(ex2)
#ex2[1:53,6:469]

#ex2t  <- as.data.frame(t(ex2))
#remove_cols <- nearZeroVar(ex2, names = TRUE, freqCut = 2, uniqueCut = 20) #corte mais radical
remove_cols <- nearZeroVar(ex2, names = TRUE) #corte mais ameno
remove_cols
# Get all column names from bloodbrain_x: all_cols
all_cols <- names(ex2)
# Remove from data: bloodbrain_x_small
ex2_x_small <- ex2[ , setdiff(all_cols, remove_cols)]


#ex2_x_small[1:53,6:22] #para corte padrao near zero
ex2_x_small[1:53,6:121] #para corte padrao near zero
#ex2_x_small[1:53,6:17] #para corte mais radical




#res.pca <- prcomp(ex2_x_small[1:53,6:121], scale = TRUE)
res.pca <- prcomp(t(ex2))
#res.pca <- prcomp(ex2[1:53,6:469])
#res.pca <- PCA(ex2_x_small[1:53,6:121], graph = FALSE,scale = TRUE)
#res.pca <- PCA(ex2[1:53,6:469], graph = FALSE,scale = TRUE)
#res.pca <- prcomp(ex2[1:53,6:469])
#res.pca <- prcomp(ex2, scale = TRUE)

res.pca
fviz_eig(res.pca)
png("pca__teste1000.png",
    width = 9*300,        # 6 x 300 pixels
    height = 7*300,
    res = 300,            # 300 pixels per inch
    pointsize = 7)        # smaller font size
fviz_pca_var(res.pca, select.var = list(contrib = 20), col.var = "black",repel = TRUE, margins =c(12,9))
dev.off()


pdf("file_pca_teste100.pdf",width=12,height=9,paper='special')
#pdf("file_pca_familia_16stotal.pdf",width=12,height=9,paper='special')
fviz_pca_ind(res.pca,
             #col.ind = ex2$Fish,
             axes = c(2,3),
             #habillage = ex2_x_small$Fish,
             #habillage = ex2_x_small$Portion,
             col.ind = "cos2", # Color by the quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             pointsize = 3,
             #pointshape = ex2_x_small$Fish,
             repel = TRUE     # Avoid text overlapping
)
dev.off()

var <- get_pca_var(res.pca)
head(var$coord, 20)

?fviz_pca_biplot
#pdf("file_pca_filo_biplot_16stotal.pdf",width=8,height=6,paper='special')
png("pca__test1000.png",
    width = 8*300,        # 6 x 300 pixels
    height = 6*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
fviz_pca_biplot(res.pca,
                legend.title = "Local",
                repel = T,
                geom.ind =  c("point", "text"),
                alpha.ind =  1.0,
                col.ind =rownames(t(ex2)),
                #col.ind = ex2_x_small$Fish,
                select.var = list(contrib = 15),
                mean.point = FALSE,
                #col.ind =  ex2_x_small$Fish,
                #fill.ind = ex2_x_small$Portion,
                pointsize = 3,
                #geom.var = c("arrow", "text"),
                col.var = "black",
                alpha.var = "contrib",
                title = NULL
                
)
dev.off()
+ labs(fill = "Species") # Change legend title
#dev.off()



# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
contrib <- res.var$contrib        # Contributions to the PCs
contrib
write.csv2(res.var$contrib,file = "pca_contrib_allVariables.csv")
#write.xlsx(res.var$contrib,file="pca_contrib_allVariables.xlsx",col.names = TRUE, row.names = TRUE)
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2    

# NOVO CCA
install.packages("ca")
library(ca)
cc <- read_excel("family_4spp_leo.xlsx")
g <- cc[,1]
rownames(cc) <- g$Family
cc <- cc[0:50,-1] 



ca.fit <- ca(cc)
ca.plot <- plot(ca.fit)
str(ca.plot)
make.ca.plot.df <- function (ca.plot.obj,
                             row.lab = "Rows",
                             col.lab = "Columns") {
  df <- data.frame(Label = c(rownames(ca.plot.obj$rows),
                             rownames(ca.plot.obj$cols)),
                   Dim1 = c(ca.plot.obj$rows[,1], ca.plot.obj$cols[,1]),
                   Dim2 = c(ca.plot.obj$rows[,2], ca.plot.obj$cols[,2]),
                   Variable = c(rep(row.lab, nrow(ca.plot.obj$rows)),
                                rep(col.lab, nrow(ca.plot.obj$cols))))
  rownames(df) <- 1:nrow(df)
  df
}
ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Sample_Point",
                              col.lab = "Parameter")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Sample_Point", 2, 1)
ca.plot.df
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]
dim.var.percs
library(ggplot2)
library(ggrepel)

p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                            col = Variable, shape = Variable,
                            label = Label, size = Size)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point()

p <- p +
  scale_x_continuous(limits = range(ca.plot.df$Dim1) + c(diff(range(ca.plot.df$Dim1)) * -0.2,
                                                         diff(range(ca.plot.df$Dim1)) * 0.2)) +
  scale_y_continuous(limits = range(ca.plot.df$Dim2) + c(diff(range(ca.plot.df$Dim2)) * -0.2,
                                                         diff(range(ca.plot.df$Dim2)) * 0.2)) +
  scale_size(range = c(4, 7), guide = F) +
  geom_label_repel(show.legend = F, segment.alpha = .5, point.padding = unit(5, "points")) +
  guides(colour = guide_legend(override.aes = list(size = 4)))

p <- p +
  labs(x = paste0("Dimension 1 (", signif(dim.var.percs[1], 3), "%)"),
       y = paste0("Dimension 2 (", signif(dim.var.percs[2], 3), "%)"),
       col = "", shape = "") +
  theme_minimal()
plot(p)
