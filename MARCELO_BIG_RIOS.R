 ##################################### NMDS PLOT BRAY- CURTIS ##############
library(readxl)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(vegan)
library(ggplot2)
library(indicspecies)


df = read_excel("C:/Users/USER/Desktop/Marcelo_BIG/data_BIG.xlsx", sheet='NMDS')


com = df[,21:68]
env = df[,3:32]

m_com = as.matrix(com)

#nmds code
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
en
plot(nmds)
plot(en)
#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores = as.data.frame(scores(nmds)$sites)

#add 'season' column as before
data.scores$season = df$Group
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)

gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = season), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("red", "orange","green"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04), 
            label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Group")

gg
ggsave("C:/Users/USER/Desktop/plot2.png", plot = gg, width = 10, height = 8, dpi = 300)

