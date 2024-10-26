library(ggplot2)
library(grid)
library(RColorBrewer)
library(ggpubr)
library(ggbreak)
library(tidyverse)
library(plotly)#Média e desvio padrão
library(plyr)
library(dplyr)
library(patchwork)
library(magrittr)
#scala
library(scales)
library(ggtext)
library(ggsignif)
library(readxl)

setwd("C:/Users/USER/Desktop/Marcelo_BIG")

colif <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "Coliformes")



data3 <- ddply(colif, c("Localidade", "Parametro"), summarise,
               N    = length(NMP),
               mean = mean(NMP),
               sd   = sd(NMP),
               sem = sd(NMP)/sqrt(length(NMP)))

data3$Localidade <- factor(data3$Localidade, levels=c('Japuíba',
                                                      'Centro',
                                                      'Jacuecanga',
                                                      'Jacareí',
                                                      'Perequeaçu',
                                                      'Taquari',
                                                      'Mambucaba',
                                                      'Bracui',
                                                      'Frade',
                                                      'São Roque'))

data3$Parametro <- factor(data3$Parametro, levels=c('E. Coli',
                                                    'Total Coliforms'))

#coli <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "coli")
#e_coli <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "e_coli")

Anova1 <- aov(NMP ~ Localidade, data = colif)
summary(Anova1)
t <- TukeyHSD(Anova1)

stat <- t$Localidade
#stat_T <- stat %>% as.data.frame()

Stat_24 <- stat %>% as.data.frame() %>% 
  mutate(Variable = "NMP") %>% 
  mutate(Group = rownames(stat)) %>%
  rowwise() %>%
  mutate(Group1 = "Jacuecanga") %>%
  mutate(Group2 = "Jacareí") %>%
  mutate(Group3 = "São Roque") %>%
  mutate(Group4 = "Mambucaba") %>%
  mutate(labels = round(`p adj`,20))

#Stat_24$y_pos <- c(200000,190000,0)

#AQUI VAI VIR
signif1 = c("***")
signif2 = c("")

#Escolher as compara??es

STAT1 <- Stat_24[1,]
STAT2 <- Stat_24[2,]
#STAT3 <- Stat_24[3,]



pe <- ggplot(data3, aes(x=Localidade, y=mean, fill=Parametro)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.2, aes(ymin=mean-sem, ymax=mean+sem)) +
  
  labs(fill= NULL, x="", y="NMP/100mL") +
  coord_cartesian(ylim = c(0,5000))+
  scale_fill_hue(direction = 1) +
  geom_hline(aes(yintercept=2500),
             color="red", linetype="dashed",
             size=1) +

  scale_y_continuous(breaks=0:20*1000) +
  scale_fill_manual(name = "Parameters", values = c("skyblue", "orange")) +

  guides(fill = guide_legend("Parameters"), color = guide_legend("Legenda")) +
  
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(hjust=0.5, vjust=0.5, size = 18),
        axis.text.y = element_text(size = 16),
        axis.text.x= element_text( hjust=0.5, vjust=0.5, size = 12)) +
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 12),
        legend.position = 'bottom')
pe

pc <- ggplot(data3, aes(x=Localidade, y=mean, fill=Parametro)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.2, aes(ymin=mean-sem, ymax=mean+sem)) +
  geom_signif(inherit.aes = FALSE, data = STAT1, 
              aes(xmin=Group1, xmax=Group4, annotations=signif1, y_position=200000),
              manual = TRUE)+
  geom_signif(inherit.aes = FALSE, data = STAT2, 
              aes(xmin=Group2, xmax=Group3, annotations=signif2, y_position=190000),
              manual = TRUE)+

  labs(fill= NULL, x="", y="NMP/100mL") +
  coord_cartesian(ylim = c(30000,200000))+
  scale_y_continuous(breaks=0:40*50000) +
  scale_fill_manual(name = "Parameters", values = c("skyblue", "orange")) +

  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x=  element_blank(),
        axis.line.x= element_blank()) +
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 12),
        legend.position = 'bottom')
pc

comb2 <- ggarrange (pc, pe,
                    ncol = 1, nrow =2,
                    common.legend = TRUE, legend = "bottom", align="v", 
                    font.label= list(size=18))
comb2



vibrio <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "vibrio")



data4 <- ddply(vibrio, c("Localidade", "Parametro"), summarise,
               N    = length(CFU),
               mean = mean(CFU),
               sd   = sd(CFU),
               sem = sd(CFU)/sqrt(length(CFU)))

data4$Localidade <- factor(data4$Localidade, levels=c('Japuíba',
                                                      'Centro',
                                                      'Jacuecanga',
                                                      'Jacareí',
                                                      'Perequeaçu',
                                                      'Taquari',
                                                      'Mambucaba',
                                                      'Bracui',
                                                      'Frade',
                                                      'São Roque'))



Anova1 <- aov(CFU ~ Localidade, data = vibrio)
summary(Anova1)
t <- TukeyHSD(Anova1)

stat <- t$Localidade
#stat_T <- stat %>% as.data.frame()

Stat_ <- stat %>% as.data.frame() %>% 
  mutate(Variable = "NMP") %>% 
  mutate(Group = rownames(stat)) %>%
  rowwise() %>%
  mutate(Group0 = "Centro") %>%
  mutate(Group1 = "Japuíba") %>%
  mutate(Group2 = "Jacareí") %>%
  mutate(Group3 = "São Roque") %>%
  mutate(Group4 = "Mambucaba") %>%
  mutate(labels = round(`p adj`,20))

Stat_ <- head (Stat_, n=3)

#Stat_$y_pos <- c(200000,190000,195000)

#AQUI VAI VIR
signif1 = c("****")
signif2 = c("")
signif3 = c("***")

#Escolher as compara??es

STAT1 <- Stat_[1,]
STAT2 <- Stat_[2,]
STAT3 <- Stat_[3,]



pv <- ggplot(data4, aes(x=Localidade, y=mean, fill=Parametro)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.2, aes(ymin=mean-sem, ymax=mean+sem)) +
  
  labs(fill= NULL, x="", y="CFU/mL") +
  coord_cartesian(ylim = c(0,100))+
  scale_fill_hue(direction = 1) +


  scale_y_continuous(breaks=0:20*20) +
  scale_fill_manual(name = "Parameters", values = c("lightgreen")) +

  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(hjust=0.5, vjust=0.5, size = 18),
        axis.text.y = element_text(size = 16),
        axis.text.x= element_text( hjust=0.5, vjust=0.5, size = 12)) +
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 12),
        legend.position = 'bottom')
pv

pt <- ggplot(data4, aes(x=Localidade, y=mean, fill=Parametro)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.2, aes(ymin=mean-sem, ymax=mean+sem)) +
  geom_signif(inherit.aes = FALSE, data = STAT1, 
              aes(xmin=Group0, xmax=Group4, annotations=signif1, y_position=3000),
              manual = TRUE)+
  geom_signif(inherit.aes = FALSE, data = STAT2, 
              aes(xmin=Group2, xmax=Group3, annotations=signif2, y_position=2800),
              manual = TRUE)+
  geom_signif(inherit.aes = FALSE, data = STAT3, 
              aes(xmin=Group0, xmax=Group1, annotations=signif3, y_position=3000),
              manual = TRUE)+
  labs(fill= NULL, x="", y="CFU/mL") +
  coord_cartesian(ylim = c(500,3000))+
  scale_y_continuous(breaks=0:20*500) +
  scale_fill_manual(name = "Parameters", values = c("lightgreen")) +

  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x= element_blank(),
        axis.line.x= element_blank()) +
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 12),
        legend.position = 'bottom')
pt
#ggsave("C:/Users/USER/Desktop/pt2.png", plot = pt, width = 10, height = 8, dpi = 300)
comb5 <- ggarrange (pt, pv,
                    ncol = 1, nrow =2,
                    common.legend = TRUE, legend = "bottom", align="v", 
                    font.label= list(size=18))

comb5

comb3 <- ggarrange (comb2, comb5, 
                    ncol = 2, nrow =1,
                    common.legend = TRUE, legend = "bottom", align="hv", 
                    labels= c("A","B"),
                    font.label= list(size=16))
comb3
#ggsave("C:/Users/USER/Desktop/comb3.tiff", plot = comb3, width = 10, height = 8, dpi = 300)
####### BOXPLOT METAIS, CITOMETRIA E NUTRIENTES ###################### 
library(ggplot2)
# Basic box plot
fsqbox <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "tukey_fisico_quimicos")
metalbox <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "tukey_metais")

met <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "FSQ_metais")
bact <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "bact")
pb <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "pb")
p <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "p")


met$Localidade <- factor(met$Localidade, levels=c('Japuíba',
                                                      'Centro',
                                                      'Jacuecanga',
                                                      'Jacareí',
                                                      'Perequeaçu',
                                                      'Taquari',
                                                      'Mambucaba',
                                                      'Bracui',
                                                      'Frade',
                                                      'São Roque'))

bact$Localidade <- factor(bact$Localidade, levels=c('Japuíba',
                                                  'Centro',
                                                  'Jacuecanga',
                                                  'Jacareí',
                                                  'Perequeaçu',
                                                  'Taquari',
                                                  'Mambucaba',
                                                  'Bracui',
                                                  'Frade',
                                                  'São Roque'))

pb$Localidade <- factor(pb$Localidade, levels=c('Japuíba',
                                                'Centro',
                                                'Jacuecanga',
                                                'Jacareí',
                                                'Perequeaçu',
                                                'Taquari',
                                                'Mambucaba',
                                                'Bracui',
                                                'Frade',
                                                'São Roque'))



p$Localidade <- factor(p$Localidade, levels=c('Japuíba',
                                                'Centro',
                                                'Jacuecanga',
                                                'Jacareí',
                                                'Perequeaçu',
                                                'Taquari',
                                                'Mambucaba',
                                                'Bracui',
                                                'Frade',
                                                'São Roque'))


graf <- ggplot(met, aes(x=Localidade, y=NMP, color=Parametro)) + 
  geom_boxplot(outlier.shape = NA,size = 1,fill="lightgray") +
  scale_color_manual(name = "",values=c("red", "blue")) +
  labs(fill= NULL, y = "mg/L",x="") +
  scale_y_continuous(limits = c(0, 0.5)) + theme_classic() + 
  geom_hline(aes(yintercept=0.1),
             color="red", linetype="dashed",
             size=1) +
  geom_hline(aes(yintercept=0.3),
             color="blue", linetype="dashed",
             size=1)

graf

graf1 <- ggplot(bact, aes(x=Localidade, y=events, color=Parametro)) + 
  geom_boxplot(outlier.shape = NA,size = 1,fill="lightgray") +
  scale_color_manual(name = "",values=c("#999999", "#E69F00"))+#, "#56B4E9") +
  labs(y = "cells/mL",x="",fill="") +
  scale_y_continuous(limits = c(0, 200000)) + theme_classic() + 
  geom_hline(aes(yintercept=50000),
             color="black", linetype="dashed",
             size=1)

graf1

graf2 <- ggplot(pb, aes(x=Localidade, y=NMP, color=Parametro)) + 
  geom_boxplot(outlier.shape = NA,size = 1,fill="lightgray") +
  scale_color_manual(name = "",values=c("#56B4E9"))+#, )
  labs(y = "mg/L",x="",fill="") +
  scale_y_continuous(limits = c(0, 0.02)) + theme_classic() +
  geom_hline(aes(yintercept=0.01),
             color="black", linetype="dashed",
             size=1)

graf2

graf3 <- ggplot(p, aes(x=Localidade, y=NMP, color=Parametro)) + 
  geom_boxplot(outlier.shape = NA,size = 1,fill="lightgray") +
  scale_color_manual(name = "",values=c("#0C710C"))+
  labs(y = "mg/L",x="",fill="") +
  scale_y_continuous(limits = c(0, 1.2)) + theme_classic() +
  geom_hline(aes(yintercept=0.15),
             color="black", linetype="dashed",
             size=1)


graf3

combi <- ggarrange (graf,graf1,graf2,graf3,
                    ncol = 2, nrow =2,
                    common.legend = FALSE, legend = "bottom", align="v",
                    labels= c("C"),
                    font.label= list(size=18))
combi

carb <- ggplot(fsqbox, aes(y=fsqbox$`COD (mg/L)`)) + 
  geom_boxplot(fill='lightblue', color="blue") +
  labs(y = "DOC (mg/L)") 
    #title = "Boxplot de Sepal Length por Species",
   #  x = "Espécies", 

p <- ggplot(fsqbox, aes(y=fsqbox$`P (µg/mL)`)) + 
  geom_boxplot(fill='yellow', color="green")  +
  labs(y = "P (mg/L)") + 
  geom_hline(aes(yintercept=0.15),color="red", linetype="dashed",size=1)

p

aut <- ggplot(fsqbox, aes(y=fsqbox$`Bacterias autotroficas (eventos/ml)`)) + 
  geom_boxplot(fill='gray', color="black")  +
  labs(y = "Autotrophic (cells/mL)") + 
  geom_hline(aes(yintercept=100000),color="red", linetype="dashed",size=1)

het <- ggplot(fsqbox, aes(y=fsqbox$`Bacterias hetrotroficas (eventos/ml)`)) + 
  geom_boxplot(fill='orange', color="black")  +
  labs(y = "Heterotrophic (cells/mL)")

fe <- ggplot(metalbox, aes(y=metalbox$`Fe (mg/L)`)) + 
  geom_boxplot(fill='brown', color="black")  +
  labs(y = "Fe (mg/L)") + 
  geom_hline(aes(yintercept=0.3),color="blue", linetype="dashed",size=1)

al <- ggplot(metalbox, aes(y=metalbox$`Al (mg/L)`)) + 
  geom_boxplot(fill='gold', color="black")  +
  labs(y = "Al (mg/L)") + 
  geom_hline(aes(yintercept=0.2),color="red", linetype="dashed",size=1)

pb <- ggplot(metalbox, aes(y=metalbox$`Pb (mg/L)`)) + 
  geom_boxplot(fill='purple', color="black")  +
  labs(y = "Pb (mg/L)") + 
  geom_hline(aes(yintercept=0.03),color="red", linetype="dashed",size=1)

cu <- ggplot(metalbox, aes(y=metalbox$`Cu (µg/mL)`)) + 
  geom_boxplot(fill='green', color="black")  +
  labs(y = "Cu (mg/L)") + 
  geom_hline(aes(yintercept=0.013),color="red", linetype="dashed",size=1)


combb <- ggarrange (carb,p,aut,het,fe,al,pb,cu,
                    ncol = 4, nrow =2,
                    common.legend = TRUE, legend = "bottom", align="v",
                    labels= c("C"),
                    font.label= list(size=18))
combb
#ggsave("C:/Users/USER/Desktop/comb.png", plot = combb, width = 10, height = 8, dpi = 300)

