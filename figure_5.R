



################## CORREÇÕES EM 20-09 FIGURE 4 #######################

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
library(readxl)
setwd("C:/Users/USER/Desktop/SUBMISSAO BIG RIVERS/Marcelo_BIG")

#wide <-read_excel("data_BIG.xlsx", sheet = "Planilha1")

library(writexl)
library(tidyr)
library(ggpattern)
install.packages("ggpattern")

install.packages("remotes")
 remotes::install_github("trevorld/ggpattern")

#dfds <-read_excel("teste_T_vera.xlsx", sheet = "teste_t_bact")
#dfds2 <-read_excel("teste_T_vera.xlsx", sheet = "teste_t_arg")

#dfdsl <- dfds %>% 
#  pivot_longer(
#    cols = `Bacteroides`:`Leptospira`, 
#    names_to = "bact",
#    values_to = "value"
#  )

#dfdsl2 <- dfds2 %>% 
#  pivot_longer(
#    cols = `Multidrug Resistance Efflux Pumps`:`Mycobacterial MmpL6 membrane protein cluster`, 
#    names_to = "arg",
#    values_to = "value"
#  )


#write_xlsx(dfdsl2, "dataframe22.xlsx")




df_22 <-read_excel("dataframe22.xlsx", sheet = "Planilha5")

df22long <- df_22 %>% 
  pivot_longer(
    cols = `ARG`:`SWG`, 
    names_to = "type",
    values_to = "value"
  )

dataz22 <- ddply(df22long, c("group", "type"), summarise,
               N    = length(value),
               mean = mean(value),
               sd   = sd(value),
               sem = sd(value)/sqrt(length(value)))


dataz22$type <- factor(dataz22$type, levels=c('SWG',
                                          'ARG',
                                          'MRG'))


write_xlsx(dataz22, "DATAO.xlsx")

p2 <- ggplot(dataz22, aes(x=group, y=mean, fill=group,alpha=type)) +
#  geom_bar(position=position_dodge(), stat="identity") +
  geom_bar_pattern(position="stack",stat="identity",
                   mapping=aes(pattern=condition)) +
  geom_errorbar(position=position_dodge(.9), width=.2, aes(ymin=mean-sem, ymax=mean+sem)) +
  scale_fill_manual(name = "", values = c("red","darkorange2","#FFE200")) +
  scale_alpha_manual(values = c(1,0.55,0.15)) +

  labs(fill= "", x="", y="Relative abundance",alpha="") + theme_classic() +
  theme(axis.text.x= element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
        

p2

ggplot(f1, aes(x = geneID, y = logfc, fill = comp)) +
  geom_col_pattern(
    aes(pattern = exp),
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.01,
    position = position_dodge2(preserve = 'single'),
  ) +
  scale_pattern_manual(
    values = c("none", "stripe"),
    guide = guide_legend(override.aes = list(fill = "grey70")) # <- make lighter
  ) +
  scale_fill_discrete(
    guide = guide_legend(override.aes = list(pattern = "none")) # <- hide pattern
  )