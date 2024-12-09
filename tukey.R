library(vegan)
library(readxl)
library(corrplot)
library(factoextra)
library(FactoMineR)

ex4 <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "tukey_metais")
ex5 <-read_excel("C:/Users/USER/Desktop/Marcelo_BIG/BOXPLOT.xlsx", sheet = "tukey_fisico_quimicos")
str(ex4)

#TUKEY + ANOVA ENTRE FISICO_QUIMICOS
summary(anova)
TukeyHSD(anova)

anova<-aov( ex4$`Al (mg/L)`~ex4$Localidade, data=ex4)
summary(anova)
TukeyHSD(anova)

anova<-aov( ex5$`P (µg/mL)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)

anova<-aov( ex5$`COD (mg/L)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)

anova<-aov( ex5$`E. Coli (NMP/100ml)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)

anova<-aov( ex5$`Total Coliforms (NMP/100ml)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)

anova<-aov( ex5$`Vibrio (CFU/ml)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)

anova<-aov( ex5$`Bacterias autotroficas (eventos/ml)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)


anova<-aov( ex5$`Bacterias hetrotroficas (eventos/ml)`~ex5$Localidade, data=ex5)
summary(anova)
TukeyHSD(anova)


library(tidyr)
library(dplyr)
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
setwd("C:/Users/USER/Desktop/Marcelo_BIG")

#wide <-read_excel("data_BIG.xlsx", sheet = "Planilha1")

library(writexl)
library(tidyr)



dfds <-read_excel("teste_T_vera.xlsx", sheet = "teste_t_bact")
dfds2 <-read_excel("teste_T_vera.xlsx", sheet = "teste_t_arg")

dfdsl <- dfds %>% 
  pivot_longer(
    cols = `Bacteroides`:`Leptospira`, 
    names_to = "bact",
    values_to = "value"
  )

dfdsl2 <- dfds2 %>% 
  pivot_longer(
    cols = `Multidrug Resistance Efflux Pumps`:`Mycobacterial MmpL6 membrane protein cluster`, 
    names_to = "arg",
    values_to = "value"
  )

write_xlsx(dfdsl, "dfdsl.xlsx")
write_xlsx(dfdsl2, "delegado22.xlsx")




df_22 <-read_excel("delegado22.xlsx", sheet = "Planilha5")

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


p2 <- ggplot(dataz22, aes(x=group, y=mean, fill=group,alpha=type)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.2, aes(ymin=mean-sem, ymax=mean+sem)) +
  scale_fill_manual(name = "", values = c("red","darkorange2","gold","darkgoldenrod1","#CC0000","#FF9900","lightyellow")) +
  scale_alpha_manual(values = c(1,0.8,0.5)) +
  labs(fill= "", x="", y="Relative abundance",alpha="") + theme_classic() +
  theme(axis.text.x= element_blank())
        #axis.title.y = element_text(size = 18),
        #axis.title.x = element_text(size = 18),
        #axis.text.y = element_text(size = 16),
        

p2
