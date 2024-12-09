library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)

test <- read_excel("C:/Users/USER/Desktop/SUBMISSAO BIG RIVERS/Marcelo_BIG/teste_revisores.xlsx", sheet="Planilha5")


ggplt <- ggplot(test,aes(x=test$`mg/L`,y=test$`%`,shape=test$FSQ))+
  geom_point()+
  theme_classic()

ggplt

# Plotting multiple Regression Lines
ggplt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=test$FSQ))


test1 <- read_excel("C:/Users/USER/Desktop/SUBMISSAO BIG RIVERS/Marcelo_BIG/teste_revisores.xlsx", sheet="Planilha7")


test1L <- test1 %>% 
  pivot_longer(
    cols = `DOC`:`Cr (10e-2)`, 
    names_to = "FSQ",
    values_to = "mg/L"
  )

test2L <- test1L %>% 
  pivot_longer(
    cols = `soma_SWG`:`soma_ARG`, 
    names_to = "MTG",
    values_to = "%"
  )


#tem que filtrar MGT = SWG,....ect

test3p <- test2L %>% filter(MTG == "soma_SWG")
test3 <- test3p %>% filter(FSQ %in% c("DOC", "NT","P"))


ggplt1 <- ggplot(test3,aes(x=test3$`mg/L`,y=test3$`%`,shape=test3$FSQ))+
  geom_point()+
  theme_classic()

gg1 <- ggplt1+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=test3$FSQ)) +
  
  labs(fill= "", x="mg/L", y="SWG(%)") + theme_classic() +
  theme(axis.text.x= element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))+
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 18),
        legend.position = 'bottom')


test4p <- test2L %>% filter(MTG == "soma_ARG")
test4 <- test4p %>% filter(FSQ %in% c("DOC", "NT","P"))

ggplt2 <- ggplot(test4,aes(x=test4$`mg/L`,y=test4$`%`,shape=test4$FSQ))+
  geom_point()+
  theme_classic()

gg2 <- ggplt2+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                   aes(color=test4$FSQ)) +
  
  labs(fill= "", x="mg/L", y="ARG(%)") + theme_classic() +
  theme(axis.text.x= element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))+
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 18),
        legend.position = 'bottom')

library(grid)
library(RColorBrewer)
library(ggpubr)

comb5 <- ggarrange (gg1, gg2,
                    ncol = 2, nrow =1,
                    common.legend = TRUE, legend = "bottom", align="v", 
                    font.label= list(size=30))

comb5

test5p <- test2L %>% filter(MTG == "soma_MRG")
test5 <- test5p %>% filter(FSQ %in% c("Al", "Fe","Pb","As","Zn","Cd","Co","Cr","Cu"))

ggplt3 <- ggplot(test5,aes(x=test5$`mg/L`,y=test5$`%`,shape=test5$FSQ))+
  geom_point()+
  theme_classic()

gg3 <- ggplt3+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                          aes(color=test5$FSQ)) +
  
  labs(fill= "", x="mg/L", y="%MRG") + theme_classic() +
  theme(axis.text.x= element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))+
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 18),
        legend.position = 'bottom')

gg3

test6p <- test2L %>% filter(MTG == "soma_MRG")
test6 <- test6p %>% filter(FSQ %in% c("DOC","NT","P"))

ggplt4 <- ggplot(test6,aes(x=test6$`mg/L`,y=test6$`%`,shape=test6$FSQ))+
  geom_point()+
  theme_classic()

gg4 <- ggplt4+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                          aes(color=test6$FSQ)) +
  
  labs(fill= "", x="mg/L", y="%MRG") + theme_classic() +
  theme(axis.text.x= element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))+
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 18),
        legend.position = 'bottom')

gg4

test7p <- test2L %>% filter(MTG == "soma_MRG")
test7 <- test7p %>% filter(FSQ %in% c("Fe","Co (10e-3)","Cr (10e-2)"))

ggplt5 <- ggplot(test7,aes(x=test7$`mg/L`,y=test7$`%`,shape=test7$FSQ))+
  geom_point()+
  theme_classic()

gg5 <- ggplt5+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                          aes(color=test7$FSQ)) + 
  scale_color_manual(values = c("Fe" = "purple", "Co (10e-3)" = "orange", "Cr (10e-2)" = "gray")) +
  
  labs(fill= "", x="mg/L", y="MRG(%)") + theme_classic() +
  theme(axis.text.x= element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))+
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 18),
        legend.position = 'bottom')

gg5

combinado <- ggarrange (gg1, gg2, gg5,
                    ncol = 3, nrow =1,
                    common.legend = FALSE, legend = "bottom", align="v", 
                    font.label= list(size=30))

combinado
