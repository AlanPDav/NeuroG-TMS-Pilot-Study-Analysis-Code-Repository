### Bases de datos de proyecto NEUROG-TMS 
### Por Alan Davalos
### Modificada de acuerdo a puntuaciones de hamilton y ELISA

### Load Packages
library(pacman)
p_load(ggpubr, ggplot2, tidyverse)


### Base ELISA

ELISAmod <- read.csv("/home/alan/Documentos/Paper_neurogénesis/Bases neurogenesis/modified_ELISA.csv", header = TRUE)

## Descriptivos, 3 grupos
names(ELISAmod)
summary(ELISAmod)

names(ELISA_LONG)
summary(ELISA_LONG)
#subjects per group
ggplot(ELISAmod, aes(Group, fill=Group)) +
  geom_bar() +
  labs(x = "Group", y = "number of subjects (n)", 
       title = "Subjects per group") 

#Group plots
a <- ggplot(ELISAmod, aes(x = Protein, y = Value, fill = Group))
a + geom_boxplot()

b <- ggplot(ELISAmod, aes(x = Protein, y = Log_transform, fill = Group))
b + geom_boxplot()

##subbases por grupo

ELISA_control <- ELISA[which(ELISA$Group == "control"),]
ELISA_MDD <- ELISA[which(ELISA$Group == "MDD"),]
ELISA_BDP <- ELISA[which(ELISA$Group == "BDP"),]

## Prueba de normalidad
#controles
ggdensity(ELISA_control$IL6_pg_ml, 
          main = "Density plot of IL6 in controls",
          xlab = "IL6")
shapiro.test(ELISA_control$IL6_pg_ml)

ggdensity(ELISA_control$IL8_pg.ml, 
          main = "Density plot of IL8 in controls",
          xlab = "IL8")
shapiro.test(ELISA_control$IL8_pg.ml)

ggdensity(ELISA_control$Trombospondine_pg.ml, 
          main = "Density plot of Trombospondine in controls",
          xlab = "Trombospondine")
shapiro.test(ELISA_control$Trombospondine_pg.ml)

ggdensity(ELISA_control$MCP1_pg.ml, 
          main = "Density plot of MCP1 in controls",
          xlab = "MCP1")
shapiro.test(ELISA_control$MCP1_pg.ml)

ggdensity(ELISA_control$TIM1_pg.ml, 
          main = "Density plot of TIM1 in controls",
          xlab = "TIM1")
shapiro.test(ELISA_control$TIM1_pg.ml)

ggdensity(ELISA_control$ TREM_pg.ml , 
          main = "Density plot of TREM in controls",
          xlab = "TREM")
shapiro.test(ELISA_control$TREM_pg.ml)

#TDM
ggdensity(ELISA_MDD$IL6_pg_ml, 
          main = "Density plot of IL6 in MDD",
          xlab = "IL6")
shapiro.test(ELISA_MDD$IL6_pg_ml)

ggdensity(ELISA_MDD$IL8_pg.ml, 
          main = "Density plot of IL8 in MDD",
          xlab = "IL8")
shapiro.test(ELISA_MDD$IL8_pg.ml)

ggdensity(ELISA_MDD$Trombospondine_pg.ml, 
          main = "Density plot of Trombospondine in MDD",
          xlab = "Trombospondine")
shapiro.test(ELISA_MDD$Trombospondine_pg.ml)

ggdensity(ELISA_MDD$MCP1_pg.ml, 
          main = "Density plot of MCP1 in MDD",
          xlab = "MCP1")
shapiro.test(ELISA_MDD$MCP1_pg.ml)

ggdensity(ELISA_MDD$TIM1_pg.ml, 
          main = "Density plot of TIM1 in MDD",
          xlab = "TIM1")
shapiro.test(ELISA_MDD$TIM1_pg.ml)

ggdensity(ELISA_MDD$ TREM_pg.ml , 
          main = "Density plot of TREM in MDD",
          xlab = "TREM")
shapiro.test(ELISA_MDD$TREM_pg.ml)

## Grafico de boxplot y comparación estadística


#IL6
il6 <- ggplot(ELISAmod, aes(x = Group, y = IL6_pg_ml, fill = Group)) 
il6 + geom_boxplot() +  labs(title ="IL6 pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total') 

t.test(ELISAmod$IL6_pg_ml, ELISAmod$Group)

kruskal.test(IL6_pg_ml ~ Group, data = ELISAmod)
pairwise.wilcox.test(ELISAmod$IL6_pg_ml, ELISAmod$Group,
                     p.adjust.method = "BH")

ggplot(ELISAmod, aes(x=Group, y=IL6_pg_ml, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')

#IL8
b <- ggplot(ELISAmod, aes(x = Group, y = IL8_pg.ml, fill = Group)) 
b + geom_boxplot() +  labs(title ="IL8 pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total') 

kruskal.test(IL8_pg.ml ~ Group, data = ELISAmod)
pairwise.wilcox.test(ELISAmod$IL8_pg.ml, ELISAmod$Group,
                     p.adjust.method = "BH")

ggplot(ELISAmod, aes(x=Group, y=IL8_pg.ml, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')

#Trombospondine
c <- ggplot(ELISAmod, aes(x = Group, y = Trombospondine_pg.ml, fill = Group)) 
c + geom_boxplot() +  labs(title ="Trombospondine pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total')

kruskal.test(Trombospondine_pg.ml ~ Group, data = ELISA)
pairwise.wilcox.test(ELISA$Trombospondine_pg.ml, ELISA$Group,
                     p.adjust.method = "BH")

sin_Tromb <- ggplot(ELISAmod, aes(x = Group, y = Trombospondine_pg.ml, fill = Group)) 
sin_Tromb + geom_boxplot() +  labs(title ="Trombospondine pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total') 

kruskal.test(Trombospondine_pg.ml ~ Group, data = ELISAmod)
pairwise.wilcox.test(sin_outliers$Trombospondine_pg.ml, ELISAmod$Group,
                     p.adjust.method = "BH")
ggplot(ELISAmod, aes(x=Group, y=Trombospondine_pg.ml, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')
#Mcp1
c <- ggplot(ELISA, aes(x = Group, y = MCP1_pg.ml, fill = Group)) 
c + geom_boxplot() +  labs(title ="MCP1 pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total')
kruskal.test(MCP1_pg.ml ~ Group, data = ELISA)
pairwise.wilcox.test(ELISA$MCP1_pg.ml, ELISA$Group,
                     p.adjust.method = "BH")


sin_MCP1 <- ggplot(ELISAmod, aes(x = Group, y = MCP1_pg.ml, fill = Group)) 
sin_MCP1 + geom_boxplot() +  labs(title ="MCP1 pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total') 

kruskal.test(MCP1_pg.ml ~ Group, data = ELISAmod)
pairwise.wilcox.test(sin_outliers$MCP1_pg.ml, ELISAmod$Group,
                     p.adjust.method = "BH")
ggplot(ELISAmod, aes(x=Group, y=MCP1_pg.ml, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')

#TIM1
d <- ggplot(ELISA, aes(x = Group, y = TIM1_pg.ml , fill = Group)) 
d + geom_boxplot() +  labs(title ="TI1M pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total')
kruskal.test(TIM1_pg.ml ~ Group, data = ELISAmod)
pairwise.wilcox.test(ELISAmod$TIM1_pg.ml, ELISAmod$Group,
                     p.adjust.method = "BH")

ggplot(ELISAmod, aes(x=Group, y=TIM1_pg.ml, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')

# Spoiler: salen casi igual que sin retirar a los sujetos, aquí los outliers parecen no interferir
sin_TIM1 <- ggplot(sin_outliers, aes(x = Group, y = TIM1_pg.ml, fill = Group)) 
sin_TIM1 + geom_boxplot() +  labs(title ="TIM1 pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total') 

kruskal.test(TIM1_pg.ml ~ Group, data = sin_outliers)
pairwise.wilcox.test(sin_outliers$TIM1_pg.ml, sin_outliers$Group,
                     p.adjust.method = "BH")

#TREM
e <- ggplot(ELISAmod, aes(x = Group, y = TREM_pg.ml , fill = Group)) 
e + geom_boxplot() +  labs(title ="TREM pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total')

kruskal.test(TREM_pg.ml ~ Group, data = ELISAmod)
pairwise.wilcox.test(ELISAmod$TREM_pg.ml, ELISAmod$Group,
                     p.adjust.method = "BH")


# Spoiler: salen casi igual que sin retirar a los sujetos, aquí los outliers parecen no interferir
sin_TREM <- ggplot(sin_outliers, aes(x = Group, y = TREM_pg.ml, fill = Group)) 
sin_TREM + geom_boxplot() +  labs(title ="TREM pg/ml en Elisa", x = 'Grupo', y = 'Puntaje total') 

kruskal.test(TREM_pg.ml ~ Group, data = sin_outliers)
pairwise.wilcox.test(sin_outliers$TREM_pg.ml, sin_outliers$Group,
                     p.adjust.method = "BH")

### Correlación
CLIN <- read.csv("/home/alan/Documentos/Paper_neurogénesis/Bases neurogenesis/clinical_all.csv", header = TRUE)

ELISA_CLINmod <- merge(ELISAmod, CLIN, by = "ID"); head(ELISA_CLIN)
sinOut_ELISA_CLIN <- merge(sin_outliers, CLIN, by = "ID"); head(sinOut_ELISA_CLIN)

mod_ELISA_control <- ELISA_CLINmod[which(ELISA_CLINmod$Group.x == "control"),]
mod_ELISA_MDD <- ELISA_CLINmod[which(ELISA_CLINmod$Group.x == "MDD"),]
mod_ELISA_BDP <- ELISA_CLINmod[which(ELISA_CLINmod$Group.x == "BDP"),]

names(ELISA_CLINmod)
#IL6
ggplot(ELISA_CLINmod, aes(x=HAM.D.Antes, y=IL6_pg_ml, colour = Group.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS w IL 6", subtitle = "Comparación de los 3 grupos") + xlab("Puntuación de depresión") + ylab("Niveles de IL-6") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Group.x)) + geom_line(stat='smooth', method = "lm", alpha=0.001) 
ggplot(ELISA_CLINmod, aes(x=HAM.D.Antes, y=IL6_pg_ml, colour = Group.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS w IL 6", subtitle = "Comparación de los 3 grupos") + xlab("Puntuación de depresión") + ylab("Niveles de IL-6") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Group.x)) + geom_line(stat='smooth', method = "lm", alpha=0.001) 

corr_elisa <- cor.test(x=mod_ELISA_control$IL6_pg_ml, y=mod_ELISA_control$HAM.D.Antes, method = 'spearman'); corr_elisa
corr_elisa1 <- cor.test(x=mod_ELISA_MDD$IL6_pg_ml, y=mod_ELISA_MDD$HAM.D.Antes, method = 'spearman'); corr_elisa1

library(nlme)
lmm2 <- lme(IL6_pg_ml ~ Group.x*HAM.D.Antes,
            random = ~ 1|ID,
            data = sinOut_ELISA_CLIN); summary(lmm2)

#IL8
ggplot(ELISA_CLIN, aes(x=HAM.D.Antes, y=IL8_pg.ml, colour = Group.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS w IL 6", subtitle = "Comparación de los 3 grupos") + xlab("Puntuación de depresión") + ylab("Niveles de IL-6") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Group.x)) + geom_line(stat='smooth', method = "lm", alpha=0.001) 
ggplot(ELISA_CLINmod, aes(x=HAM.D.Antes, y=IL8_pg.ml, colour = Group.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS w IL 6", subtitle = "Comparación de los 3 grupos") + xlab("Puntuación de depresión") + ylab("Niveles de IL-6") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Group.x)) + geom_line(stat='smooth', method = "lm", alpha=0.001) 

corr_elisa <- cor.test(x=mod_ELISA_control$IL8_pg.ml, y=mod_ELISA_control$HAM.D.Antes, method = 'spearman'); corr_elisa
corr_elisa1 <- cor.test(x=mod_ELISA_MDD$IL8_pg.ml, y=mod_ELISA_MDD$HAM.D.Antes, method = 'spearman'); corr_elisa1

library(nlme)
lmm2 <- lme(IL8_pg.ml ~ Group.x*HAM.D.Antes,
            random = ~ 1|ID,
            data = sinOut_ELISA_CLIN); summary(lmm2)

#Trombospondine_pg.ml
ggplot(ELISA_CLIN, aes(x=HAM.D.Antes, y=Trombospondine_pg.ml, colour = Group.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS w IL 6", subtitle = "Comparación de los 3 grupos") + xlab("Puntuación de depresión") + ylab("Niveles de IL-6") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Group.x)) + geom_line(stat='smooth', method = "lm", alpha=0.001) 
ggplot(ELISA_CLINmod, aes(x=HAM.D.Antes, y=Trombospondine_pg.ml, colour = Group.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS w IL 6", subtitle = "Comparación de los 3 grupos") + xlab("Puntuación de depresión") + ylab("Niveles de Trombospondina") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Group.x)) + geom_line(stat='smooth', method = "lm", alpha=0.001) 

corr_elisa <- cor.test(x=mod_ELISA_control$Trombospondine_pg.ml, y=mod_ELISA_control$Trombospondine_pg.ml, method = 'spearman'); corr_elisa
corr_elisa1 <- cor.test(x=mod_ELISA_MDD$Trombospondine_pg.ml, y=mod_ELISA_MDD$Trombospondine_pg.ml, method = 'spearman'); corr_elisa1

library(nlme)
lmm2 <- lme(IL8_pg.ml ~ Group.x*HAM.D.Antes,
            random = ~ 1|ID,
            data = sinOut_ELISA_CLIN); summary(lmm2)
