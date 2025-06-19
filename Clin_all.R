## Neurogenesis y TMS datos demográficos de TDM y TLP##
## Por Alan Dávalos ##

## Cargar librerías
library(pacman)
p_load(tidyverse, DataExplorer, ggplot2, magrittr, qwraps2, moonBook, tidyr)

## Cargar bases y acomodar variables

Neuro_clin <- read.csv("/home/alan/Documentos/Paper_neurogénesis/Bases neurogenesis/clinical_all.csv", header = T)
Neuro_clin$ID <- factor(Neuro_clin$ID)

Neuro_clin = Neuro_clin[-c(2,4,5,8,14,34,35,36),]

Neuro_clin_ctr<- Neuro_clin[which(Neuro_clin$Group=="Control"),]
Neuro_clin_tdm <- Neuro_clin[which(Neuro_clin$Group=="MDD"),]
Neuro_clin_tlp <- Neuro_clin[which(Neuro_clin$Group=="BDP"),]

Neuro_DEMO_all <- read.csv("/home/alan/Documentos/Paper_neurogénesis/Bases neurogenesis/Demo_all.csv", header = T)

##Descriptivas

summary(Neuro_clin)

mean(Neuro_clin_ctr$HAM.D.Antes)
sd(Neuro_clin_ctr$HAM.D.Antes)

mean(Neuro_clin_tdm$HAM.D.Antes)
sd(Neuro_clin_tdm$HAM.D.Antes)

mean(Neuro_clin_tlp$HAM.D.Antes)
sd(Neuro_clin_tlp$HAM.D.Antes)

Neuro_clin2 <- select(Neuro_clin, -ID, -Group, -HAM.D.Antes, -HAM.D.Despues)

mytable(Group~HAM.D.Antes+HAM.D.Despues, data=Neuro_clin)

mytable(Group~HAM.D.Antes+HAM.D.Despues, data=Neuro_clin_ctr)
mytable(Group~HAM.D.Antes+HAM.D.Despues, data=Neuro_clin_tdm)
mytable(Group~HAM.D.Antes+HAM.D.Despues, data=Neuro_clin_tlp)

describe(Neuro_DEMO_all)
bpplot(Neuro_DEMO_all)

plot_missing(Neuro_DEMO_all)
plot_bar(Neuro_DEMO_all)


Neuro_DEMO_all$Escolaridad


summary(Neuro_DEMO_all_dep)
describe(Neuro_DEMO_all_dep)

# Calcular la desviación estándar para las columnas seleccionadas en la matriz 'Neuro_clin_tlp'
desviacion_estandar <- sapply(Neuro_clin_tlp[, c("HAM.D.Antes", "HAM.D.Despues", "HAM.A.Antes", "HAM.A.Despues")], sd, na.rm = TRUE)
desviacion_estandar

## Gráficas
#Grupo
Neuro_DEMO_all %>% 
  filter(!is.na(Grupo)) %>% 
  ggplot(aes(x = factor(1), fill = Grupo)) + geom_bar() +
  coord_polar("y") + labs(x="Sujetos") + labs(y="") + ggtitle("Fig 1.  Sujetos por grupo")

#Sexo
Neuro_DEMO_all %>% 
  filter(!is.na(Sexo)) %>% 
  ggplot(aes(x = factor(1), fill = Sexo)) + geom_bar() +
  coord_polar("y") + labs(x="Sujetos") + labs(y="") + ggtitle("Fig 2. Sexo de los participantes")

ds  <- ggplot( Neuro_DEMO_all, aes(x= Sexo))
ds + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Sexo") + labs(y="Sujetos") + ggtitle("Fig 3. Sexo de los sujetos del estudio") 

ggplot (Neuro_DEMO_all_control, aes(x= Sexo)) + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Edad") + labs(y="Sujetos") + ggtitle("Fig 3. Sexo de los sujetos controles") 


## Edad
ed  <- ggplot(Neuro_DEMO_all, aes(x= Edad))
ed + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Edad") + labs(y="Sujetos") + ggtitle("Fig 3. Edad de los sujetos del estudio") 
sd(Neuro_DEMO_all$Edad)

# Edad de grupo de TMS

Neuro_DEMO_all$Grupo

variableedad11 <-c("Edad") 
stats.dataedad11 <- sapply(Neuro_DEMOt[variableedad11], mystats)
stats.dataedad11 <- t(stats.dataedad11)
stats.dataedad11 <- round(stats.dataedad11, digits=2)
stats.dataedad11

# Edad de grupo de fármaco
Neuro_DEMOf$Grupo

variableedad21 <-c("Edad") 
stats.dataedad21 <- sapply(Neuro_DEMOf[variableedad21], mystats)
stats.dataedad21 <- t(stats.dataedad21)
stats.dataedad21 <- round(stats.dataedad2, digits=2)
stats.dataedad21

# Edad de grupo control
Neuro_DEMOc$Grupo

variableedad22 <-c("Edad") 
stats.dataedad22 <- sapply(Neuro_DEMOc[variableedad22], mystats)
stats.dataedad22 <- t(stats.dataedad22)
stats.dataedad22 <- round(stats.dataedad2, digits=2)
stats.dataedad22


#Estado civil

Neuro_DEMO %>% 
  filter(!is.na(Estado_civil)) %>% 
  ggplot(aes(x = factor(1), fill = Estado_civil)) + geom_bar() +
  coord_polar("y") + labs(x="Sujetos") + labs(y="") + ggtitle("Estado civil") 

g  <- ggplot( TMS_DEMO, aes(x= q5_civ))
g + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Estado civil") + labs(y="Sujetos") + ggtitle("Fig 4. Estado civil de los participantes") 

ggplot(Neuro_DEMO, aes(Estado_civil, ..count..)) + geom_bar(aes(fill = Grupo), position = "dodge") + ggtitle("Fig 4. Estado civil de los participantes") 

#Situación laboral

TMS_DEMO %>% 
  filter(!is.na(q6_employeeyr)) %>% 
  ggplot(aes(x = q6_employeeyr)) + geom_bar(color="black", fill="darkolivegreen4") + labs(title = "Fig 5. Situación laboral en los últimos3 años", x = "Situación laboral", y = "Número de sujetos")

ggplot(Neuro_DEMO, aes(Laboral, ..count..)) + geom_bar(aes(fill = Grupo), position = "dodge") + ggtitle("Fig 5. Situación laboral de los participantes") 

## Escolaridad
ggplot(Neuro_DEMO, aes(Escolaridad, ..count..)) + geom_bar(aes(fill = Grupo), position = "dodge") + ggtitle("Fig 6. Escolaridad de los participantes")


## Tabla final de deomgráficos
set.seed(42)


mean_sd(Neuro_DEMO_all$Edad)

our_summarytest <-
  list("Age" =
         list("min" = ~ min(Neuro_DEMO_all$Edad),
              "max" = ~ max(Neuro_DEMO_all$Edad),
              "mean (sd)" = ~ qwraps2::mean_sd(Neuro_DEMO_all$Edad)),
       "Sex" =
         list("Male" = ~ qwraps2::n_perc0(Neuro_DEMO_all$Sexo == "M"),
              "Female"  = ~ qwraps2::n_perc0(Neuro_DEMO_all$Sexo == "F"))
)

test_table <- summary_table(Neuro_DEMO_all, our_summarytest); test_table
print(test_table)

summary(Neuro_DEMO_all)
mytable(Grupo~Edad+Sexo+Escolaridad+Estado_civil+Laboral+Socioeconomico, data=Neuro_DEMO_all)
mytable(Group~HAM.A.Antes+HAM.A.Despues+HAM.D.Antes+HAM.D.Despues+Beck1+Beck2, data=Neuro_clin)

ggplot(Neuro_clin, aes(x=Subgroup, y=HAM.D.Antes, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')  + theme_classic() +  theme(text = element_text(size = 15)) +
  labs(title ="HDRS Antes", x = 'Group', y = 'HDRS') 

ggplot(Neuro_clin, aes(x=Group, y=HAM.D.Despues, fill=Group)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')  + theme_classic() +  theme(text = element_text(size = 15)) +
  labs(title ="HDRS Despues", x = 'Group', y = 'HDRS') 

# Grupo depresión
ggplot(Neuro_clin_tdm, aes(x=Subgroup, y=HAM.D.Antes, fill=Subgroup)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')  + theme_classic() +  theme(text = element_text(size = 15)) +
  labs(title ="HDRS Antes, grupo depresión", x = 'Group', y = 'HDRS') 

ggplot(Neuro_clin_tdm, aes(x=Subgroup, y=HAM.D.Despues, fill=Subgroup)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')  + theme_classic() +  theme(text = element_text(size = 15)) +
  labs(title ="HDRS Despues, grupo depresión", x = 'Group', y = 'HDRS') 

# Grupo TLP
ggplot(Neuro_clin_tlp, aes(x=Subgroup, y=HAM.D.Antes, fill=Subgroup)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')  + theme_classic() +  theme(text = element_text(size = 15)) +
  labs(title ="HDRS Antes, grupo TLP", x = 'Group', y = 'HDRS') 

ggplot(Neuro_clin_tlp, aes(x=Subgroup, y=HAM.D.Despues, fill=Subgroup)) + geom_boxplot(position=position_dodge(0.8)) +
  geom_dotplot(binaxis='y', stackdir='center')  + theme_classic() +  theme(text = element_text(size = 15)) +
  labs(title ="HDRS Despues, grupo TLP", x = 'Group', y = 'HDRS') 

## Kruskall
kruskal.test(HAM.D.Despues ~ Subgroup, data = Neuro_clin_tlp)
pairwise.wilcox.test(ELISA$TIM1_pg.ml, ELISA$Group,
                     p.adjust.method = "BH")

## Fisher
fisher.test(table(Neuro_clin_tdm$Subgroup, Neuro_clin_tdm$HAM.D.Antes))
fisher.test(table(Neuro_clin_tdm$Subgroup, Neuro_clin_tdm$HAM.D.Despues))

fisher.test(table(Neuro_clin_tlp$Subgroup, Neuro_clin_tdm$HAM.D.Antes))
fisher.test(table(Neuro_clin_tlp$Subgroup, Neuro_clin_tdm$HAM.D.Despues))


## COmparar antes y despues
Neuro_clin2 <- Neuro_clin %>% select(c('ID','Subgroup', "Group", "HAM.D.Antes", "HAM.D.Despues")); head((Neuro_clin2))
data_long <- gather(Neuro_clin2, measurement, HDRS, HAM.D.Antes:HAM.D.Despues, factor_key=TRUE); head(data_long)
data_long <- data_long %>% filter(Group != "Control"); data_long

aov_Hdrs <- aov(HDRS~Group*Subgroup*measurement,data_long); aov_Hdrs; summary(aov_Hdrs)
TukeyHSD(aov_Hdrs) #Contraste entre grupos

# Separando grupos
Neuro_clin2_ctr<- data_long[which(data_long$Group=="Control"),]
Neuro_clin2_tdm <- data_long[which(data_long$Group=="MDD"),]
Neuro_clin2_tlp <- data_long[which(data_long$Group=="BDP"),]

kruskal.test(HDRS ~ Subgroup*measurement, data = Neuro_clin2_tdm)

aov_HdrsTDM <- aov(HDRS~Subgroup*measurement,Neuro_clin2_tdm); aov_Hdrs; summary(aov_HdrsTDM)
TukeyHSD(aov_HdrsTDM) #Contraste entre grupos

aov_HdrsTLP <- aov(HDRS~Subgroup*measurement,Neuro_clin2_tlp); aov_HdrsTLP; summary(aov_HdrsTLP)
TukeyHSD(aov_HdrsTLP) #Contraste entre grupos


### Hacer Summary table diferente
  our_summary1 <-
    list("Age" =
           list("min" = ~ min(Neuro_DEMO_all$Edad),
                "max" = ~ max(Neuro_DEMO_all$Edad),
                "mean (sd)" = ~ qwraps2::mean_sd(Neuro_DEMO_all$Edad)),
         "Sex" =
           list("Male" = ~ qwraps2::n_perc0(Neuro_DEMO_all$Sexo == "M"),
                "Female"  = ~ qwraps2::n_perc0(Neuro_DEMO_all$Sexo == "F")),
        "Socio-economic status" =
         list("High" = ~ qwraps2::n_perc0(Neuro_DEMO_all$Socioeconomico == "Alto"),
              "Middle" = ~ qwraps2::n_perc0(Neuro_DEMO_all$Socioeconomico == "Medio"),
              "Low" ~ qwraps2::n_perc0(Neuro_DEMO_all$Socioeconomico == "Bajo")),
       "Psychiatric diagnoses" =
         list("Major depressive disorder" = ~ qwraps2::n_perc0(Neuro_DEMO_all$comorbilidad_psiq1 == "TDM"),
              "Generalized anxiety disorder" = ~ qwraps2::n_perc0(Neuro_DEMO_all$comorbilidad_psiq1== "TAG"),
              "Persistent depressive disorder" = ~ qwraps2::n_perc0(Neuro_DEMO_all$comorbilidad_psiq1 == "Distimia"), 
              "Panic disorder" = ~ qwraps2::n_perc0(Neuro_DEMO_all$comorbilidad_psiq2 == "Trastorno de angustia"), 
              "Borderline personality disorder" = ~ qwraps2::n_perc0(Neuro_DEMO_all$Estudio == "TLP"),
              "None" = ~ qwraps2::n_perc0(Neuro_DEMO_all$comorbilidad_psiq1 == "No"))
  )

test_table2 <- summary_table(Neuro_DEMO_all, our_summary1)
test_table2

https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html
    



