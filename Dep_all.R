## Neurogenesis y TMS datos demográficos de TDM y TLP##
## Por Alan Dávalos ##

## Cargar librerías
library(pacman)
p_load(tidyverse, Hmisc, DataExplorer, ggplot2, magrittr, qwraps2)

## Cargar bases y acomodar variables

Neuro_DEMO_all <- read.csv("/home/alan/Documents/neurogenesis y TMS/Tesis de especialidad/Bases finales/bases_csv/Demo_all.csv", header = T)
Neuro_DEMO_all$Id <- factor(Neuro_DEMO_all$Id)
Neuro_DEMO_all$Grupo <- factor(Neuro_DEMO_all$Grupo,
                           levels = c(1, 2, 3),
                           labels = c("Farmaco", "TMS", "Control"
                           ))


##Descriptivas
table(Neuro_DEMO_all$Grupo,Neuro_DEMO_all$Sexo)
summary(Neuro_DEMO_all)

describe(Neuro_DEMO_all)
bpplot(Neuro_DEMO_all)

plot_missing(Neuro_DEMO_all)
plot_bar(Neuro_DEMO_all)

mystats <-function(x, na.omit=TRUE) {
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean (x)
  n <- length (x)
  s <- sd (x)
  min <- min(x)
  max <- max (x)
  q <- quantile (x, probs=c(0.25, 0.50, 0.75))
  skew <- sum ((x-m)^3/s^3)/n
  kurt <- sum ((x-m)^4/s^4)/n-3
  return(c(n=n, mean=m, stdev=s, min=min, max=max, quantiles=q, kurtosis=kurt, skew=skew))
}

Neuro_DEMO_all$Escolaridad

Neuro_DEMO_all_control <- Neuro_DEMO_all[which(Neuro_DEMO_all$Estudio=="Control"),]
Neuro_DEMO_all_dep <- Neuro_DEMO_all[which(Neuro_DEMO_all$Estudio=="Depresión"),]
Neuro_DEMO_all_tlp <- Neuro_DEMO_all[which(Neuro_DEMO_all$Estudio=="TLP"),]

summary(Neuro_DEMO_all_dep)
describe(Neuro_DEMO_all_dep)


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
ds + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Edad") + labs(y="Sujetos") + ggtitle("Fig 3. Sexo de los sujetos del estudio") 

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
    

data("mtcars")
mtcars2 <-
  dplyr::mutate(mtcars,
                cyl_factor = factor(cyl, 
                                    levels = c(6,4,8),
                                    labels = paste(c(6,4,8), "cylinders")),
                cyl_character = paste(cyl, "cylinders"))

str(mtcars2)

with(mtcars2, table(cyl_factor, cyl_character))

mean_sd(mtcars2$mpg)


