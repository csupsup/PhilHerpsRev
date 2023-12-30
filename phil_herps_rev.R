---
title: "PH Herp Review"
author: "C.E. Supsup"
date: "30 January 2023"
output:
  html_document:
    df_print: paged
  pdf_document: default
indent: no
---
```{r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
```

**1. Load packages**
```{r, load packages}
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(lemon)
library(ggpubr)
library(tidyr)
library(sf)
library(rgdal)
library(cowplot)
library(jtools)
library(caret)
```

**2. Read data**
```{r, read data}
#read data
ph.herps <- read.csv("ph_herps_biblio_30Jan2023.csv")
ph.herps <- ph.herps %>% replace(is.na(.), 0)

#summarise data per study type
#amphibians
amphi <- mutate(ph.herps, amph.check = if_else(Amphibians==1 & Checklist==1 , "1", "0"))
amphi <- mutate(amphi, amph.newsp = if_else(Amphibians==1 & NewSp==1 , "1", "0"))
amphi <- mutate(amphi, amph.ecolnat = if_else(Amphibians==1 & EcolNat==1 , "1", "0"))
amphi <- mutate(amphi, amph.epb = if_else(Amphibians==1 & EvolPhyBiog==1 , "1", "0"))
amphi <- mutate(amphi, amph.conserv = if_else(Amphibians==1 & Conserv==1 , "1", "0"))

check <- sum(as.numeric(amphi$amph.check))
newsp <- sum(as.numeric(amphi$amph.newsp))
ecolnat <- sum(as.numeric(amphi$amph.ecolnat))
epb <- sum(as.numeric(amphi$amph.epb))
conserv <- sum(as.numeric(amphi$amph.conserv))

Taxa <- c("Amphibians", "Amphibians", "Amphibians", "Amphibians", "Amphibians")
Types <- c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

amphi.sum <- data.frame(Taxa, Types, Sum)

#lizards
liz <- mutate(ph.herps, liz.check = if_else(Lizards==1 & Checklist==1 , "1", "0"))
liz <- mutate(liz, liz.newsp = if_else(Lizards==1 & NewSp==1 , "1", "0"))
liz <- mutate(liz, liz.ecolnat = if_else(Lizards==1 & EcolNat==1 , "1", "0"))
liz <- mutate(liz, liz.epb = if_else(Lizards==1 & EvolPhyBiog==1 , "1", "0"))
liz <- mutate(liz, liz.conserv = if_else(Lizards==1 & Conserv==1 , "1", "0"))

check <- sum(as.numeric(liz$liz.check))
newsp <- sum(as.numeric(liz$liz.newsp))
ecolnat <- sum(as.numeric(liz$liz.ecolnat))
epb <- sum(as.numeric(liz$liz.epb))
conserv <- sum(as.numeric(liz$liz.conserv))

Taxa <- c("Lizards", "Lizards", "Lizards", "Lizards", "Lizards")
Types <- c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

liz.sum <- data.frame(Taxa, Types, Sum)

#Snakes
snk <- mutate(ph.herps, snk.check = if_else(Snakes==1 & Checklist==1 , "1", "0"))
snk <- mutate(snk, snk.newsp = if_else(Snakes==1 & NewSp==1 , "1", "0"))
snk <- mutate(snk, snk.ecolnat = if_else(Snakes==1 & EcolNat==1 , "1", "0"))
snk <- mutate(snk, snk.epb = if_else(Snakes==1 & EvolPhyBiog==1 , "1", "0"))
snk <- mutate(snk, snk.conserv = if_else(Snakes==1 & Conserv==1 , "1", "0"))

check <- sum(as.numeric(snk$snk.check))
newsp <- sum(as.numeric(snk$snk.newsp))
ecolnat <- sum(as.numeric(snk$snk.ecolnat))
epb <- sum(as.numeric(snk$snk.epb))
conserv <- sum(as.numeric(snk$snk.conserv))

Taxa <- c("Snakes", "Snakes", "Snakes", "Snakes", "Snakes")
Types <- c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

snk.sum <- data.frame(Taxa, Types, Sum)

#Crocodiles
croc <- mutate(ph.herps, croc.check = if_else(Crocodiles==1 & Checklist==1 , "1", "0"))
croc <- mutate(croc, croc.newsp = if_else(Crocodiles==1 & NewSp==1 , "1", "0"))
croc <- mutate(croc, croc.ecolnat = if_else(Crocodiles==1 & EcolNat==1 , "1", "0"))
croc <- mutate(croc, croc.epb = if_else(Crocodiles==1 & EvolPhyBiog==1 , "1", "0"))
croc <- mutate(croc, croc.conserv = if_else(Crocodiles==1 & Conserv==1 , "1", "0"))

check <- sum(as.numeric(croc$croc.check))
newsp <- sum(as.numeric(croc$croc.newsp))
ecolnat <- sum(as.numeric(croc$croc.ecolnat))
epb <- sum(as.numeric(croc$croc.epb))
conserv <- sum(as.numeric(croc$croc.conserv))

Taxa <- c("Crocodiles", "Crocodiles", "Crocodiles", "Crocodiles", "Crocodiles")
Types <- c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

croc.sum <- data.frame(Taxa, Types, Sum)

#Turtles
turt <- mutate(ph.herps, turt.check = if_else(Turtles==1 & Checklist==1 , "1", "0"))
turt <- mutate(turt, turt.newsp = if_else(Turtles==1 & NewSp==1 , "1", "0"))
turt <- mutate(turt, turt.ecolnat = if_else(Turtles==1 & EcolNat==1 , "1", "0"))
turt <- mutate(turt, turt.epb = if_else(Turtles==1 & EvolPhyBiog==1 , "1", "0"))
turt <- mutate(turt, turt.conserv = if_else(Turtles==1 & Conserv==1 , "1", "0"))

check <- sum(as.numeric(turt$turt.check))
newsp <- sum(as.numeric(turt$turt.newsp))
ecolnat <- sum(as.numeric(turt$turt.ecolnat))
epb <- sum(as.numeric(turt$turt.epb))
conserv <- sum(as.numeric(turt$turt.conserv))

Taxa <- c("Turtles", "Turtles", "Turtles", "Turtles", "Turtles")
Types <- c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

turt.sum <- data.frame(Taxa, Types, Sum)

#all taxa
multi <- mutate(ph.herps, multi.check = if_else(Multitaxa==1 & Checklist==1 , "1", "0"))
multi <- mutate(multi, multi.newsp = if_else(Multitaxa==1 & NewSp==1 , "1", "0"))
multi <- mutate(multi, multi.ecolnat = if_else(Multitaxa==1 & EcolNat==1 , "1", "0"))
multi <- mutate(multi, multi.epb = if_else(Multitaxa==1 & EvolPhyBiog==1 , "1", "0"))
multi <- mutate(multi, multi.conserv = if_else(Multitaxa==1 & Conserv==1 , "1", "0"))

check <- sum(as.numeric(multi$multi.check))
newsp <- sum(as.numeric(multi$multi.newsp))
ecolnat <- sum(as.numeric(multi$multi.ecolnat))
epb <- sum(as.numeric(multi$multi.epb))
conserv <- sum(as.numeric(multi$multi.conserv))

Taxa <- c("Multitaxa", "Multitaxa", "Multitaxa", "Multitaxa", "Multitaxa")
Types <- c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

multi.sum <- data.frame(Taxa, Types, Sum)

#pool data
herps.stype <- rbind(amphi.sum, liz.sum, snk.sum, turt.sum, croc.sum, multi.sum)

herps.stype

```

**3. Plot number of papers vs types of study**
```{r, num vs types}
#assignlabels
herps.stype <- mutate(herps.stype, lab = if_else(Types %in% "Diversity & Distribution", "Diversity & Distribution", 
  if_else(Types %in% "Taxonomy", "Taxonomy", if_else(Types %in% "Ecology", "Ecology", 
  if_else(Types %in% "Phylogenetics & Biogeography", "Phylogenetics & Biogeography", 
  if_else(Types %in% "Conservation", "Conservation", "Not data"))))))

herps.stype$lab <- factor(herps.stype$lab, levels = c("Diversity & Distribution", "Taxonomy", "Ecology", "Phylogenetics & Biogeography", "Conservation"))

herps.stype$Types <- factor(herps.stype$Types, levels = unique(herps.stype$Types))

#create stacked barplot
p.stype <- ggplot(herps.stype, aes(x = factor(Taxa, level = c("Amphibians", "Lizards", "Snakes", "Turtles", "Crocodiles", "Multitaxa")), 
  y = Sum, fill = lab, label = Sum)) + 
  geom_col(position = "stack") + 
  geom_text(size = 4,aes(label = scales::comma(Sum)), position = position_stack(vjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black"), legend.position = "", 
    text = element_text(size=20), plot.title = element_text(size = 25, face = "bold"), 
    axis.title=element_text(size=16, face="bold")) + 
  scale_fill_manual(values = c("#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43")) + 
  labs(fill="", x="", y="Number of papers") + 
  ggtitle("(A)")

#legend.direction = "horizontal", legend.key.size = unit(0.7, 'cm'), legend.title=element_blank(),

#create pie chart
check <- sum(as.numeric(ph.herps$Checklist))
newsp <- sum(as.numeric(ph.herps$NewSp))
ecolnat <- sum(as.numeric(ph.herps$EcolNat))
epb <- sum(as.numeric(ph.herps$EvolPhyBiog))
conserv <- sum(as.numeric(ph.herps$Conserv))

d.pie <- rbind(check, newsp, ecolnat, epb, conserv)
d.pie <- data.frame(d.pie, Types)


d.pie <- d.pie %>%mutate(Percentage=paste0(round(d.pie/sum(d.pie)*100,1)))

d.pie$Types <- factor(d.pie$Types, levels = unique(d.pie$Types))

p.pie <- ggplot(d.pie[1:5, ], aes("", Percentage, fill = Types)) +
    geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
    coord_polar("y") +
    geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 7) +
    labs(x = NULL, y = NULL, fill = NULL, title = "") +
    guides(fill = guide_legend(reverse = FALSE)) +
    scale_fill_manual(values = c("#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43")) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "black", size = 25, face ="bold"), 
          legend.position = c(0.0,0.97), ,text = element_text(size=20)) + 
    ggtitle("(B)")


grid.arrange(p.stype, p.pie, ncol = 2)

#com.legend <- get_legend(p.stype)
#grid.arrange(arrangeGrob(p.stype+ theme(legend.position="none"), p.pie+ theme(legend.position="none"), nrow = 1, ncol =2), arrangeGrob(nullGrob(), com.legend, nullGrob(), nrow = 1), ncol = 1, heights = c(3,0.4))
#export fig
tiff("fig_0.tif", res=300, width = 15, height = 7, unit="in")
grid.arrange(p.stype, p.pie, ncol = 2)
dev.off()

```

**4. Plot number of papers vs year**
```{r, num vs year}
#count papers per year
num.year <- ph.herps %>% count(Year)
#poisson regression
glm.res <- glm(n ~ Year, data = num.year, family = poisson(link = "log"))
#results summary
summ(glm.res)

#local polynomial regression
#k-fold cross validation
l.ctrl <- trainControl(method = "cv", number = 5)
l.grid <- expand.grid(span = seq(0.5, 1.0, len = 6), degree = 1)
#perform cross-validation using smoothing spans ranging from 0.5 to 1.0
loess.mod <- train(n ~ Year, data = num.year, method = "gamLoess", tuneGrid=l.grid, trControl = l.ctrl)
#print results of k-fold cross-validation
print(loess.mod) #select span with lowest RMSE
#perform polynomial regression
loess.res  <- loess(n ~ Year, data=num.year, span=.9)

#plot poisson regression
p.nyear.glm <- ggplot(num.year, aes(x=Year, y=n)) + 
    geom_point(size=3) + 
    geom_smooth(method="glm", formula = y ~ x, method.args = list(family = "poisson"), 
                col="firebrick", size=1, se = TRUE, level = 0.95, span = 1, linetype = "solid") +
    labs(y="Number of papers", x="Year", colour = "") + 
    scale_x_continuous("Year", labels = as.character(num.year$Year), breaks = num.year$Year) + 
    scale_y_continuous(breaks = seq(-10, 60, by=10), limits=c(-10,60)) +
    geom_text(aes(label=n),hjust=0.7, vjust=2, size = 4) + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
          axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
          legend.text=element_text(size=15),plot.title = element_text(size = 20, face = "bold"), 
          legend.title = element_blank(), legend.box="vertical", 
          axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
    ggtitle("(A)") + 
    geom_area(fill="#69b3a2", alpha=0.5) 

#plot local polynomial regression
p.nyear.loc <- ggplot(num.year, aes(x=Year, y=n)) + 
    geom_point(size=3) +  
    labs(y="Number of papers", x="Year", colour = "") + 
    scale_x_continuous("Year", labels = as.character(num.year$Year), breaks = num.year$Year) +
    scale_y_continuous(breaks = seq(-10, 60, by=10), limits=c(-10,60)) +
    geom_text(aes(label=n),hjust=0.8, vjust=2, size = 4) + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
          axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
          legend.text=element_text(size=15),plot.title = element_text(size = 20, face = "bold"), 
          legend.title = element_blank(), legend.box="vertical", 
          axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
    #geom_smooth(col="firebrick") + 
    ggtitle("(B)") + 
    geom_area(fill="#69b3a2", alpha=0.5) +
    stat_smooth(span=0.9, se=TRUE, col="firebrick")

grid.arrange(p.nyear.glm,p.nyear.loc, ncol = 2)

#export fig
tiff("fig_1.tif", res=300, width = 12, height = 5, unit="in")
grid.arrange(p.nyear.glm, p.nyear.loc, ncol = 2)
dev.off()
```

**5. Plot data demography**
```{r, demog}
#arrange data by nationality and gender
#foreign data
for.dem <- mutate(ph.herps, for.dem.m = if_else(Foreign==1 & Male==1 , "1", "0"))
for.dem <- mutate(for.dem, for.dem.f = if_else(Foreign==1 & Female==1 , "1", "0"))

for.m <- sum(as.numeric(for.dem$for.dem.m))
for.f <- sum(as.numeric(for.dem$for.dem.f))

Nat <- c("Foreign", "Foreign")
Gender <- c("Male", "Female")
Sum <- c(for.m, for.f)

for.sum <- data.frame(Nat, Gender, Sum)

#local data
loc.dem <- mutate(ph.herps, loc.dem.m = if_else(Filipino==1 & Male==1 , "1", "0"))
loc.dem <- mutate(loc.dem, loc.dem.f = if_else(Filipino==1 & Female==1 , "1", "0"))

loc.m <- sum(as.numeric(loc.dem$loc.dem.m))
loc.f <- sum(as.numeric(loc.dem$loc.dem.f))

Nat <- c("Filipino", "Filipino")
Gender <- c("Male", "Female")
Sum <- c(loc.m, loc.f)

loc.sum <- data.frame(Nat, Gender, Sum)

#merge data
for.loc <- rbind(for.sum, loc.sum)

#assignlabels
for.loc <- mutate(for.loc, lab = if_else(Gender %in% "Male", "Male", if_else(Gender %in% "Female", "Female", "Not data")))

for.loc$lab <- factor(for.loc$lab, levels = c("Male", "Female"))

for.loc$Gender <- factor(for.loc$Gender, levels = unique(for.loc$Gender))

#create stacked barplot
p.for.loc <- ggplot(for.loc, aes(x = factor(Nat, level = c("Foreign", "Filipino")), y = Sum, fill = lab, label = Sum)) + 
  geom_col(position = "stack") + 
  geom_text(size = 4,aes(label = scales::comma(Sum)), position = position_stack(vjust = 0.5)) + 
  theme(panel.background = element_blank(), legend.position ="none", panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
    axis.line = element_blank(), axis.text=element_text(size=14), plot.title = element_text(size = 20, face = "bold"), 
    axis.title=element_text(size=16, face="bold")) + 
  scale_fill_manual(values = c("#fee08b", "#3288bd")) + 
  labs(fill="", x="", y="Number of papers") + 
  ggtitle("(B)")

#aggregate data per year
m.all <- ph.herps %>% group_by(Year) %>% summarise(Freq = sum(Male))
m.all <- m.all %>% mutate(gender = "Male")
m.all <- m.all %>% rename("m.freq" = "Freq")
m.all <- m.all %>% rename("m.year" = "Year")
m.all <- m.all %>% rename("m.gender" = "gender")

f.all <- ph.herps %>% group_by(Year) %>% summarise(Freq = sum(Female))
f.all <- f.all %>% mutate(gender = "Female")
f.all <- f.all %>% rename("f.freq" = "Freq")
f.all <- f.all %>% rename("f.year" = "Year")
f.all <- f.all %>% rename("f.gender" = "gender")

m.f <- cbind(m.all, f.all)

p.m.f <- ggplot(m.f) + 
  geom_area(aes(x = m.year, y = m.freq),
            fill = "#fee08b") +
  geom_area(aes(x = f.year, y = f.freq),
            fill = "#3288bd") +
  labs(y="Number of papers", x="Year", colour = "") +
  scale_x_continuous("Year", labels = as.character(m.f$m.year), breaks = m.f$m.year) +
  theme(panel.background = element_blank(), legend.position = "none",panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text=element_text(size=14), 
    axis.title=element_text(size=16, face="bold"), plot.title = element_text(size = 20, face = "bold"), 
    axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
  ggtitle("(A)")

#get legend
legend <- get_legend(p.for.loc + theme(legend.position = "top", legend.direction = "vertical",legend.text=element_text(size=15)))

plot_grid(p.m.f, p.for.loc, ncol=2, align = "h")

tiff("fig_2.tif", res=300, width = 10, height = 5, unit="in")
plot_grid(p.m.f, p.for.loc, ncol=2, align = "h")
dev.off()

tiff("fig_2_legend.tif", res=300, width = 2, height = 2, unit="in")
plot(legend)
dev.off()
```

**6. Plot new species data**
```{r, new species}
#read data
newsp <- read.csv("newsp.csv")

#get commulative number of papers with species description
newsp <- newsp %>% mutate(Cum = cumsum(Number))

newsp <- newsp %>% mutate(ACum = cumsum(Amphibians))
newsp <- newsp %>% mutate(SCum = cumsum(Snakes))
newsp <- newsp %>% mutate(LCum = cumsum(Lizards))

newsp2 <- newsp[c(1, 7:9)]
colnames(newsp2)[2] <- "Amphibians"
colnames(newsp2)[3] <- "Snakes"
colnames(newsp2)[4] <- "Lizards"

newsp2 <- pivot_longer(newsp2, cols = Amphibians:Lizards, names_to = "Taxa", values_to = "Value")

p.new <- ggplot(newsp2, aes(Year, Value)) + 
  geom_bar(data = newsp, aes(x=Year, y=Number),stat="identity", fill="#fdae61",colour="white") + 
  geom_line(aes(linetype=Taxa), size = 1) + 
  geom_point(aes(shape = Taxa), size = 2) + 
  ylab("Number of new species") + 
  xlab("Year") + 
  scale_x_continuous("Year", labels = as.character(newsp$Year), breaks = newsp$Year) + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5), plot.title = element_text(size = 20, face = "bold"), 
    legend.position = c(0.20,0.7), panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    legend.key=element_blank(), legend.key.size = unit(1, 'cm'), axis.title=element_text(size=16, face="bold"), text = element_text(size = 15)) + 
  ggtitle("(A)")

p.new
```

**7. Plot data types**
```{r, data types}

mol.morph <- read.csv("mol_morph_newsp_13Feb2023.csv")

#aggregate data
d.mph <- mol.morph %>% group_by(Year) %>% summarise(Freq = sum(Morph))
d.mph <- d.mph %>% mutate(Types = "Morphology")
d.mol <- mol.morph %>% group_by(Year) %>% summarise(Freq = sum(Mol))
d.mol <- d.mol %>% mutate(Types = "Molecular")

d.all <- rbind(d.mph, d.mol)

#assignlabels
d.all <- mutate(d.all, lab = if_else(Types %in% "Morphology", "Morphology", if_else(Types %in% "Molecular", "Includes Molecular Data", "Not data")))

d.all$lab <- factor(d.all$lab, levels = c("Morphology", "Includes Molecular Data"))

#create stacked barplot
p.d.types <- ggplot(d.all, aes(x = Year, y = Freq, fill = lab, label = Freq)) + 
  geom_col(position = "stack") + 
  theme(panel.background = element_blank(), axis.line = element_blank(), legend.position = c(0.75, 0.9), 
    legend.direction = "vertical", legend.key.size = unit(0.4, 'cm'), panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
    legend.title=element_blank(), text = element_text(size=15), plot.title = element_text(size = 20, face = "bold"), 
    axis.title=element_text(size=16, face="bold"), axis.text.x = element_text(angle = 70, vjust = 0.5)) + 
  scale_fill_manual(values = c("#abdda4", "#5e4fa2")) + 
  labs(fill="", x="", y="Number of papers") + 
  scale_x_continuous("Year", labels = as.character(d.all$Year), breaks = d.all$Year) + 
  ggtitle("(B)")

p.d.types

tiff("fig_3.tif", res=300, width = 10, height = 5, unit="in")
ggarrange(p.new, p.d.types, ncol=2)
dev.off()
```

**8. Plot maps**
```{r, maps}
#new species
ph.adm <- read_sf("ph_shapefile/ph_adm0.shp")
ph.herps.newsp <- read_sf("ph_shapefile/ph_herps_newsp.shp")

ph.herps.newsp <- ph.herps.newsp %>% rename("TaxaCode" = "Taxa")
ph.herps.newsp <- ph.herps.newsp %>% rename("Taxa" = "TaxaName")

p.cols <- c("#f46d43","#18aa00", "#6ee5e7")

m.newsp <- ggplot() +
  geom_sf(data = ph.adm, fill = "#818181", color = NA) +
  geom_sf(data = ph.herps.newsp, aes(shape = Taxa, fill = Taxa), size = 1.5) +
  labs(title = "") + 
  scale_shape_manual(values = c(22,23,21)) + 
  scale_fill_manual(values = p.cols) + 
  theme(legend.position = "top", legend.direction = "vertical", legend.key = element_rect(fill = "transparent"), 
    axis.line = element_blank(), panel.background=element_blank(), panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), axis.title.y=element_blank(),axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.title = element_text(size = 15, face = "bold"), 
    legend.title = element_blank(), legend.key.size = unit(0, 'lines'), plot.margin = unit(c(0, 0, 0, 0), "cm"), 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.justification = "left") + 
  guides(fill=guide_legend(keywidth=0.1, keyheight=0.1, default.unit="inch"), shape = guide_legend(override.aes = list(size = 2.5))) + 
  ggtitle("(A)")

#new distribution records and natural history notes
ph.herps.ng <- read_sf("ph_shapefile/ph_herps_newgeo_nat.shp")

ph.herps.ng <- ph.herps.ng %>% rename("TaxaCode" = "Taxa")

ph.herps.ng<- mutate(ph.herps.ng, Paper = if_else(Type == 1, "New Distribution Records", if_else(Type == 4, "Natural History Notes", "No data")))

p.cols <- c("#52ed6c", "#a951c8")

m.ng <- ggplot() +
  geom_sf(data = ph.adm, fill = "#818181", color = NA) +
  geom_sf(data = ph.herps.ng, aes(shape = Paper, fill = Paper), size = 1.5) +
  labs(title = "") + 
  scale_shape_manual(values = c(22,23)) + 
  scale_fill_manual(values = p.cols) + 
  theme(legend.position = "top", legend.direction = "vertical", legend.key = element_rect(fill = "transparent"), 
    axis.line = element_blank(), panel.background=element_blank(), panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
    plot.title = element_text(size = 15, face = "bold"), legend.title = element_blank(), 
    legend.key.size = unit(0, 'lines'), plot.margin = unit(c(0, 0, 0, 0), "cm"), 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.justification = "left") + 
  guides(fill=guide_legend(keywidth=0.1, keyheight=0.1, default.unit="inch"), shape = guide_legend(override.aes = list(size = 2.5))) + 
  ggtitle("(B)")

#surveys
ph.herps.s <- read_sf("ph_shapefile/ph_herps_surveys.shp")

ph.herps.s <- ph.herps.s %>% rename("TaxaCode" = "Taxa")

ph.herps.s <- mutate(ph.herps.s, Paper = if_else(Type == 2, "2002-2022", if_else(Type == 5, "Pre-2002", if_else(Type == 6, "Curated Database", "No data"))))

ph.herps.s$Paper <- factor(ph.herps.s$Paper, levels = c("2002-2022", "Pre-2002", "Curated Database"))

p.cols <- c("#d53e4f", "#115deb", "#ffff2a")

m.s <- ggplot() +
  geom_sf(data = ph.adm, fill = "#818181", color = NA) +
  geom_sf(data = ph.herps.s, aes(shape = Paper, fill = Paper), size = 1.5) + 
  labs(title = "") + scale_shape_manual(values = c(21,22,24)) + scale_fill_manual(values=p.cols) + 
  theme(legend.position = "top", legend.direction = "vertical",legend.key = element_rect(fill = "transparent"), 
    axis.line = element_blank(), panel.background=element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
    plot.title = element_text(size = 15, face = "bold"), legend.title = element_blank(), legend.key.size = unit(0, 'lines'), 
    plot.margin = unit(c(0, 0, 0, 0), "cm"), panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.justification = "left")  + 
  ggtitle("(C)") + 
  guides(fill=guide_legend(keywidth=0.1, keyheight=0.1, default.unit="inch"), shape = guide_legend(override.aes = list(size = 2.5)))

plot_grid(m.newsp, m.ng, m.s, ncol=3, align = "hv")

#legend.key.size = unit(0, 'lines') to reduce space between vertical legend keys

tiff("fig_4.tif", res=300, width = 6.0, height = 4.5, unit="in")
plot_grid(m.newsp, m.ng, m.s, ncol=3, align = "hv")
dev.off()

#proportions of targeted surveys per PAIC
ph.herps2 <- read.csv("ph_herps_review_coor_29Dec2023.csv")

count.paic <- ph.herps2 %>%
    group_by(Type) %>%
    count(PAIC)

#remove empty rows
count.paic[!(is.na(count.paic$PAIC) | count.paic$PAIC==""), ]

#calulate percentage
prop.paic <- count.paic %>% group_by(Type) %>% mutate(per= prop.table(n) * 100)

prop.paic
```
