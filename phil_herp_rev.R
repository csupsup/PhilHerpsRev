---
title: "PH Herp Status"
author: "Supsup et al."
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
Types <- c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation")
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
Types <- c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation")
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
Types <- c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation")
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
Types <- c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation")
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
Types <- c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation")
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
Types <- c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation")
Sum <- c(check, newsp, ecolnat, epb, conserv)

multi.sum <- data.frame(Taxa, Types, Sum)

#pool data
herps.stype <- rbind(amphi.sum, liz.sum, snk.sum, turt.sum, croc.sum, multi.sum)

herps.stype
```

**3. Plot number of papers vs types of study**
```{r, num vs types}
#assignlabels
herps.stype <- mutate(herps.stype, lab = if_else(Types %in% "Checklist & Biodiversity", "Checklist & Biodiversity", if_else(Types %in% "New Species & Taxonomy", "New Species & Taxonomy", if_else(Types %in% "Ecology & Natural History", "Ecology & Natural History", if_else(Types %in% "Evolution & Phylogenetics", "Evolution & Phylogenetics", if_else(Types %in% "Conservation", "Conservation", "Not data"))))))

herps.stype$lab <- factor(herps.stype$lab, levels = c("Checklist & Biodiversity", "New Species & Taxonomy", "Ecology & Natural History", "Evolution & Phylogenetics", "Conservation"))

herps.stype$Types <- factor(herps.stype$Types, levels = unique(herps.stype$Types))

#create stacked barplot
p.stype <- ggplot(herps.stype, aes(x = factor(Taxa, level = c("Amphibians", "Lizards", "Snakes", "Turtles", "Crocodiles", "Multitaxa")), y = Sum, fill = lab, label = Sum)) + geom_col(position = "stack") + geom_text(size = 4,aes(label = scales::comma(Sum)), position = position_stack(vjust = 0.5)) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = c(0.8, 0.9), legend.direction = "vertical", legend.key.size = unit(0.7, 'cm'), legend.title=element_blank(), text = element_text(size=20), plot.title = element_text(size = 20, face = "bold"), axis.title=element_text(size=16, face="bold")) + scale_fill_manual(values = c("#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43")) + labs(fill="", x="", y="Number of papers") + ggtitle("")

p.stype

#export fig
tiff("fig_0.tif", res=300, width = 9, height = 7, unit="in")
p.stype
dev.off()

```

**4. Plot number of papers vs year**
```{r, num vs year}
#count papers per year
num.year <- ph.herps %>% count(Year)

#plot local polynomial regression fitting method

p.nyear.lm <- ggplot(num.year, aes(x=Year, y=n)) + 
    geom_point(size=3) + 
    geom_smooth(method="lm", col="firebrick", size=1, se = TRUE, level = 0.95, span = 1, linetype = "solid") + labs(y="Number of papers", x="Year", colour = "") + scale_x_continuous("Year", labels = as.character(num.year$Year), breaks = num.year$Year) + geom_text(aes(label=n),hjust=0.7, vjust=2, size = 4) + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), legend.text=element_text(size=15),plot.title = element_text(size = 20, face = "bold"), legend.title = element_blank(), legend.box="vertical", axis.text.x = element_text(angle = 70, vjust = 0.5)) + ggtitle("A") + geom_area(fill="#69b3a2", alpha=0.5)

p.nyear.loc <- ggplot(num.year, aes(x=Year, y=n)) + 
    geom_point(size=3) +  
    labs(y="Number of papers", x="Year", colour = "") + scale_x_continuous("Year", labels = as.character(num.year$Year), breaks = num.year$Year) + geom_text(aes(label=n),hjust=0.8, vjust=2, size = 4) + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), legend.text=element_text(size=15), plot.title = element_text(size = 20, face = "bold"), legend.title = element_blank(), legend.box="vertical", axis.text.x = element_text(angle = 70, vjust = 0.5)) + geom_smooth(col="firebrick") + ggtitle("B") + geom_area(fill="#69b3a2", alpha=0.5)

grid.arrange(p.nyear.lm,p.nyear.loc, ncol = 2)

#export fig
tiff("fig_1.tif", res=300, width = 12, height = 5, unit="in")
grid.arrange(p.nyear.lm,p.nyear.loc, ncol = 2)
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
loc.dem <- mutate(ph.herps, loc.dem.m = if_else(Local==1 & Male==1 , "1", "0"))
loc.dem <- mutate(loc.dem, loc.dem.f = if_else(Local==1 & Female==1 , "1", "0"))

loc.m <- sum(as.numeric(loc.dem$loc.dem.m))
loc.f <- sum(as.numeric(loc.dem$loc.dem.f))

Nat <- c("Local", "Local")
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
p.for.loc <- ggplot(for.loc, aes(x = factor(Nat, level = c("Foreign", "Local")), y = Sum, fill = lab, label = Sum)) + geom_col(position = "stack") + geom_text(size = 4,aes(label = scales::comma(Sum)), position = position_stack(vjust = 0.5)) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.direction = "horizontal", legend.key.size = unit(0.7, 'cm'), legend.title=element_blank(), text = element_text(size=20), plot.title = element_text(size = 20, face = "bold"), axis.title=element_text(size=16, face="bold")) + scale_fill_manual(values = c("#fee08b", "#3288bd")) + labs(fill="", x="", y="Number of papers") + ggtitle("")

p.for.loc

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
            fill = "#fee08b", alpha = 0.85) +
  geom_area(aes(x = f.year, y = f.freq),
            fill = "#3288bd", alpha = 0.85) +
  labs(y="Number of papers", x="Year", colour = "") +
  scale_x_continuous("Year", labels = as.character(m.f$m.year), breaks = m.f$m.year) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), legend.text=element_text(size=15), plot.title = element_text(size = 20, face = "bold"), legend.title = element_blank(), legend.box="vertical", axis.text.x = element_text(angle = 70, vjust = 0.5))

grid.arrange(p.m.f, p.for.loc, ncol=2)

tiff("fig_2.tif", res=300, width = 10, height = 5, unit="in")
grid.arrange(p.m.f, p.for.loc, ncol=2)
dev.off()

```

**6. Plot data types**
```{r, data types}
#aggregate data
d.mph <- ph.herps %>% group_by(Year) %>% summarise(Freq = sum(Morph))
d.mph <- d.mph %>% mutate(Types = "Morphology")
d.mol <- ph.herps %>% group_by(Year) %>% summarise(Freq = sum(Molec))
d.mol <- d.mol %>% mutate(Types = "Molecular")

d.all <- rbind(d.mph, d.mol)

#assignlabels
d.all <- mutate(d.all, lab = if_else(Types %in% "Morphology", "Morphology", if_else(Types %in% "Molecular", "Inlcudes Molecular Data", "Not data")))

d.all$lab <- factor(d.all$lab, levels = c("Morphology", "Inlcudes Molecular Data"))

d.all$Gender <- factor(d.all$Types, levels = unique(d.all$Types))

#create stacked barplot
p.d.types <- ggplot(d.all, aes(x = Year, y = Freq, fill = lab, label = Freq)) + geom_col(position = "stack") + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "bottom", legend.direction = "horizontal", legend.key.size = unit(0.7, 'cm'), legend.title=element_blank(), text = element_text(size=20), plot.title = element_text(size = 20, face = "bold"), axis.title=element_text(size=16, face="bold"), axis.text.x = element_text(angle = 70, vjust = 0.5)) + scale_fill_manual(values = c("#abdda4", "#5e4fa2")) + labs(fill="", x="", y="Number of papers") + ggtitle("") + scale_x_continuous("Year", labels = as.character(d.all$Year), breaks = d.all$Year)

p.d.types

tiff("fig_3.tif", res=300, width = 8, height = 5, unit="in")
p.d.types
dev.off()
```
