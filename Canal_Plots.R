## Plots Paride : Canal topic

df <- read_excel("Suez_entries_Ismael.xlsx")
head(df)
colnames(df)

#Rhine canal
df<- df[,c(20,30,53,55,56,59,69)]
str(df)

df<-df[-c(1:2),]
sum(df$cost_mil) #33.55 and 20.18

#Species donut
df$cost_mil <- round(df$cost_mil)
df1<- df %>% group_by(Species) %>% summarise(Costs= sum(cost_mil),
                                             Entries=n())
df1$fraction <- df1$Costs / sum(df1$Costs)

# Compute the cumulative percentages (top of each rectangle)
df1$ymax = cumsum(df1$fraction)

# Compute the bottom of each rectangle
df1$ymin = c(0, head(df1$ymax, n=-1))

df1
p1<- PieDonut(df1, aes(Species, Costs, count=Costs),
         ratioByGroup = F, r0 = 0.5, r1 = 0.7)   
         
p2<- PieDonut(df1, aes(Species, Costs, count=Entries),
              ratioByGroup = F, r0 = 0.5, r1 = 0.7)            
         

plot_grid(p1, p2, labels = c('A', 'B'))
##----------------------------
df1<- df %>% group_by(Impacted_sector) %>% summarise(Costs= sum(cost_mil),
                                             Entries=n())
df1$fraction <- df1$Costs / sum(df1$Costs)
df1
df1$Costs<- round(df1$Costs,2)

df$Impacted_sector[df$Impacted_sector=="Authorities-Stakeholders/Public and social welfare"]<- "Mixed"

p1<- PieDonut(df1, aes(Impacted_sector, Costs, count=Costs),
              ratioByGroup = F, r0 = 0.5, r1 = 0.7)   

p2<- PieDonut(df1, aes(Impacted_sector, Costs, count=Entries),
              ratioByGroup = F, r0 = 0.5, r1 = 0.7)            


library(patchwork)

p1 + p2

df1
ggplot() +
  ylab(paste0("")) +
  geom_point(data = df1,
             aes(x = Method_reliability, y = Costs, shape = Calibration),
             alpha = .6) +
  xlab("Year")+
  theme_bw() +
  scale_y_log10(breaks = 10^(-15:15),
                labels = scales::comma) +
  annotation_logticks(sides = "l") +
  geom_line(data =  
            aes_string(x = "Year",
                       y = "fit",
                       group = "Details"),
            size = 1, alpha = .6) +
  geom_ribbon(data = cost.over.time.obs$estimated.annual.costs[cost.over.time.obs$estimated.annual.costs$model == "Quantile regression", ],
              aes(x = Year,
                  ymin = lwr,
                  ymax = upr,
                  group = Details),
              alpha = .1) +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.position = c(.15, .85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
p4










## Stackover flow
colnames(df1)
library(plotly)
df1$Entries<- as.numeric(df1$Entries)

plot_ly(df1) %>%
  add_pie(labels = ~Species, values = ~Costs, 
          type = "pie", hole = 0.7, sort = F,
          marker = list(line = list(width = 2))) %>%
  add_pie(df1, labels = ~ Species, values = ~Entries, 
          domain = list(
            x = c(0.15, 0.85),
            y = c(0.15, 0.85))),
          sort = F) %>%
  layout(title = "Chart", 
         legend = list(title = list(text = "Species")))



df1 <- structure(list(Species = c("Cercopagis pengoi", "Dreissena polymorpha" ), Costs = c(0.27, 33.27), Entries = c(7L, 5L), fraction = c(0.00805008944543828,  0.991949910554562), ymax = c(0.00805008944543828, 1), ymin = c(0,  0.00805008944543828)), row.names = c(NA, -2L), class = c("tbl_df",  "tbl", "data.frame"))
str(df1)
df1$Species<- as.factor(df1$Species)
PieDonut(df1, aes(Species, Costs, count=Entries),




##Manually
str(df1)
df1$Species<- as.factor(df1$Species)
ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Species)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(1, 4)) +theme_void()+ 
  scale_fill_manual(values = c("Cercopagis pengoi" = "cyan1",
                              "Dreissena polymorpha" = "coral2"))

df1
df1$Entries<- as.factor(df1$Entries)

ggplot(df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Entries)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(1, 4)) +theme_void()+ 
  scale_fill_manual(values = c("7" = "cyan1",
                               "5" = "coral2"))



###World map
install.packages("OpenStreetMap")
install.packages("DT")
install.packages("RColorBrewer")
install.packages("mapproj")
install.packages("sf")
install.packages("RgoogleMaps")
install.packages("scales")
install.packages("rworldmap")
install.packages("maps")
install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("ggspatial")
install.packages("maptools")
install.packages("leaflet")
install.packages("tmap")
install.packages("here")
install.packages("rgdal")
install.packages("scales")
install.packages("flextable")
# install package from github
install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")

options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
op <- options(gvis.plot.tag='chart')  # set gViz options
# load package
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
op <- options(gvis.plot.tag='chart')  # set gViz options
# load package
library(OpenStreetMap)
library(DT)
library(RColorBrewer)
library(mapproj)
library(sf)
library(RgoogleMaps)
library(scales)
library(rworldmap)
library(maps)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(maptools)
library(leaflet)
library(sf)
library(tmap)
library(here)
library(rgdal)
library(scales)
library(flextable)
# activate klippy for copy-to-clipboard button
klippy::klippy()

ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-110.00, 60.00), ylim = c(-5.00, 70.00), expand = T) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

newmap <- getMap(resolution = "high")
# plot map
plot(newmap, xlim = c(-100, 59), ylim = c(10, 69), 
     asp = 1, fill = T, border = "darkgray", 
     col = "wheat2", bg = "azure2")


#Panama Canal

plot(newmap, xlim = c(-83, -76), ylim = c(5, 12), 
     asp = 1, fill = T, border = "darkgray", 
     col = "wheat2", bg = "azure2")



#Suez
plot(newmap, xlim = c(32, 35), ylim = c(20, 39), 
     asp = 1, fill = T, border = "darkgray", 
     col = "wheat2", bg = "azure2")


#RMD
plot(newmap, xlim = c(25, 35), ylim = c(37,64), 
     asp = 1, fill = T, border = "darkgray", 
     col = "wheat2", bg = "azure2")




