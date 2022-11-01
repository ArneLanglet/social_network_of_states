# clear workspace
rm (list = ls())

#install.packages("readr")
library(readr)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(igraph) 
library(readxl)
library(rtf)
library(sandwich)
library(car)
#install.packages("car")
library(car)
library(ggrepel)
library(ggplot2)
library(GGally)
library(ggplot2)
library(igraph)
library(network)
library(igraph)
library(MASS)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(stargazer)

setwd("C:/Users/arnel/OneDrive/Desktop/Kursmaterialien/categorical variables/final paper")


## adjacency matrix state network
networld <- read.csv("networld.csv", row.names = 2)

matrixwelt <- as.matrix(networld[2:194], weighted = T, mode = "undirected")

networld <- graph_from_adjacency_matrix(matrixwelt, diag = F, weighted = T, mode = "undirected")

networld <- simplify(networld, remove.loops = TRUE)

networld <- igraph::delete.vertices(networld, degree(networld)==0)


set.seed(123)
plot(networld, main="Network of States",
     vertex.label.cex=1,
    # vertex.label.color = V(networld3)$colr,
     vertex.color = "white",
     vertex.shape = "circle",
     weight.edge.length = E(networld)$weight*2,
    edge.width = 0.5,
    vertex.size = 2,
     layout=layout.fruchterman.reingold)


mygraph <- as.data.frame(layout.fruchterman.reingold(networld))
mygraph$name <- V(networld)$name

g <- get.data.frame(networld) 

g$from.x <- mygraph$V1[match(g$from, mygraph$name)]  #  match the from locations from the node data.frame we previously connected
g$from.y <- mygraph$V2[match(g$from, mygraph$name)]
g$to.x <- mygraph$V1[match(g$to, mygraph$name)]  #  match the to locations from the node data.frame we previously connected
g$to.y <- mygraph$V2[match(g$to, mygraph$name)]



ggplot() +
  geom_segment(data=g,aes(x=from.x,xend = to.x, y=from.y,yend = to.y, size=weight/100),colour = "lightgrey") +
  #geom_point(data=mygraph,aes(x=V1,y=V2),size=5,colour="black") +  # adds a black border around the nodes
  geom_point(data=mygraph,aes(x=V1,y=V2),size=2,colour="black") +
  #geom_text_repel(label=mygraph$name, cex = 2.5, colour = "red",
  #                max.overlaps = Inf) +
  geom_text_repel(data=mygraph,aes(x=V1,y=V2,label=name, max.overlaps = 20, colour = "red")) + # add the node labels
  scale_x_continuous(expand=c(0,1))+  # expand the x limits 
  scale_y_continuous(expand=c(0,1))+ # expand the y limits
  theme_bw() +
  theme(
    axis.text.x = element_blank(),  # remove x-axis text
    axis.text.y = element_blank(), # remove y-axis text
    axis.ticks = element_blank(),  # remove axis ticks
    axis.title.x = element_blank(), # remove x-axis labels
    axis.title.y = element_blank(), # remove y-axis labels
    panel.background = element_blank(), 
    panel.border =element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank()) +
    theme(legend.position = "none")


## read country eigenvector scores, ratified treaties, gdp, 
cntry_full <- read.csv("cntry_full.csv")




ggplot(cntry_full, aes(x=eigenvector, y=overall_membership)) + geom_point() +
  geom_text_repel(label=cntry_full$name, cex = 2.5, colour = "red",
                  max.overlaps = Inf) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  xlab("Centrality in international social network") + 
  ylab("Number of multilateral env. agreements ratified") +
  theme_classic()


ggplot(cntry_full, aes(x=GDPpc, y=major_agreement_selection)) + geom_point() +
  geom_text_repel(label=cntry_full$name, cex = 2.5, colour = "red",
                  max.overlaps = Inf) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  xlab("GDP pc") + 
  ylab("Number of multilateral env. agreements ratified") +
  theme_classic()



## poisson regression
# model 1 all MEAs
glm1 <- glm(overall_membership ~  eigenvector + GDPpc + demos_stand,family=poisson,data=cntry_full)


# baseline model
glm1b <- glm(overall_membership ~  GDPpc + demos_stand,family=poisson,data=cntry_full)


# model 2 recent ratifications
glm2 <- glm(ratifications_201517 ~ eigenvector + GDPpc + demos_stand, family=poisson,data=cntry_full)

# model 3 major MEAs
glm3 <- glm(major_agreement_selection ~ eigenvector + GDPpc + demos_stand, family=poisson,data=cntry_full)

glm1

# include robust standard errors
summary(glm1, type = "hc3")

summary(glm2, type = "hc3")

summary(glm3, type = "hc3")
# are the same as normal se



#### stargazer to copy/paste into latex
stargazer(glm1, glm2, glm3)


tab_model(glm1, glm2, glm3, auto.label = FALSE)

tab_model(glm1, glm1b, auto.label = FALSE)

stargazer(glm1, glm1b)

