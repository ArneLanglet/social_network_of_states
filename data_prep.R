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

Country <-  c('Afghanistan', 'Albania', 'Algeria', 'Andorra', 'Antigua and Barbuda', 'Argentina', 'Armenia', 'Australia', 'Azerbaijan', 'Austria', 'Aruba',
            'Angola','Bahamas', 'Bahrain', 'Bangladesh', 'Barbados', 'Belarus', 'Belgium', 'Belize',
            'Benin', 'Bhutan', 'Bolivia', 'Bosnia and Herzegovina', 'Brunei Darussalam',
            'Botswana', 'Brazil', 'Bulgaria', 'Burkina Faso', 'Burundi', 'Czech Republic', 'Cyprus',
            'Cuba', 'Croatia', 'Cote dIvoire', 'Costa Rica', 'Congo, Democratic Republic of', 'Congo, Republic of the',
            'Comoros', 'Colombia', 'China', 'Chile', 'Chad', 'Central African Republic', 'Cape Verde',
            'Canada', 'Cameroon', 'Cambodia', 'Ethiopia', 'Estonia', 'Eritrea', 'Equatorial Guinea', 'El Salvador',
            'Egypt', 'Ecuador', 'Dominica', 'Dominican Republic', 'Djibouti', 'Denmark', 'Fiji', 'Faeroe Islands',
            'Finland', 'France', 'Gabon', 'Gambia', 'Georgia', 'Germany', 'Ghana',
            'Greece', 'Greenland', 'Grenada', 'Guatemala', 'Guinea',
            'Guinea-Bissau', 'Guyana', 'Haiti', 'Honduras', 'Hong Kong', 'Hungary',
            'Iceland', 'India', 'Indonesia', 'Iran', 'Iraq', 'Ireland', 'Israel', 'Italy', 'Jamaica',
            'Japan', 'Jordan', 'Kazakhstan', 'Kenya', 'Kiribati', 'Korea, Democratic Peoples Republic', 'Kyrgyzstan',
            'Korea, Republic of', 'Kosovo', 'Kuwait', 'Laos', 'Latvia', 'Lebanon', 'Lesotho', 'Liberia', 'Libya',
            'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia', 'Madagascar', 'Malawi', 'Malaysia',
            'Maldives', 'Mali', 'Malta', 'Marshall Islands', 'Mauritania', 'Mauritius', 'Mexico', 'Micronesia',
            'Moldova', 'Monaco', 'Mongolia', 'Montenegro', 'Morocco', 'Mozambique', 'Myanmar', 'Namibia', 'Nauru',
            'Nepal', 'Netherlands', 'New Zealand', 'Nicaragua', 'Niger', 'Nigeria', 'Norway', 'Oman', 'Pakistan',
            'Panama', 'Papua New Guinea', 'Palau', 'Paraguay', 'Peru', 'Philippines', 'Poland',
            'Portugal', 'Qatar', 'Romania', 'Russian Federation', 'Rwanda', 'Saint Helena', 'Saint Kitts and Nevis', 
            'Saint Lucia', 'Samoa', 'San Marino', 'Sao Tome and Principe', 'Saudi Arabia', 'Senegal', 'Serbia', 'Seychelles',
            'Sierra Leone', 'Singapore', 'Slovakia', 'Slovenia', 'Solomon Islands', 'Somalia', 'South Africa', 'Spain',
            'Sri Lanka', 'Sudan', 'South Sudan', 'Suriname', 'Swaziland', 'Saint Vincent and the Grenadines', 'Switzerland', 'Syria', 'Taiwan', 'Sweden', 'Tajikistan', 'Tanzania',
            'Thailand', 'Timor-Leste', 'Togo', 'Tonga', 'Trinidad and Tobago', 'Tunisia', 'Turkey', 'Turkmenistan', 'Tuvalu', 'Uganda', 'Ukraine', 
            'United Arab Emirates', 'United Kingdom', 'United States', 'Uruguay',
            'Uzbekistan', 'Vanuatu', 'Venezuela', 'Viet Nam', 'Yemen', 'Zambia', 'Zimbabwe')
#  Kosovo, Aruba, Hong Kong, Taiwan, Saint Helena were taken out because do not seem to exist in network

### Loop to read.CSV s into a list
setwd("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores")
cnt_list <- list()
for (i in Country) { cnt_list[[i]] <- read_csv(paste0("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores/", i, "_country_members.csv"))}

#### all csv files into one df
#f <- list.files(   "C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores",   pattern = "*.csv",b  full.names = TRUE)

#d <- purrr::map_df(f, readr::read_csv, .id = "id")


cnt_list <- lapply(cnt_list, function(x) {
  names(x)[ grep("Ratifications*", names(x))] <- "Ratifications"
  names(x)[ grep("Agreement Name", names(x))] <- "Name"
  names(x)[ grep("1st Withdrawal*", names(x))] <- "Withdrawal"
  x} )




# Keep MEAs
# Generelle Liste mit allen MEAs, die keine Amendments sind, ratifiziert und nicht withdrawn wurden
cnt_list1 <- lapply(cnt_list, subset, Inclusion == "MEA - Multilateral Environmental Agreements")
cnt_list1 <- lapply(cnt_list1, subset, !is.na (Ratifications))
cnt_list1 <- lapply(cnt_list1, subset, is.na (Withdrawal))

# 2. Liste mit Ratifizierungen in 2015-2017
cnt_list2 <- lapply(cnt_list1, subset, Ratifications > 2015-01-01)
cnt_list2 <- lapply(cnt_list2, subset, is.na (Withdrawal))

# 3. List mit Withdrawals in 2015-2017
# cnt_list3 <- lapply(cnt_list1, subset, Withdrawal > 2015-01-01)

# 4. Liste mit 21 selected Treaties by Timmons et al. 2004
ids <- c(2621, 2650, 2675, 2674, 2785, 2793, 2812, 2813, 2814, 2832, 2841, 2896, 2904, 2947,

                  2982, 3083, 3086, 3117, 3126, 3128, 3188)
cnt_list4 <- lapply(cnt_list, subset, mitch_id %in% ids)
cnt_list4 <- lapply(cnt_list4, subset, !is.na (Ratifications))
cnt_list4 <- lapply(cnt_list4, subset, is.na (Withdrawal))


#### Rows/Anzahl der Treaties z?hlen
overall_membership <- sapply(cnt_list1, NROW)
ratifications_201517 <- sapply(cnt_list2, NROW)
#Withdrs_after_2015 <- sapply(cnt_list3, NROW)
major_agreement_selection <- sapply(cnt_list4, NROW)

name <- Country


countryscore <- data.frame(name, overall_membership, ratifications_201517, major_agreement_selection, stringsAsFactors=FALSE)


write.csv(countryscore, "countryscore.csv")


## Einige Staaten m?ssen umbenannt werden, damit sp?ter das Anmergen an die eigenvector Centrality klappt -> Z.133
countryscore$name[countryscore$name=="Cape Verde"] <- 'Cabo Verde'
countryscore$name[countryscore$name=="Syria"] <- 'Syrian Arab Republic'
countryscore$name[countryscore$name=="Korea, Republic of"] <- 'Korea Rep'
countryscore$name[countryscore$name=="Cote dIvoire"] <- "C?te d'Ivoire"
countryscore$name[countryscore$name=="Congo, Republic of the"] <- "Congo"
countryscore$name[countryscore$name=="Sao Tome and Principe"] <- 'Sao Tom?-Principe'
countryscore$name[countryscore$name=="Moldova"] <- "Republic of Moldova"
countryscore$name[countryscore$name=="Viet Nam"] <- "Vietnam"
countryscore$name[countryscore$name=="United States"] <- "USA"
countryscore$name[countryscore$name=="United Kingdom"] <- "UK"
countryscore$name[countryscore$name=="Russian Federation"] <- "Russia"
countryscore$name[countryscore$name=="Korea, Republic of"] <- "Korea Rep"
countryscore$name[countryscore$name=="Micronesia"] <- "Federated States of Micronesia"
countryscore$name[countryscore$name=="Korea, Democratic Peoples Republic"] <- "Democratic People's Republic of Korea"
countryscore$name[countryscore$name=="Congo, Democratic Republic of"] <- "Democratic Republic of the Congo"




#sort by country name
countryscore <- countryscore[order(countryscore$name),]

##### Netzwerkgraph erstellen
##### networld ist für eigenvector
##### networld3 ist für visualisierung - deutlich trennen und nur eins anspielen! 
# set working directory
setwd("C:/Users/Arne Langlet/Desktop/MAIB/3. Semester WI 2017-18/HA/Sample")

# 1. Netzwerk einlesen aus Excel Matrix

df <- read_excel("Staaten3.xlsx", sheet = 1)

write.csv(df, "networld.csv")


matrixwelt <- as.matrix(df[2:194], weighted = T, mode = "undirected")
#matrixwelt2 <- as.matrix(df[2:194], weighted = NULL, mode = "undirected")

row.names(matrixwelt) <- df$Name

#?graph_from_adjacency_matrix
networld <- graph_from_adjacency_matrix(matrixwelt, diag = F, weighted = T, mode = "undirected")
networld

#networld3 <- graph_from_adjacency_matrix(matrixwelt2, diag = F, weighted = NULL, mode = "undirected")
#networld3


# Namen hinzuf?gen
name <- (c(df$Name))


# summarize
sum(matrixwelt)
# check weight
E(networld)$weight
#is_weighted(networld3)
is_weighted(networld)
# remove loops 
networld <- simplify(networld, remove.loops = TRUE)
#networld3 <- simplify(networld3)

# delete isolates
#networld3 <- delete.vertices(simplify(networld3), degree(networld3)==0)
#networld <- delete.vertices(simplify(networld), degree(networld)==0)


strength(networld)

### eigenvector Centrality messen
eigenvector   <- evcent(networld)$vector
eigenvector


#### mean eigenvector messen + differenz zum mean
m <- mean(eigenvector)

eigenvector_mean = eigenvector - m
eigenvector_mean <- abs(eigenvector_mean)

## Eigenvector Variable erstellen
countryvalue <- data.frame(name, eigenvector)
#countryvaluemean <- data.frame(name, eigenvector_mean)

### centrality dataframe sortieren
#countryvalue <- countryvalue[order(countryvalue$name)]



#### Score und Zentralit?t mergen
cntry_full <- merge(countryvalue, countryscore, by="name", all.x = TRUE)
#cntry_full <- merge(cntry_full, countryvaluemean, by="name", all.x = TRUE)

##### dummy f?r core-periphery errstellen 

cntry_full$core <- ifelse(cntry_full$eigenvector > 0.4,1,0)

############################# GDP per capita hinzuf?gen

setwd("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores")
gdp <- read_xlsx("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores/gdppc.xlsx", col_names = TRUE)
gdp <- na.omit(gdp)

cntrygdp <- as.data.frame(gdp)

cntrygdp
names(cntrygdp)[names(cntrygdp)=="2017"] <- "GDP"
cntrygdp
names(cntrygdp)[names(cntrygdp)=="Country Name"] <- "name"
cntrygdp

#### Einige Staaten m?ssen WIEDER umbenannt werden, damit das Anmergen an das bestehende Cntry_full
cntrygdp$name[cntrygdp$name=="Cape Verde"] <- 'Cabo Verde'
cntrygdp$name[cntrygdp$name=="Syria"] <- 'Syrian Arab Republic'
cntrygdp$name[cntrygdp$name=="Korea, Republic of"] <- 'Korea Rep'
cntrygdp$name[cntrygdp$name=="Cote d'Ivoire"] <- "C?te d'Ivoire"
cntrygdp$name[cntrygdp$name=="Congo, Republic of the"] <- "Congo"
cntrygdp$name[cntrygdp$name=="Sao Tome and Principe"] <- 'Sao Tom?-Principe'
cntrygdp$name[cntrygdp$name=="Moldova"] <- "Republic of Moldova"
cntrygdp$name[cntrygdp$name=="Viet Nam"] <- "Vietnam"
cntrygdp$name[cntrygdp$name=="United States"] <- "USA"
cntrygdp$name[cntrygdp$name=="United Kingdom"] <- "UK"
cntrygdp$name[cntrygdp$name=="Russian Federation"] <- "Russia"
cntrygdp$name[cntrygdp$name=="Korea, Republic of"] <- "Korea Rep"
cntrygdp$name[cntrygdp$name=="Micronesia"] <- "Federated States of Micronesia"
cntrygdp$name[cntrygdp$name=="Korea, Democratic Peoples Republic"] <- "Democratic People's Republic of Korea"
cntrygdp$name[cntrygdp$name=="Congo, Democratic Republic of"] <- "Democratic Republic of the Congo"



#eigenvector_mean <- abs(eigenvector_mean)

cntry_full <- merge(cntry_full,  cntrygdp, by="name", all.x = TRUE)

cntry_full <- mutate(cntry_full, GDPpc = GDP / 100000)


# quadratic eigenvector for cuznet curve test... 
#cntry_full <- mutate(cntry_full, eigenvector2 = eigenvector^2)

cntry_full

######### Add Democracy Score
#install.packages("dplyr")
library(readxl)
library(dplyr)
setwd("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores")
demo <- read_xlsx("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores/democracyindex.xlsx", sheet = 2,col_names = TRUE)
demo <- as.data.frame(demo)
demo <- na.omit(demo)
demo <- select(demo, name, demos)

##### change some names for merge
demo$name[demo$name=="Brunei"] <- 'Brunei Darussalam'
demo$name[demo$name=="Cape Verde"] <- 'Cabo Verde'
demo$name[demo$name=="Congo (Brazzaville)"] <- 'Congo'
demo$name[demo$name=="Congo (Kinshasa)"] <- 'Democratic Republic of the Congo'
demo$name[demo$name=="Cote d'Ivoire"] <- "C?te d'Ivoire"
demo$name[demo$name=="North Korea"] <- "Democratic People's Republic of Korea"
demo$name[demo$name=="Micronesia"] <- "Federated States of Micronesia"
demo$name[demo$name=="The Gambia"] <- "Gambia"
demo$name[demo$name=="South Korea"] <- "Korea Rep"
demo$name[demo$name=="Moldova"] <- "Republic of Moldova"
demo$name[demo$name=="St. Kitts and Nevis"] <- "Saint Kitts and Nevis"
demo$name[demo$name=="St. Lucia"] <- "Saint Lucia"
demo$name[demo$name=="St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
demo$name[demo$name=="Sao Tome and Principe"] <- "Sao Tom?-Principe"
demo$name[demo$name=="Syria"] <- 'Syrian Arab Republic'
demo$name[demo$name=="United States"] <- "USA"
demo$name[demo$name=="United Kingdom"] <- "UK"



cntry_full <- merge(cntry_full, demo, by="name", all.x = TRUE)


cntry_full$demos_stand <- 1 + cntry_full$demos

hist(cntry_full$demos_stand)

hist(cntry_full$GDPpc)


######### 


## Country_full Tabelle als rtf-file speichern

rtffile <- RTF("cntry_full.rtf")  # this can be an .rtf or a .doc
addTable(rtffile, cbind((cntry_full)))
done(rtffile)

Tab1

?sort
sort(Score)

#####cntry full als dataframe csv speichern
write.csv(cntry_full, "cntry_full.csv")


############################### attribute treaty score to nodes
env <- cut(overall_membership, breaks = c(0, 50, 80, 120, Inf), labels = FALSE)
is.na(env) <- 1

st <- as.data.frame(name, env)

V(networld2)$env <- env

env[is.na(env)] <- 1

overall_membership

## set color according to centrality
#networld2[is.na(Score)] <- 0

palette <- c("black", "blue","red","orange","yellow")
par(mfrow=c(1,2))
colrs <- rainbow
networld2

?plot

V(networld3)$shape <- env
V(networld3)$shape

shapes <- c("circle", "square", "csquare", "rectangle")

shapes[c(env)]

colr <- palette[c(env)]


V(networld3)$colr <- (networld3)colr

shapes[V(networld2)$env]

install.packages("GGally")
library(GGally)
install.packages("ggplot2")
library(ggplot2)
library(igraph)
install.packages("network")
library(network)
library(igraph)
networld4 = network(matrixwelt2, directed = FALSE, weighted = NULL)

ggnet2(networld4)

set.seed(123)

pdf("networld2.pdf")
set.seed(123)
plot(networld3, main="Network of States",
     vertex.label.cex=0.5 ,
     vertex.label.color = V(networld3)$colr,
     vertex.color = "white",
     vertex.shape = "square",
     weight.edge.length = 10,
     layout=layout.fruchterman.reingold)
dev.off()



assign(paste0("d",i,sep="_"),country_members)

##### R Tipp Test:
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  ldf[[k]] <- read.csv(listcsv[k])
}
str(ldf[[2]]) 

###

#### plot Darstellung
plot(countryscore, countryvalue, main="Environmental Treaties / Centrality", type="p")
?plot

#install.packages("car")
library(car)

png("Membership-Eigenvector.png")
scatterplot(overall_membership ~ eigenvector, data=cntry_full)
librdev.off()


#scatterplot(overall_membership ~ eigenvector, data=cntry_full)
#scatterplot(overall_membership ~ demos, data=cntry_full)


scatterplot(ratifications_201517 ~ eigenvector, data=cntry_full)
scatterplot(major_agreement_selection ~ eigenvector, data=cntry_full)

png("select-eigenvector.png")
scatterplot(major_agreement_selection ~ eigenvector, data=cntry_full)
  text(major_agreement_selection, eigenvector, 
       labels=cntry_full$name, cex= 0.7)


lidev.off()

scatterplot(overall_membership ~ eigenvector, data=cntry_full)

library(ggrepel)

ggplot(cntry_full, aes(x=eigenvector, y=overall_membership)) + geom_point() +
  geom_text_repel(label=cntry_full$name, cex = 2.5, colour = "red",
                  max.overlaps = Inf) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  xlab("Centrality in international social network") + 
  ylab("Number of multilateral env. agreements ratified") +
  theme_classic()



 

#### logit test regression

library(MASS)
#nb1 <- glm.nb(overall_membership ~  eigenvector + GDPpc + demos, data=cntry_full )
#nb2 <- glm.nb(ratifications_201517 ~ eigenvector + GDPpc + demos, data=cntry_full)
#nb3 <- glm.nb(major_agreement_selection ~ eigenvector + GDPpc + demos , data=cntry_full)
#?glm.nb


#squared <- glm.nb(overall_membership ~  eigenvector + GDPpc + eigenvector2, data=cntry_full)

?glm
glm1 <- glm(overall_membership ~  eigenvector + GDPpc + demos,family=poisson,data=cntry_full)
glm1 <- glm(overall_membership ~  eigenvector + GDPpc + demos,family=quasipoisson,data=cntry_full)

glm1
glm2 <- glm(ratifications_201517 ~ eigenvector + GDPpc + demos, family=poisson,data=cntry_full)
glm3 <- glm(major_agreement_selection ~ eigenvector + GDPpc + demos, family=poisson,data=cntry_full)
lm5 <- lm(overall_membership ~ core, family=logit, data=cntry_full)


glm1


tab_model(glm1, auto.label = FALSE)

llm5
#glm4 <- glm(overall_membership ~ eigenvector_mean, family=poisson,data=cntry_full)

summary(glm1)

### permutation test using permtest
install.packages("remotes")
library(remotes)
install_version("glmperm")
library(glmperm)
?glmperm
p1 <- prr.test(overall_membership ~ eigenvector, data=cntry_full, var ="eigenvector", family = quasipoisson)
summary(p1)
p2 <- prr.test(overall_membership ~ GDPpc, data=cntry_full, var ="GDPpc", family = poisson)

summary(p2)


### Permutation test 
#install.packages("coin")
library(coin)
symmetry_test(overall_membership ~ eigenvector | name, data = cntry_full)

?independence_test
  
i1 <- independence_test(eigenvector ~ overall_membership, data=cntry_full, distribution ="asymptotic")
i2 <- independence_test(ratifications_201517 ~ eigenvector, data=cntry_full, distribution ="asymptotic")
i3 <- independence_test(major_agreement_selection ~ eigenvector, data=cntry_full, distribution ="asymptotic")
i1b <- independence_test(overall_membership ~ GDPpc, data=cntry_full, distribution ="asymptotic")
i2b <- independence_test(ratifications_201517 ~ GDPpc, data=cntry_full, distribution ="asymptotic")
i3b <- independence_test(major_agreement_selection ~ GDPpc, data=cntry_full, distribution ="asymptotic")
i1c <- independence_test(overall_membership ~ demos, data=cntry_full, distribution ="asymptotic")
i2c <- independence_test(ratifications_201517 ~ demos, data=cntry_full, distribution ="asymptotic")
i3c <- independence_test(major_agreement_selection ~ demos, data=cntry_full, distribution ="asymptotic")

independence_test(eigenvector + GDPpc + demos ~ overall_membership, data=cntry_full, teststat = "quadratic", distribution = approximate (B = 10000))



i1
i2
i3

i1b
i2b
i3b

i1c
i2c
i3c

?independence_test
#?as.factor
?coin

##### output into stargazer -> latex
#install.packages("stargazer")
library(stargazer)
?stargazer


## summary statistics
stargazer(cntry_full, float =TRUE, float.env = "table*")
, summary = TRUE, type = "text", out = "sum.txt")
stargazer(cntry_full, summary = FALSE, type = "text", out = "data.txt")

## glm sum
stargazer(glm2, glm1, glm3)
, 
          type="text",
         out="models.txt")

#stargazer(nb1, nb2, nb3, 
          type="text",
          out="nbmodels.txt", omit.stat = c("ll","rsq")

stargazer(lm5)
          , type="text", out="core-lm")          
          
stargazer(nb1, nb2, nb3)

## permutation sum
?stargazer
stargazer(i1)

## Beispiel Graph
library(readxl)
beispiel <- read_excel("C:/Users/Arne Langlet/Desktop/MAIB/Masterarbeit/Staaten Scores/beispiel.xlsx")
View(beispiel)


beispiel$'Social Capital' <- as.numeric(beispiel$`Social Capital`)
scatterplot("Environmental Treaty Ratifications" ~  'Social Capital', data=beispiel)
with(beispiel,plot('Environmental Treaty Ratifications', 'Social Capital'))
