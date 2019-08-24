# install all r basic package like caret in rstudio
# conda install --name rstudio -c r r-essentials

####################################
####################################
Lab2 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Data-Analysis-for-Absolute-Beginners-master/Mod2Labs.csv",
    sep = ','
  )

View(Lab2)
cor(Lab2$mpg, Lab2$cyl)
cor(Lab2$gear, Lab2$carb)
mean(Lab2$wt)
Lab2$prior <- with(df, ifelse(a == b, a + b, b - a))

####################################
####################################
Lab3.2 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Data-Analysis-for-Absolute-Beginners-master/Mod3Lab1.csv",
    sep = ','
  )

View(Lab3.2)

####################################
####################################
Lab4 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Data-Analysis-for-Absolute-Beginners-master/Mod4Labs.csv",
    sep = ','
  )
View(Lab4)
median(Lab4$liking)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data <- c(3, 3, 7, 9, 20) #vector

mean(data)
getmode(data)
median(data)
hist(Lab4$runtime)

h <- Lab4[(Lab4[, 2] > 114.39),]
nrow(h)

####################################
####################################
library(dplyr)
Lab5.1 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Data-Analysis-for-Absolute-Beginners-master/Mod5Lab1.csv",
    sep = ','
  )
View(Lab5.1)
summary(Lab5.1)
str(Lab5.1)
Lab5.1$time <- as.numeric(Lab5.1$time)

Lab5.1 %>% group_by(provider, day) %>%
  summarise(average_time = mean(duration..min.))

####################################
####################################
Lab5.2 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Data-Analysis-for-Absolute-Beginners-master/Mod5Lab2.csv",
    sep = ','
  )
View(Lab5.2)

Lab5.2 %>% group_by(class,level) %>%
  summarise(average_overall.cource = mean(overall.course))


final <- c(85,85,99,110,147)
median(final)

#skewness, show distribution
library(e1071)
final_skew <- c(5, 7, 13, 15, 19, 44)
skewness(final_skew, type = 2)


avg_deg <- c(75,71,65,59,45,36,28)
energy_bill <- c(50,55,68,75,90,110,135)

cor(avg_deg,energy_bill)
sd(avg_deg)
mean(energy_bill)

####################################
####################################
#challenge
df.challenge <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Data-Analysis-for-Absolute-Beginners-master/ChallengeLab.csv",
    sep = ','
  )

df.challenge.group_sales <- df.challenge %>% group_by(city) %>%
  summarise(overall_sales = sum(sales))

mean(df.challenge$listings)
cor(df.challenge$sales,df.challenge$inventory)

#Prep data
summary(df.challange)
str(df.challenge)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
commatodot <- function (x) gsub(",", "", x)

for (i in 4:5){
  df.challenge[, i]<- gsub("^\\$", "", df.challenge[, i])
  df.challenge[, i] <- commatodot(df.challenge[, i])
  df.challenge[, i] <- trim(df.challenge[, i])
  df.challenge[, i] <- as.numeric(df.challenge[, i])
}

#####################
#correlation heatmap#
#####################
library(ggplot2)
mydata <- df.challenge[, c(3:7)]
str(mydata)
cormat <- round(cor(mydata),2)

library(reshape2)
cormat
# melt = kolom id jadi id, measure jd baris dan nilainya jadi kolom baru
melted_cormat <- melt(cormat)
melted_cormat

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
