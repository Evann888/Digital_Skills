mod1lab1 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/Mod1Lab1.csv",
    sep = ','
  )
min(mod1lab1$age)

mean(mod1lab1[11:20,2])

x <- c(3, 6, 2, 3, 1)
sd(x)
sd(c(2,1,5,3,1,2,2,3,1))

####################################
####################################
mod2lab <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/Mod2Lab.csv",
    sep = ','
  )
hist(mod2lab$coffee)

num = c(1, 4, 2, 7, 19)
mean_num <- mean(num)
med_num <- median(num)
sd_num <- sd(num)

3*(mean_num-med_num)/sd_num

skewness(num)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modmeanmed <- c(1, 2, 4, 6, 8, 11, 11, 13)
mean(modmeanmed)
getmode(modmeanmed)
median(modmeanmed)

data_median <- c(97, 99, 101, 101, 108, 110, 127, 199, 309, 400)
median(data_median)

data_sd_iqr <- c(33, 37, 45, 46, 51, 60, 65)
sd(data_sd_iqr)

#https://lagunita.stanford.edu/courses/course-v1:Medicine+IPE21CC+ongoing/wiki/HRP258/example-r-classwork-solutions-using-r/calculating-inner-quartile-range-r/
#have 9 different type calculate iqr (in course use type 2)
Q3 <- quantile(data_sd_iqr, 0.75, type = 2) 
Q1 <- quantile(data_sd_iqr, 0.25, type = 2)  
print (Q3 - Q1)

IQR(data_sd_iqr, type = 7)

data_sd_iqr_2 = c(2, 3, 5, 11, 22, 25, 26)
IQR(data_sd_iqr_2, type = 2)

# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
ggplot(mod2lab, aes(x=preference, fill=preference)) +
  geom_bar(stat="count") +  
  geom_text(stat="count", aes(label=..count..),vjust=1.6, color="white", size=3.5) +
  theme_minimal()

#3 variabel, x=factor, y=count, fill=black(category)
ggplot(mod2lab, aes(x=preference, fill=black)) +
  geom_bar(stat="count", position=position_dodge()) +  
  geom_text(stat="count",aes(label=..count..), vjust=1.6, color="white",position = position_dodge(0.9), size=3.5) +
  theme_minimal()

####################################
##############KPI###################
####################################
mod4lab1 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/Mod4Lab1.csv",
    sep = ','
  )
mod4lab1_first10 <- head(mod4lab1,10)
mean(mod4lab1_first10$timetomarket)

mod4lab2 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/Mod4Lab2.csv",
    sep = ','
  )

ggplot(mod4lab2, aes(x=resolved.in.sla, fill=resolved.in.sla)) +
  geom_bar(stat="count", position=position_dodge()) +  
  geom_text(stat="count",aes(label=..count..), vjust=1.6, color="white",position = position_dodge(0.9), size=3.5) +
  theme_minimal()

mod4lab3 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/Mod4Lab3.csv",
    sep = ','
  )
sum(mod4lab3$revenue)
(12.5+126)/31
138.5/31

df.average_revenue.id <- mod4lab3 %>% group_by(userid) %>%
                          summarise(average_revenue.id = mean(revenue))

mod4lab4 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/Mod4Lab4.csv",
    sep = ','
  )

ggplot(mod4lab4, aes(x=period, y=revenue, fill=period)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  theme_minimal()
211177/141875*100

496/142
486.5/139

# Final challenge
data_tail <- c(3,5,12,9,2,5,7,10,1)

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

sum = 0
for (i in 1:length(data_tail)){
  print(3*data_tail[i]-2)
  sum = sum + 3*data_tail[i]-2
}
sum

(126245 - 124311) /124311

final_data_iqr = c(860,1200,500,550,680,1100,450,900,1050)
IQR(final_data_iqr, type = 3)

shoulder_height <- c(12,21,34,22,10,18,20,30,8)
mean(shoulder_height)

4/16*100
f <- c(62000,54500,33400,101260,155000,46000,40000,87700,24000,50000,41250,38000,210500,90000,60500,51250,46000)
median(f)

final_data_sd <- c(100, 107, 103, 114, 119, 117, 130, 128, 122, 101)
sd(final_data_sd)

final_data_iqr2 <- c(260, 275, 275, 290, 310, 355, 410)
IQR(final_data_iqr2, type = 2)

df.final_challenge1 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/ChLab1.csv",
    sep = ','
  )
View(df.final_challenge1)
names(df.final_challenge1)[2:3] <- c("sold.per.region","region.type")

df.final_challenge1 %>% group_by(region.type) %>%
  summarise(average_time = mean(sold.per.region))

nrow(df.final_challenge1)
#p-value kecil artinya Ho diterima, yang berarti tren akan berlanjut kedepan
t.test(sold.per.region~region.type,data = df.final_challenge1)

df.final_challenge2 <-
  read.csv(
    "E:/ITS/KPP PELATIHAN DLL/DigiTalent 2019/DA/Essential-Math-for-Data-Analysis/ChLab2.csv",
    sep = ','
  )
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

ggplot(df.final_challenge2, aes(x=period, y=revenue, fill=revenue)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  theme_minimal()

ggplot(mod4lab4, aes(x=period, y=revenue, fill=period)) +
  geom_bar(stat="identity", position=position_dodge()) +  
  theme_minimal()

mean(df.final_challenge2[1:8,3])
