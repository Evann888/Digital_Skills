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

####################################
####################################