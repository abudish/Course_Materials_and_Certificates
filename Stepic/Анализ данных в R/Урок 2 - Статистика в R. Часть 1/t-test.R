?iris
df  <- iris

str(df)

df1  <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x =Sepal.Length ))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#The same result in one line
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)

bartlett.test(Sepal.Length  ~ Species, df1)


t.test(Sepal.Length  ~ Species, df1)
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

t.test(Sepal.Length  ~ Species, df1, var.equal = T)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)


?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()


wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value





################
####### Задания ##########
# Задача про свинок!
#   
# Воспользуемся еще одним встроенным набором данных в R  - ToothGrowth.
# Данные позволяют исследовать рост зубов у морских свинок 
# в зависимости от дозировки витамина C и типа потребляемых продуктов.

library(dplyr)
# Сравните среднее значение длины зубов свинок, 
# которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм,
# со средним значением длины зубов свинок,
# которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 

tooth_1 <- ToothGrowth %>% 
  filter(supp == 'OJ', dose == 0.5) %>% 
  pull(len)
tooth_2 <- ToothGrowth %>% 
  filter(supp == 'VC', dose == 2) %>% 
  pull(len)  
# Значение t - критерия сохраните в переменную t_stat.
t_stat <- t.test(tooth_1, tooth_2)$statistic

# Альтернативное решение
correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)    
t_stat <- t.test(len ~ supp, correct_data)$statistic



# Скачайте данные, посвященные влиянию различного типа лечения на показатель 
# артериального давления. 
# https://stepic.org/media/attachments/lesson/11504/lekarstva.csv
drugs <- read.csv("lekarstva.csv")
# По всем испытуемым сравните показатель давления до начала лечения 
# (Pressure_before) с показателем давления после лечения (Pressure_after)
# при помощи t - критерия для зависимых выборок. 
drugs_t_test <- t.test(drugs$Pressure_before, drugs$Pressure_after, paired = T)

# В поле для ответа укажите значение t - критерия.
#(В качестве десятичного разделителя используйте запятую, например: 123,54)
drugs_t_test$statistic



# В этом задании нужно проверить гипотезу о равенстве средних двух выборок, 
# загрузив набор данных (нажмите начать решать задание) и выполнив все
# необходимые операции на вашем компьютере. В скачанных данных вы найдете 
# две переменные: количественную переменную, и номинативную переменную с
# двумя градациями (которая разделяет наблюдения на две группы).
df <- read.table("dataset_11504_15.txt")


# Сначала с помощью теста Бартлетта проверьте гомогенность дисперсий двух выборок.
# В случае, если дисперсии значимо не отличаются (с уровнем 0.05), примените тест
# Стьюдента, иначе - непараметрический тест (Манна-Уитни). В поле для ответа введите
# получившийся p-value, с точностью четыре знака после запятой.
# Обратите внимание, что по умолчанию в t.test стоит var.equal = FALSE, 
# так как мы будем применять его только в случае гомогенности дисперсий, 
# измените значение этого параметра на  var.equal = TRUE.

bartlett.test(V1 ~ V2, df)

wilcox.test(V1~V2, df, paired = T)
t.test(V1~V2, df, paired = T, var.equal = T)


# В этом задании также необходимо скачать данные на ваш компьютер.
# В данных сохранены две количественные переменные, проверьте гипотезу о равенстве
# средних этих переменных при помощи t- теста для независимых выборок.
# 
# Если обнаружены значимые различия (p< 0.05), то введите через пробел три числа: 
#   среднее значение первой переменной, среднее значение второй переменной, 
# p - уровень значимости. Например:
#   
#   22.45 12.56 0.04
# 
# Если значимые различия не обнаружены, то в поле для ответа введите: 
#   
#   "The difference is not significant"
# В этой задаче оставьте var.equal = FALSE
df2 <- read.table("dataset_11504_16.txt")
t.test(V1~V2, df, var.equal = FALSE)
ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)