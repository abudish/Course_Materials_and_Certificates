df <- read.csv("grants.csv")
str(df)

#One way
df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

# Alternative
df$status <- factor(df$status, labels = c("Not funded", "Funded"))

# 1d table
t1 <- table(df$status)
t1

dim(t1)

# 2d table
t2 <-  table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)
t2

dim(t2)

prop.table(t2) # proportions

prop.table(t2, 1) # sum by row 100%
prop.table(t2, 2) # sum by column 100%

# 3d table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)

#######
#Binomial test
######
binom.test(x=5, n=20, p=0.5)
binom.test(t1)

# Chi-square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$observed
chi$expected

t2
chisq.test(t2)


# Fisher's Exact Test
fisher.test(t2)





########
#К частям таблицы можно обращаться так же, как и к матрицам. 
#HairEyeColor - таблица с данными, встроенными в R. 
#Посмотрите на неё в R. Команда dimnames(HairEyeColor) позволит нам посмотреть,
#какие измерения есть в этой таблице и как они называются.
HairEyeColor
dimnames(HairEyeColor)

#Например, чтобы обратиться к части таблицы, в которой хранятся данные только о мужчинах,
#нам нужно выполнить следующую команду: 
HairEyeColor[ , ,'Male']

# Ваша задача в переменную red_men 
# сохранить долю рыжеволосых (Red) от общего числа голубоглазых мужчин.
blueMaleProportions <- prop.table(HairEyeColor[, 'Blue', 'Male'])
red_men <- blueMale['Red']


# С таблицами, как и с матрицами, можно совершать разные арифметические операции,
# например, суммировать все элементы таблицы.

#Напишите число зеленоглазых женщин в наборе данных HairEyeColor.
sum(HairEyeColor[ ,'Green','Female'])



# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин
# из таблицы HairEyeColor. По оси X должен идти цвет волос,
# цвет столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.

# Постройте график на основе предложенного кода, сохранив его в переменную obj. 
# Укажите, чему равен аргумент data, что должно находиться в aes(). 
# Изучите справку по geom_bar(), чтобы узнать, 
# чему должен равняться аргумент position для отображения цвета глаз в виде соседних столбиков.
# Там же вы найдёте ответ на вопрос, за что отвечает аргумент stat.
# С помощью scale_fill_manual мы говорим графику, что мы хотим, чтобы он использовал указанные нами цвета. 
# Дополните предложенный код:
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = mydata[mydata$Sex =="Female", ],
              aes(x = Hair, y = Freq, fill = Eye)) + 
        geom_bar(stat="identity", position = "dodge") + 
        scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj


# На основе таблицы HairEyeColor создайте ещё одну таблицу, 
# в которой хранится информация о распределении цвета глаз у женщин-шатенок (Hair = 'Brown').
# Проведите тест равномерности распределения цвета глаз у шатенок 
# и выведите значение хи-квадрата для этого теста.
brownHairFemale <- HairEyeColor[ 'Brown', ,'Female']
chisq.test(brownHairFemale)






# Воспользуемся данными diamonds из библиотеки ggplot2.
# При помощи критерия Хи - квадрат проверьте гипотезу
# о взаимосвязи качества огранки бриллианта (сut) и его цвета (color).
# В переменную main_stat сохраните значение статистики критерия Хи - квадрат.
# Обратите внимание, main_stat должен быть вектором из одного элемента, а не списком (листом).
diamonds
d <- table(cut = diamonds$cut, color = diamonds$color)
d
chi_result <- chisq.test(d)
main_stat <- chi_result$statistic



# При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) 
# и каратов (carat) бриллиантов.
# Для этого сначала нужно перевести эти количественные переменные
# в формат пригодный для Хи - квадрат. 
#Создайте две новые переменные в данных diamonds:
        # factor_price - где будет 1, если значение цены больше либо равно чем среднее, 
        # и 0, если значение цены ниже среднего цены по выборке.

diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0)) 
#factor_price <-  as.numeric(diamonds$price >= mean(diamonds$price))
        # factor_carat - где будет 1, если число карат больше либо равно чем среднее,
        # и 0, если ниже среднего числа карат по выборке.
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)) 
#factor_carat <- as.numeric(diamonds$carat >= mean (diamonds$carat))

# Используя эти шкалы при помощи Хи - квадрат проверьте исходную гипотезу.
chi_result <- chisq.test(diamonds$factor_price, diamonds$factor_carat)
#  Сохраните в переменную main_stat значение критерия  Хи - квадрат.
main_stat <- chi_result$statistic



# При помощи точного критерия Фишера проверьте гипотезу
# о взаимосвязи типа коробки передач (am) и типа двигателя (vs) в данных mtcars.
# Результат выполнения критерия сохраните в переменную.
fisher_cars <- fisher.test(mtcars$am, mtcars$vs)

# Получившийся p - уровень значимости сохраните в переменную fisher_test.
fisher_test <- fisher_cars$p.value
