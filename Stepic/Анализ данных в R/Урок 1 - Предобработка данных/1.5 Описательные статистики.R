#Рассчитайте среднее значение времени разгона (qsec) для автомобилей,
# число цилиндров (cyl) у которых не равняется 3 
# и показатель количества миль на галлон топлива (mpg) больше 20.
result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20 ])


# При помощи функции aggregate рассчитайте стандартное отклонение 
# переменной hp (лошадиные силы) и переменной disp (вместимости двигателя)
# у машин с автоматической и ручной коробкой передач. 
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)



#При помощи функции aggregate рассчитайте количество 
# непропущенных наблюдений по переменной Ozone в 7, 8 и 9 месяце. 
# Для определения количества наблюдений используйте функцию length(). 
aggregate(Ozone ~ Month, 
          data = subset(airquality, Month %in% c(7, 8, 9)), 
          FUN = length)


# Note!
# Difference how NAs removed in aggregate(), describe(), describeBy()


#Примените функцию describeBy к количественным переменным данных airquality,
# группируя наблюдения по переменной Month. 
descr <- describeBy(x = airquality[, -c(5,6)], group = airquality$Month)
descr
# Чему равен коэффициент асимметрии (skew) переменной Wind в восьмом месяце?
descr$`8`$skew[3] # примерно 0.04


# Обратимся к встроенным данным iris. 
# Соотнесите значения стандартного отклонения переменных.
descr <- describeBy(x = iris[, -5])
descr[, c(1,4)]

# В данных iris расположите по убыванию значения медиан
# количественных переменных в группе virginica.
descr <- describeBy(x = iris[, -5], group = iris$Species)
descr$virginica[order(descr$virginica$median, decreasing = T),
                c(1, 5)]


#В переменной my_vector сохранен вектор с пропущенными значениями.
# Вам нужно создать новый вектор fixed_vector, 
#в котором все пропущенные значения вектора my_vector будут заменены
#на среднее значение по имеющимся наблюдениям.

#При этом исходный вектор оставьте без изменений!

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA
# Попробуйте решить это задание при помощи функции replace()
fixed_vector <- replace(x = my_vector, 
        list = is.na(my_vector),
        values = mean(my_vector, na.rm = T))