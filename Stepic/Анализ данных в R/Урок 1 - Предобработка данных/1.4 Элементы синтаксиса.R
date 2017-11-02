good_months <- c()
i <- length(AirPassengers)
for (i in 1:(i - 1)) {
        if (AirPassengers[i+1] > AirPassengers[i]) {
                good_months <- c(good_months, AirPassengers[i+1])
        }
}

moving_average <- c()

for (i in 1:(i-9)) {
        moving_average <- c(moving_average, mean(AirPassengers[i:(i+9)]))
}

# Более быстрая и правильная альтернатива
moving_average <- numeric(135) # создаем пустой числовой вектор из 135 элементов    
last_index <- length(AirPassengers) - 9    
for (i in 1:last_index) {    
        end <- i + 9    
        moving_average[i] <- mean(AirPassengers[i:end])    
} 