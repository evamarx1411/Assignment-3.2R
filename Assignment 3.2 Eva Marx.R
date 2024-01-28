#### Assignment 3.2R Eva Marx

## Q1
remind_me <- function() {
  reminders <- list(
    list(date = "1998-06-24", event = "Madeleine's Birthday"),
    list(date = "2001-09-20", event = "Mareen's Birthday"),
    list(date = "1962-03-25", event = "Mum's Birthday"),
    list(date = "1960-04-30", event = "Dad's Birthday"),
    list(date = "2000-11-14", event = "My Birthday")
    # more reminders could be added here
  )
  
  currentDay <- as.POSIXlt(Sys.time())$mday
  currentMonth <- as.POSIXlt(Sys.time())$mon + 1 # current month (January is 1)
  
  for (reminder in reminders) {
    reminderDate <- as.Date(reminder$date)
    reminderDay <- as.POSIXlt(reminderDate)$mday
    reminderMonth <- as.POSIXlt(reminderDate)$mon + 1
    
    if (currentDay == reminderDay && currentMonth == reminderMonth) {
      message(paste("Reminder:", reminder$event, "today!"))
    }
  }
}


cheat <- function(exercise_number) {
  if (exercise_number == 1) {
    cat('set.seed(123)\n')
    cat('minGrade <- 1\n')
    cat('maxGrade <- 10\n')
    cat('meanGrade <- 7.5\n')
    cat('n <- 60\n')
    cat('\n')
    cat('possibleGrades <- seq(minGrade, maxGrade, by = 0.5)\n')
    cat('probabilities <- dnorm(possibleGrades, mean = meanGrade, sd = 1.5) / sum(dnorm(possibleGrades, mean = meanGrade, sd = 1.5))\n')
    cat('\n')
    cat('simulatedGrades <- sample(possibleGrades, n, replace = TRUE, prob = probabilities)\n')
    cat('hist(simulatedGrades, main = "Simulated Grades PIPS 2024", xlab = "Simulated Grades")\n')
    
  } else if (exercise_number == 2) {
    cat('schipholData <- read.table("schiphol_data.txt", sep = ",", header = TRUE)\n')
    cat('plot(x = schipholData$DATE, y = schipholData$TMIN, xlab = "Time", ylab = "Minimum Temp", main = "Minimum Temp at Schiphol Airport")\n')
    
  } else if (exercise_number == 3) {
    cat('library(ggplot2)\n')
    cat('library(titanic)\n')
    cat('\n')
    cat('titanicData <- titanic_train\n')
    cat('\n')
    cat('ggplot(data = titanicData, aes(x = Sex, fill = factor(Survived, labels = c("dead", "alive")))) +\n')
    cat('  geom_bar(position = "stack") +\n')
    cat('  labs(x = "Sex",\n')
    cat('       y = "Count",\n')
    cat('       fill = "How did it go?")\n')
    
  } else {
    cat("Invalid exercise number. Please choose a valid exercise (1, 2, or 3).\n")
  }
}

# github repository for this exercise: https://github.com/evamarx1411/Assignment-3.2R


## Q2
library(sf)
library(ggplot2)

make_art <- function(seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  polygon1 <- sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
  polygon2 <- sf::st_polygon(list(cbind(c(0.15, 0.85, 0.85, 0.15, 0.15), c(0.15, 0.15, 0.85, 0.85, 0.15))))
  polygon1Color <- sprintf("#%02X%02X%02X", sample(0:255, 1), sample(0:255, 1), sample(0:255, 1))
  polygon2Color <- sprintf("#%02X%02X%02X", sample(0:255, 1), sample(0:255, 1), sample(0:255, 1))
  
  dots1 <- data.frame(x = runif(100, 0.16, 0.84), y = runif(100, 0.16, 0.84))
  dots2 <- data.frame(x = runif(100, 0.16, 0.84), y = runif(100, 0.16, 0.84))
  dots1Color <- sprintf("#%02X%02X%02X", sample(0:255, 1), sample(0:255, 1), sample(0:255, 1))
  dots2Color <- sprintf("#%02X%02X%02X", sample(0:255, 1), sample(0:255, 1), sample(0:255, 1))
  
  ggplot() +
    geom_sf(data = polygon1, fill = polygon1Color, colour = polygon1Color) +
    geom_sf(data = polygon2, fill = polygon2Color, colour = polygon2Color) +
    geom_point(data = dots1, mapping = aes(x = x, y = y), colour = dots1Color, size = 0.1) +
    geom_point(data = dots2, mapping = aes(x = x, y = y), colour = dots2Color, size = 0.1) +
    coord_sf(expand = FALSE) +
    theme_void()
}