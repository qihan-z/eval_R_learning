knitr::opts_chunk$set(echo = TRUE)
library(dyplr)
library(ggplot2)

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))


qplot(y = audience_score, x = factor(thtr_rel_month), data = movies, geom = "boxplot")

qplot(y = critics_score, x = factor(thtr_rel_month), data = movies, geom = "boxplot")


movies %>% summarise(mean(audience_score, na.rm=TRUE))
movies %>% group_by(thtr_rel_month) %>% summarise(mean(audience_score, na.rm=TRUE))

movies %>% summarise(mean(critics_score, na.rm=TRUE))
movies %>% group_by(thtr_rel_month) %>% summarise(mean(critics_score, na.rm=TRUE))



qplot(y = critics_score, x = audience_score, data = movies)


qplot(y = critics_score, x = audience_score, data = movies)

qplot(y = critics_score, x = imdb_rating, data = movies)


inference(y = audience_score, x = thtr_rel_month, data = movies, statistic = "mean", type = "ht", alternative = "greater", method = "theoretical")


inference(y = critics_score, x = thtr_rel_month, data = movies, statistic = "mean", type = "ht", alternative = "greater", method = "theoretical")


qplot(y = critics_score, x = audience_score, data = movies, geom = "point") +  stat_smooth(method = "lm", se = FALSE)

linreg <- lm(critics_score ~ audience_score, data = movies)
summary(linreg)

qplot(x = .fitted, y = .resid, data = linreg) +  geom_hline(yintercept = 0, linetype = "dashed") +  xlab("Fitted values") +  ylab("Residuals")


qplot(x = .resid, data = linreg, geom = "histogram", binwidth = 5) +  xlab("Residuals")

qplot(sample = .resid, data = linreg, stat = "qq")


movies_complete <- lm(critics_score ~ title_type+ genre + runtime + mpaa_rating+ thtr_rel_year + thtr_rel_month+ imdb_rating + imdb_num_votes + critics_rating + critics_score+ audience_rating + audience_score + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box , data = movies)
summary(movies_complete)



movies_full <- lm(critics_score ~ title_type + thtr_rel_year + imdb_rating, data = movies)



qplot(x = .fitted, y = .resid, data = movies_full) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")+
  ggtitle("Residuals vs. Fitted values")



qplot(data = movies_full, x = .resid, geom = "histogram") +
  xlab("Residuals") +
  ggtitle("Histogram of residuals")

qplot(sample = .resid, data = movies_full, stat = "qq")+
ggtitle("Normal probability plot of residuals")


qplot(data = movies_full, y = .resid) +
geom_hline(yintercept = 0, linetype = "dashed") +
 ylab("Residuals") +
xlab("Order of data collection") +
ggtitle("Residuals vs. Order of data collection")



new_movie <- data.frame(audience_score=82)
predict(linreg, new_movie, interval = "prediction")


qplot(y = critics_score, x = audience_score, data = movies, geom = "point") +  stat_smooth(method = "lm", se = FALSE) + geom_point(aes(82,77.11),colour="red",size=3)


new_movie = data.frame(title_type="Feature Film", thtr_rel_year=2015, imdb_rating=8.3)
predict(movies_full, new_movie, interval="prediction")

