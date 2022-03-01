load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

plot(x = movies$imdb_num_votes, y = movies$audience_score)
slr1 <- lm(audience_score ~ imdb_num_votes, data = movies)
summary(slr1)
abline(slr1)
qqnorm(slr1$residuals)
qqline(slr1$residuals)

plot(x = movies$critics_score, y = movies$audience_score)
slr2 <- lm(audience_score ~ critics_score, data = movies)
summary(slr2)
abline(slr2)
qqnorm(slr2$residuals)
qqline(slr2$residuals)


slr3 <- lm(audience_score ~ best_pic_nom, data = movies)
summary(slr3) 
plot(x = movies$best_pic_nom, y = movies$audience_score)
qqnorm(slr3$residuals)
qqline(slr3$residuals)


slr4 <- lm(audience_score ~ best_dir_win, data = movies)
summary(slr4)
plot(x = movies$best_dir_win, y = movies$audience_score)
qqnorm(slr4$residuals)
qqline(slr4$residuals)


slr5 <- lm(audience_score ~ top200_box, data = movies)
summary(slr5)
plot(x = movies$top200_box, y = movies$audience_score)
qqnorm(slr5$residuals)
qqline(slr5$residuals)

plot(x = movies$critics_score, y = movies$imdb_num_votes)
slr6 <- lm(imdb_num_votes ~ critics_score, data = movies)
abline(slr6)
qqnorm(slr6$residuals)
qqline(slr6$residuals)

m_Mmovies <- lm(audience_score ~  runtime+thtr_rel_year+imdb_num_votes + critics_score +critics_rating , data = movies) 

hist(m_Mmovies$residuals)
qqnorm(m_Mmovies$residuals)
qqline(m_Mmovies$residuals)
plot(abs(m_Mmovies$residuals) ~m_Mmovies$fitted)
abline(h = 0, lty = 3)
plot(m_Mmovies$residuals)  
plot(m_Mmovies$residuals~movies$imdb_num_votes)
plot(m_Mmovies$residuals~movies$critics_score)
plot(m_Mmovies$residuals~movies$runtime)
plot(m_Mmovies$residuals~movies$thtr_rel_year)
plot(m_Mmovies$residuals~movies$critics_rating)
    


m = lm(audience_score ~  runtime+thtr_rel_year+imdb_num_votes + critics_score +critics_rating , data = movies)
mov = data.frame(imdb_num_votes = 168202, critics_score = 97, thtr_rel_year= 2013, runtime=90, critics_rating= "Certified Fresh")
predict(m , mov , interval = "prediction")

