## Illustrating interactions

library(ggplot2)

x <- 1:30 + rnorm(30)
y1 <- x * 1.5 + rnorm(30)
y2 <- y1 + 7 + rnorm(30, 0, 0.3)

dat <- data.frame(x = rep(x, 2), A = as.factor(rep(1:2, each = 30)), y = c(y1,y2))

ggplot(data = dat, aes(x = x, y = y, color = A)) + 
  geom_point() + 
  ggtitle("No interaction")
ggsave("code/chapter_08/no-interaction.png")

x <- 1:30 + rnorm(30)
y1 <- x * 1.5 + rnorm(30)
y2 <- x * -1.5 + rnorm(30, 0, 0.5) + 50

dat1 <- data.frame(x = rep(x, 2), A = as.factor(rep(1:2, each = 30)), y = c(y1,y2))

ggplot(data = dat1, aes(x = x, y = y, color = A)) + 
  geom_point() + 
  ggtitle("Strong interaction (cross-over)")
ggsave("code/chapter_08/cross-over-interaction.png")

x <- 1:30 + rnorm(30)
y1 <- x * 1.5 + rnorm(30)
y2 <- x * 2.5 + rnorm(30, 0, 0.5) 

dat2 <- data.frame(x = rep(x, 2), A = as.factor(rep(1:2, each = 30)), y = c(y1,y2))

ggplot(data = dat2, aes(x = x, y = y, color = A)) + 
  geom_point() + 
  ggtitle("Interaction")
ggsave("code/chapter_08/interaction.png")
