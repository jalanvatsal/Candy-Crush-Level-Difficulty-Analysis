#Loading package
library(tidyverse)

#Setting Reasonable Plot Dimensions
options(repr.plot.width = 5, repr.plot.height = 4)

#Read-in Data
candy_crush <- read_csv("candy_crush.csv")
head(candy_crush)

#Calculate the Number of Unique Players
length(unique(candy_crush$player_id))

#Calculate the Range of Dates for Which we are Working With
range(candy_crush$dt)

#Calculating Level Difficulty by Calculating Probability of Beating a Level Given Attempts and Wins
level_difficulty <- candy_crush %>%
  group_by(level) %>%
  summarise(wins = sum(num_success),
            attempts = sum(num_attempts)) %>% 
  mutate(p_win = wins / attempts)

max(level_difficulty$p_win)

#Data Visualization of Level Difficulty
ggplot(level_difficulty, 
         aes(level,p_win)) +
         geom_point() + geom_line(colour = "red") +
  scale_x_binned(breaks = 1:15) +
  scale_y_continuous(breaks = seq(0, 7, by = 0.1)) + 
  labs(y = "Probability of Clearing the Level",
       x = "Level") + 
  geom_hline(yintercept = 0.10, linetype = "dashed")

#Computing Standard Error for each level for Bernoulli process
level_difficulty <- level_difficulty %>% 
  mutate(standard_error = sqrt(p_win * (1 - p_win)) / sqrt(attempts))

#Visualizing Error Bars, whose lengths reflect ne Standard Error 
ggplot(level_difficulty, 
       aes(level,p_win)) +
  geom_point() + geom_line(colour = "red") +
  scale_x_binned(breaks = 1:15) +
  scale_y_continuous(breaks = seq(0, 7, by = 0.1)) + 
  labs(y = "Probability of Clearing the Level",
       x = "Level") + 
  geom_hline(yintercept = 0.10, linetype = "dashed") +
  geom_errorbar(aes(ymin = p_win - standard_error, ymax = p_win + standard_error))

#Question: What is the probability that someone clears all levels without failing once?
prod(level_difficulty$p_win)

#Question: What is the hardest level? And what is the probability of passing this level once if someone attempts it 100 times?

#Calculating the Hardest Level
level_difficulty[level_difficulty$p_win == min(level_difficulty$p_win),]

#Calculation of 2nd Question using the Binomial Probability Distribution
dbinom(1, size = 100, prob = 0.0381)


  
