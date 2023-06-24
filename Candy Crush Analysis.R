#Candy Crush Project
library(tidyverse)

#Sets Reasonable Plot Dimensions
options(repr.plot.width = 5, repr.plot.height = 4)

#Read-in data
candy_crush <- read_csv("candy_crush.csv")
head(candy_crush)

#Number of Unique Players
length(unique(candy_crush$player_id))

#Calculate Range of Dates for which we are working with
range(candy_crush$dt)

#Calculating Level Difficulty
level_difficulty <- candy_crush %>%
  group_by(level) %>%
  summarise(wins = sum(num_success),
            attempts = sum(num_attempts)) %>% 
  mutate(p_win = wins / attempts)

max(level_difficulty$p_win)


#Visualization
ggplot(level_difficulty, 
         aes(level,p_win)) +
         geom_point() + geom_line(colour = "red") +
  scale_x_binned(breaks = 1:15) +
  scale_y_continuous(breaks = seq(0, 7, by = 0.1)) + 
  labs(y = "Probability of Clearing the Level",
       x = "Level") + 
  geom_hline(yintercept = 0.10, linetype = "dashed")

#Computing Standard Error for each level
level_difficulty <- level_difficulty %>% 
  mutate(standard_error = sqrt(p_win * (1 - p_win)) / sqrt(attempts))

#Length of Error Bars should reflect One Standard Error 
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


  