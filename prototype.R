library(tidyr)
library(dplyr)
library(ggplot2)

# This file is meant to prototype the raw data access and graphing capabilities

# Read data in from 538
polls538 <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv"))

#Filter data to relevant polls
main_data <- polls538 %>%
               filter(party=="DEM") %>%
               filter(cycle==2020) %>%
               filter(state=="") %>%
               filter(fte_grade %in% c("A","A-")) %>%
               select(question_id,poll_id,pollster,end_date,candidate_name,pct) %>%
               filter(poll_id==62715)

main_data_new <- main_data %>%
                 filter(pct>3)

remaining <- (100-sum(main_data_new$pct))
even_split_remaining <- remaining/count(main_data_new)[[1]]

remaining_candidates <- unique(main_data_new$candidate_name)

main_data$pct_new <- main_data$pct + (main_data$pct>=3)*even_split_remaining
main_data$pct_new <- main_data$pct + (main_data$candidate_name==remaining_candidates[3])*remaining
main_data$pct_new[main_data$pct<3] <- NA


#Plot results for each candidate
cutoff <- data.frame(yintercept=3, cutoff=factor(3))
ggplot(data = main_data) +
  geom_point(colour="black",fill="black", shape=21, size = 1,aes(x = candidate_name, y=pct)) +
  geom_point(colour="blue",fill="blue", shape=22, size = 1,aes(x = candidate_name, y=pct_new)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff,show.legend = FALSE) 


# TO DO:
# - Take this basic concept and port to Shiny. Add:
#    1. Ability to customize which poll you use
#    2. Threshold below which candidates drop
#    3. Potential Reallocation rules
#    4. Graph labeling
