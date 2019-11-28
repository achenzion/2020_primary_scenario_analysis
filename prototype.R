library(tidyr)
library(dplyr)
library(ggplot2)

# This file is meant to prototype the raw data access and graphing capabilities
# This file is not used in the final application.
# see ./2020_Primary_Scenarios/app.R for application

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

orig <- main_data[,c("candidate_name","pct")]
orig$type <- "Raw"
new <- main_data[,c("candidate_name","pct_new")]
names(new) <- c("candidate_name","pct")
new$type <- "Reallocated"

plot_data <- rbind(orig,new)  %>%
  arrange(pct)

#Plot results for each candidate
cutoff <- data.frame(yintercept=3, cutoff=factor(3))
ggplot(data = plot_data,aes(y = pct, x = candidate_name, 
                            shape=type, colour=type)) +
  geom_point() +
  theme_minimal() + 
  labs(x="Candidates",y="Poll Result (%)",title="Poll Results",
       caption = "Source: FiveThirtyEight Latest Polls - https://projects.fivethirtyeight.com/polls/president-primary-d/") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff,show.legend = FALSE) 

