
# Packages ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(tidytext)
library(R.utils)
library(wordcloud)


# set working directory ----
setwd("~/Downloads/CC-Qualit-master")

# Load data ----
# The survey responses
sust_data <- read.csv("sust_behaviour.csv")

# A lookup table which connects each column in `sust_data` to the actual question on the survey
sust_lookup <- read.csv("sust_lookup.csv")

# A list of boring and non-useful words, bundled with `tidytext`
data(stop_words)


# Inspect the data ----
head(sust_data)

str(sust_data)

head(sust_lookup)

# Inspect a column
unique(sust_data$sustainability_daily_think)
str(sust_data$sustainability_daily_think)


# Fix data ----
# Make column an ordered factor
sust_data$sustainability_daily_think <- factor(sust_data$sustainability_daily_think,
																							 levels=c("Never", "Rarely", "Sometimes", "Often", "All the time"), 
																							 ordered=TRUE)

# Remove NAs from a column
sust_data$energy_action_n <- nchar(as.character(sust_data$energy_action))

# Diverging stacked bar chart ----

# Create a summary dataframe of likert responses to a single question
sust_think_summ_wide <- sust_data %>%
	group_by(gender, sustainability_daily_think) %>%
	tally() %>%
	mutate(perc = n / sum(n) * 100) %>%
	dplyr::select(-n) %>%
	group_by(gender) %>%
	spread(sustainability_daily_think, perc)

sust_think_summ_hi_lo <- sust_think_summ_wide %>%
	mutate(midlow = Sometimes / 2,
				 midhigh = Sometimes / 2) %>%
	dplyr::select(gender, Never, Rarely, midlow, midhigh, Often, `All the time`) %>%
	gather(key = response, value = perc, 2:7) %>%
	`colnames<-`(c("gender", "response", "perc"))

# Split data into high and low groups
sust_think_summ_hi <- sust_think_summ_hi_lo %>%
	filter(response %in% c("All the time", "Often", "midhigh")) %>%
	mutate(response = factor(response, levels = c("All the time", "Often", "midhigh")))

sust_think_summ_lo <- sust_think_summ_hi_lo %>%
	filter(response %in% c("midlow", "Rarely", "Never")) %>%
	mutate(response = factor(response, levels = c("Never", "Rarely", "midlow")))

# Use RColorBrewer to store a preset diverging colour palette as a vector of colour codes 
legend_pal <- brewer.pal(name = "RdBu", n = 5)

# Duplicate the middle value, remember that "Sometimes" is actually two groups, "midhigh" and "midlow"
legend_pal <- insert(legend_pal, ats = 3, legend_pal[3])

# Replace the ugly white colour for "Sometimes" with a pleasant dishwater grey
legend_pal <- gsub("#F7F7F7", "#9C9C9C", legend_pal)

# Assign names to the vector based on the colours we want for each group
names(legend_pal) <- c("All the time", "Often", "midhigh", "midlow", "Rarely", "Never" )

# Make plot
ggplot() + 
	geom_bar(data=sust_think_summ_hi, aes(x = gender, y=perc, fill = response), stat="identity") +
	geom_bar(data=sust_think_summ_lo, aes(x = gender, y=-perc, fill = response), stat="identity") + 
	geom_hline(yintercept = 0, color =c("black")) + 
	scale_fill_manual(values = legend_pal, 
										breaks = c("All the time", "Often", "midhigh", "Rarely", "Never"),
										labels = c("All the time", "Often", "Sometimes", "Rarely", "Never")) +
	coord_flip() + 
	labs(x = "Gender", y = "Percentage of respondents (%)") + 
	ggtitle(sust_lookup$survey_question[sust_lookup$column_title == "sustainability_daily_think"]) +
	theme_classic() 

# Basic stacked bar chart ----
	
# Count the number of actions performed by counting characters
sust_data$energy_action_n <- nchar(as.character(sust_data$energy_action))

# Create a colour palette
male_female_pal <- c("#0389F0", "#E30031")
names(male_female_pal) <- c("Male", "Female")

# Create plot
ggplot(sust_data, aes(x =energy_action_n, fill = gender)) + 
	geom_bar() + 
	scale_fill_manual(values = male_female_pal) + 
	scale_x_continuous(breaks = seq(1:8)) +
	theme_classic()

# Bubble plot ----

# Create tally by age and how often thinking about actions
sust_bubble <- sust_data %>%
	group_by(age, sustainability_daily_think) %>%
	tally()

# Create plot
ggplot(sust_bubble, aes(x = age, y = sustainability_daily_think)) +
	geom_point(aes(size = n)) + 
	theme_classic()

# Text mining ----

# Explore a column
sust_data$energy_action_comment


# Create gathered comments
sust_comm_gather <- sust_data %>% 
	dplyr::select(id, gender, energy_action_comment, 
								food_action_comment, water_action_comment, 
								waste_action_comment, other_action_comment) %>%
	gather(action, comment, -id, -gender) %>%
	mutate(comment = as.character(comment))

# Transform to one word per row and remove boring words
sust_comm_tidy <- sust_comm_gather %>%
	group_by(gender) %>%
	unnest_tokens(output = comment_word,
								input = comment) %>%
	anti_join(stop_words, by = c("comment_word" = "word")) %>%
	count(comment_word, sort = TRUE)  %>%
	filter(n > 10) %>%
	filter(!is.na(comment_word)) 

# Create plot by gender
ggplot(sust_comm_tidy, aes(x = comment_word, y = n, fill = gender)) + 
	geom_bar(stat = "identity") + 
	coord_flip() + 
	scale_fill_manual(values = male_female_pal) + 
	theme_classic()


# The same, but only for one question
tidy_energy_often_comment <- sust_data %>%
	mutate(energy_action_comment = as.character(energy_action_comment)) %>%
	unnest_tokens(output = energy_action_comment_word,
								input = energy_action_comment) %>%
	anti_join(stop_words, by = c("energy_action_comment_word" = "word")) %>%
	count(energy_action_comment_word, sort = TRUE) 


tidy_energy_often_comment_summ <- tidy_energy_often_comment %>%
	filter(n > 10) %>%
	filter(!is.na(energy_action_comment_word)) %>%
	mutate(energy_action_comment_word = reorder(energy_action_comment_word, n ))

ggplot(tidy_energy_often_comment_summ, aes(x = energy_action_comment_word, y = n)) +
	geom_col() +
	xlab(NULL) +
	coord_flip() + 
	theme_classic()

# Wordcloud

tidy_energy_often_comment %>%
	with(wordcloud(words = energy_action_comment_word, freq = n, max.words = 100))

# Statistical analysis ----

# Chi squared
gender_think_chi <- chisq.test(sust_data$gender, sust_data$sustainability_daily_think)
gender_think_chi

# Poisson regression
energy_action_pois <- glm(energy_action_n ~ gender, family = "poisson", data = sust_data)
summary(energy_action_pois)

# Multi-variate poisson
energy_action_pois_int <- glm(energy_action_n ~ gender * age, family = "poisson", data = sust_data)
summary(energy_action_pois_int)


