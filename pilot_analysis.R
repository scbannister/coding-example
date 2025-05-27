#######################################
#### Kallinen & Ravaja - Pilot Data ###
#### Scott Bannister ##################
#### 16.04.2024 #######################
#######################################

#################################
# Pre-processing stages
#################################

# Reset and clear memory
rm(list=ls(all=TRUE))

# Set working directory
setwd('insert your directory path here') 

# Load relevant R packages - if not installed to machine, use command install.packages("packagename") first!
library(tidyverse)  # Efficient package for tidy data
library(scales)     # For rescaling variables
library(ggpubr)     # For multiplot visualisations
library(ggpattern)  # For specific visualisation approach (I've never used outside of this script!)
library(lmerTest)   # For mixed effects models (or repeated-measures ANOVA that handles missing data)
library(rstatix)    # For repeated-measures ANOVA

# Load user R functions 
source("pilot_cleaning.R") # For data cleaning and preparation for analysis

# Import dataset
dataset <- read_csv('data_file_here.csv', col_names = TRUE)

# Remove incomplete and ineligible responses
dataset <- dataset %>%
  filter(!(Progress < 100)) %>%
  filter(!(Musicianship == 3))

# Generate basic descriptive stats for the dataset
table(dataset$Gender)  # Gender distribution
table(dataset$Nationality) # Nationality frequencies
mean(dataset$Age, na.rm = TRUE)  # Mean age = 49.32
sd(dataset$Age, na.rm = TRUE)    # SD age = 15.75

# Run data cleaning function and change data to long format and prepare for analysis; function outputs two datasets within a "df" list object
df <- pilot_cleaning(dataset)

df_full <- as.data.frame(df[1]) # Here we can extract the full long dataset
df_trim <- as.data.frame(df[2]) # Here we can extract the trimmed dataset
rm(dataset, df) # We no longer need the original dataset or list object from function

############################
# Key data visualisations
############################

# Prepare emotion data for visualisation
viz <- df_trim %>%
  filter(!(condition == 'Anger')) %>% # We filter out the "anger" pieces as per the original study
  pivot_longer(cols = c(pa, na, valence, arousal), names_to = 'emotion', values_to = 'rating') %>%  # move to longer format for visualisation purposes
  group_by(task, condition, emotion) %>%  # Group data by rating task, music emotion condition, and then emotion dimension
  summarise(M = mean(rating, na.rm = TRUE), SE = sd(rating, na.rm = TRUE) / sqrt(length(rating))) # Create mean and standard error summarising stats

# Simple bar chart of felt and perceived emotion dimension ratings, across the three music emotion conditions (fear, joy, sad)
ggplot(viz, aes(x = factor(emotion, level = c('valence', 'arousal', 'pa', 'na')), y = M, fill = task)) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black', size = 0.8) + # Confirm bar chart approach
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), stat = 'identity', position = position_dodge(), color = 'grey30') +  # Add error bars for standard error
  facet_wrap(.~condition) + # Split bar charts into three for each music emotion condition
  scale_fill_manual(values = c("#ff4e36","#f6eee5"), labels = c('Felt', 'Perceived')) + # Add manual colouring to bars
  scale_x_discrete(labels = c('Valence', 'Arousal', 'Pos Act', 'Neg Act')) +  # Update x-axis labels
  geom_hline(yintercept = 0) +  # For neatness we can start the bars at 0 on y-axis
  labs(x = 'Emotion Dimension', y = 'Mean Ratings +- SE', fill = 'Rating Type') + # Update overall axis titles and colour legend
  theme_bw() +  # Set overall simple theme
  theme(axis.title = element_text(face = 'bold', size = 16), title = element_text(face = 'bold', size = 18), axis.text = element_text(size = 11), strip.text = element_text(face = 'bold', size = 10), axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_text(size = 16), legend.position = 'bottom')  #Customise various font elements (e.g., size, boldface...)

# Replicate Table 2 from Kallinen and Ravaja original study
viz <- df_full %>%
  group_by(condition, task, emotion) %>%  # Group data by music emotion condition, rating task, and emotion dimension
  summarise(M = mean(rating, na.rm = TRUE), SD = sd(rating, na.rm = TRUE)) %>%  # Create mean and st. deviation summarising stats
  filter(!(task == 'Feel')) %>% # Filter data to include only "felt" emotion ratings
  filter(emotion == 'sad' | emotion == 'angry' | emotion == 'happy' | emotion == 'fearful') %>% # Filter data to look only at ratings for four specific emotions
  print()

################################################################################################################################
### Hypothesis 1a + 1b - positive and negative relationships between perceived and felt emotion ratings (depending on music)
################################################################################################################################

####################
# Valence Ratings
####################

### Diagnostics

# Data were pooled across individual pieces in target paper! What, means taken across the three pieces within each category? Hard to tell from paper!
test_data <- df_trim %>%
  group_by(task, condition, participant, anxiety, BIS, BAS, sensation, IRI) %>% # Group data by rating task, music emotion condition, individual participant, and trait characteristic data
  filter(!(condition == 'Anger')) %>% # Filter out angry music (as per original study)
  summarise(M = mean(na, na.rm = T))  # Get mean summaries

# Outliers?
test_data %>%
  group_by(condition, task) %>%
  identify_outliers(M)  # Check for any possible outliers

# Normality?
test_data %>%
  group_by(condition, task) %>%
  shapiro_test(M) # Shapiro test for normality
ggqqplot(test_data, "M", ggtheme = theme_bw()) +
  facet_grid(condition ~ task, labeller = "label_both") # Look at qqplots for each music emotion condition, per rating type (felt/perceived)
hist(test_data$M) # Look at histogram of data to check for normality

# Repeated-measures ANOVA
model <- anova_test(as.data.frame(test_data), dv = M, wid = participant, within = c(task, condition),  effect.size = 'ges', detailed = TRUE)  # Run ANOVA
print(model)  # Show ANOVA results
tab <- get_anova_table(model, correction = 'auto') %>%
  adjust_pvalue(method = 'bonferroni')  # p-value adjustments
knitr::kable(tab, digits = 3) # Print out neat table of results for reporting

# Posthoc comparisons? No difference in valence ratings between joy and sad music, but valence much higher in joy and sad music compared to fear
pairwise_t_test(as.data.frame(test_data), M ~ condition, paired = TRUE, p.adjust.method = "bonferroni")

# Run ANOVAs with each participant trait characteristic as a covariate in turn (as per original study being replicated)
model <- anova_test(as.data.frame(val), dv = M, wid = participant, within = c(task, condition), covariate = c(anxiety),  effect.size = 'ges', detailed = TRUE)
get_anova_table(model, correction = 'auto') %>%
  adjust_pvalue(method = 'bonferroni')

model <- anova_test(as.data.frame(val), dv = M, wid = participant, within = c(task, condition), covariate = c(sensation),  effect.size = 'ges', detailed = TRUE)
get_anova_table(model, correction = 'auto') %>%
  adjust_pvalue(method = 'bonferroni')

model <- anova_test(as.data.frame(val), dv = M, wid = participant, within = c(task, condition), covariate = c(BIS),  effect.size = 'ges', detailed = TRUE)
tab <- get_anova_table(model, correction = 'auto') %>%
  adjust_pvalue(method = 'bonferroni')

model <- anova_test(as.data.frame(val), dv = M, wid = participant, within = c(task, condition), covariate = c(BAS),  effect.size = 'ges', detailed = TRUE)
get_anova_table(model, correction = 'auto') %>%
  adjust_pvalue(method = 'bonferroni')

model <- anova_test(as.data.frame(val), dv = M, wid = participant, within = c(task, condition), covariate = c(IRI),  effect.size = 'ges', detailed = TRUE)
get_anova_table(model, correction = 'auto') %>%
  adjust_pvalue(method = 'bonferroni')

# Plot any significant interactions - there is a marginally significant interaction between sensation-seeking and music emotion condition on valence ratings
viz <- test_data %>%
  group_by(condition, participant) %>%
  summarise(score = mean(M, na.rm = TRUE), SE = sd(M, na.rm = TRUE) / sqrt(length(M)), sensation = mean(sensation, na.rm = TRUE))

# Plot interaction between sensation-seeking and music emotion condition for valence ratings (we see correlations for fear and joy music, not sad music!)
ggplot(viz, aes(x = sensation, y = score, color = condition)) +
  geom_point(stat = 'identity') + # Confirm we are building a scatterplot graph
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) + # Fit line from linear model, to visualise relationships between emotion ratings and sensation-seeking
  scale_color_manual(values = c("#c70000", "#ff4e36","black")) +  # Add manual colours for points and model line
  facet_wrap(.~condition) + # Split into three plots by music emotion condition
  scale_y_continuous(limits = c(-8, 8)) + # Manually confirm scale range for y-axis
  geom_hline(yintercept = 0) +  # For Neatness, x-axis intercepts y-axis at 0
  labs(x = 'Sensation Seeking', y = 'Mean Valence') + # Axis titles
  theme_bw() +  # Simple theme for easy visualisation
  theme(axis.title = element_text(face = 'bold', size = 16), title = element_text(face = 'bold', size = 18), axis.text = element_text(size = 11), strip.text = element_text(face = 'bold', size = 10), axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())  # Manual modifications to various text characteristics of plot

