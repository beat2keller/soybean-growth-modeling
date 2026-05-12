
# this files visualizes the two methods to calculating the confidence intervals

library(ggplot2)
library(car)

# load the results
load('model/bootstrap/modelbootstrap_candidates_seed42.RData')
load('model/bootstrap/modelbootstrap_candidates_seed42_bootstrapped_r999.RData')

summary(model)
summary(fit)

# merging the results
intervals_result <- intervals(model)$fixed
confint_result <- confint(fit, type = 'perc')

intervals_df <- as.data.frame(intervals_result)
confint_df <- as.data.frame(confint(fit, type = 'perc'))

combined_df <- data.frame(
  Term = rownames(intervals_df),
  Lower_Interval = intervals_df$lower,
  Upper_Interval = intervals_df$upper,
  Lower_Confint = confint_df[,1],
  Upper_Confint = confint_df[,2]
)

# calculating combined coverage
max_upper = pmax(combined_df$Upper_Interval, combined_df$Upper_Confint) 
min_upper = pmin(combined_df$Upper_Interval, combined_df$Upper_Confint) 
max_lower = pmax(combined_df$Lower_Interval, combined_df$Lower_Confint)
min_lower = pmin(combined_df$Lower_Interval, combined_df$Lower_Confint)
combined_coverage = (min_upper - max_lower)/ (max_upper - min_lower)
combined_df <- combined_df %>% mutate(combined_coverage)

# ordering the df by combined coverage
combined_df <- combined_df[order(-combined_coverage),]

# all estimates together
ggplot(combined_df, aes(y= reorder(Term, combined_coverage))) +
  geom_errorbar(aes(xmin = Lower_Confint, xmax = Upper_Confint, color = "Confidence Interval"), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = Lower_Interval, xmax = Upper_Interval, color = "Interval"), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_text(aes(x = pmax(Upper_Confint, Upper_Interval), label = round(combined_coverage, 2)), hjust = -0.2, color = "black", size=rel(2.5)) +
  labs(title = "Intervals and Confidence Intervals Coverage", y = "Coeeficient", x = "Value", color = "Legend") +
  scale_color_manual(values = c("Confidence Interval" = "red", "Interval" = "blue")) +
  theme(legend.position = "bottom")

## creating two sub dfs for small and large confidence intervals for better visualisation
large_df <- combined_df %>%
  filter(Upper_Confint - Lower_Confint > 5)

small_df <- anti_join(combined_df, large_df) 

# large estimates for CI
ggplot(large_df, aes(y= reorder(Term, combined_coverage))) +
  geom_errorbar(aes(xmin = Lower_Confint, xmax = Upper_Confint, color = "Confidence Interval"), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = Lower_Interval, xmax = Upper_Interval, color = "Interval"), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_text(aes(x = pmax(Upper_Confint, Upper_Interval), label = round(combined_coverage, 2)), hjust = -0.2, color = "black", size=rel(2.5)) +
  labs(title = "Intervals and Confidence Intervals Coverage", y = "Coeeficient", x = "Value", color = "Legend") +
  scale_color_manual(values = c("Confidence Interval" = "red", "Interval" = "blue")) +
  theme(legend.position = "bottom")

# small estimates for CI
ggplot(small_df, aes(y= reorder(Term, combined_coverage))) +
  geom_errorbar(aes(xmin = Lower_Confint, xmax = Upper_Confint, color = "Confidence Interval"), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = Lower_Interval, xmax = Upper_Interval, color = "Interval"), 
                width = 0.3, position = position_dodge(width = 0.5)) +
  geom_text(aes(x = pmax(Upper_Confint, Upper_Interval), label = round(combined_coverage, 2)), hjust = -0.2, color = "black", size=rel(2.5)) +
  labs(title = "Intervals and Confidence Intervals Coverage", y = "Coeeficient", x = "Value", color = "Legend") +
  scale_color_manual(values = c("Confidence Interval" = "red", "Interval" = "blue")) +
  theme(legend.position = "bottom")

