library(tidyverse)
library(tidymodels)
library(lubridate)
library(infer)
library(dplyr)

f <- file.choose()
FM_mortgages <- read_csv(f)
colnames <- c(colnames(FM_mortgages))

nrow(FM_mortgages)
colnames
glimpse(FM_mortgages)
FM_mortgages %>% arrange(CREDIT_SCORE) %>% select(SELLER_NAME)
FM_mortgages %>% arrange(DELINQUENT) %>% select(SELLER_NAME)

FM_mortgages %>% ggplot(aes(x = CREDIT_SCORE)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Credit Score", y = NULL)

nrow(FM_mortgages)
mean(FM_mortgages$CREDIT_SCORE, na.rm = TRUE)

length(unique(FM_mortgages$SELLER_NAME))
FM_mortgages$DELINQUENT

FM_mortgages %>% 
  filter(DELINQUENT == "TRUE") %>% count()

#relationship between UPB and credit score

FM_mortgages %>% ggplot(aes(x= CREDIT_SCORE, y = ORIGINAL_UPB)) + 
  geom_point() + 
  labs(x = "Credit Score", y = "Unpaid Balance")

FM_mortgages %>% ggplot(aes(x = DELINQUENT)) +
  geom_bar() +
  labs(x = "Num Deliquent")

length(unique(FM_mortgages$PRODUCT_TYPE))

FM_mortgages %>%
  filter(FIRST_PAYMENT_DATE < 200000)




mean(FM_mortgages$ORIGINAL_UPB, na.rm = TRUE)

FM_mortgages %>% ggplot(aes(x= MATURITY_DATE, y = CREDIT_SCORE)) + 
  geom_point() + 
  labs(x = "Maturity", y = "Credit Score")


FM_mortgages %>% ggplot(aes(x = ORIGINAL_UPB)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Unpaid Balance", y = NULL)

#Linear regression 
score_defeault_fit <- linear_reg() %>% set_engine("lm") %>% 
  fit(CREDIT_SCORE ~ DELINQUENT, data = FM_mortgages)

glimpse(tidy(score_defeault_fit))

glance(score_defeault_fit)$r.squared
glance(score_defeault_fit)

ggplot(FM_mortgages, aes(x = DELINQUENT, y = CREDIT_SCORE)) +
  geom_boxplot() +
  labs(x = "Delinquent", y = "Credit Score") +
  theme_minimal()



####Bootstrapping #####



fm_sample <- FM_mortgages %>% 
  sample_n(3000, replace = FALSE)



boot_creditScore_means <-  fm_sample %>% 
  specify(response = CREDIT_SCORE) %>% 
  generate(reps = 1500, type = "bootstrap") %>%
  calculate(stat = "mean")
  boot_creditScore_means$stat
  
  
  
  
  
  delinquent <- fm_sample %>% filter(DELINQUENT == "TRUE")
  non_delinquent <- fm_sample %>% filter(DELINQUENT == "FALSE")
  glimpse(delinquent)
  glimpse(non_delinquent)
  
  mean(delinquent$CREDIT_SCORE, na.rm = TRUE)
  mean(non_delinquent$CREDIT_SCORE, na.rm = TRUE)
  
  
  
  # Bootstrap the mean credit score for delinquent mortgages
  boot_delinquent <- delinquent %>% 
    specify(response = CREDIT_SCORE) %>% 
    generate(reps = 1500, type = "bootstrap") %>%
    calculate(stat = "mean")
  
  mean(boot_delinquent$replicate)
  
  # Bootstrap the mean credit score for non-delinquent mortgages
  boot_non_delinquent <- non_delinquent %>% 
    specify(response = CREDIT_SCORE) %>% 
    generate(reps = 1500, type = "bootstrap") %>%
    calculate(stat = "mean")  


mean(boot_non_delinquent$replicate)






# Calculate test statistic
#mean_diff <- mean(boot_delinquent$stat) - mean(boot_non_delinquent$stat)

# Generate null distribution
#null_distribution <- replicate(10000, {
 # delinquent_boot <- sample(boot_delinquent$stat, replace = TRUE)
  #non_delinquent_boot <- sample(boot_non_delinquent$stat, replace = TRUE)
  #mean(delinquent_boot) - mean(non_delinquent_boot)
#})
#null_distribution

# Calculate p-value
#p_value <- 2 * mean(abs(null_distribution) >= abs(mean_diff))

# Output p-value
#p_value



glimpse(replicate)



# Separate the data into delinquent and non-delinquent mortgages
mean(fm_sample$CREDIT_SCORE, na.rm = TRUE)

boot_credScore_shifted <- fm_sample %>%
  filter(!is.na(CREDIT_SCORE)) %>%
  specify(response = CREDIT_SCORE) %>%
  hypothesise(null = "point", mu = 698) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

ggplot(data=boot_credScore_shifted, aes(x = stat)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 711.6571, color = "green") +
  geom_vline(xintercept = 698 + (698 - 711.6571), color = "green")
  
boot_credScore_shifted %>%
  filter( stat <= 711.6571) %>%
  summarise(p_value = 2 * (n() / nrow(boot_credScore_shifted)))

























