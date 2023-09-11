# Load packages
library(tidyverse)

# Read in customer data
customer_data <- read_csv("Mall_Customers.csv")

# Exploration
str(customer_data)

names(customer_data)
head(customer_data)

summary(customer_data$Age)
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score..1.100.)

# Visualize Gender Distribution
  # simple bar chart
ggplot(data = customer_data, aes(Gender)) +
  geom_bar(aes(fill = Gender)) +
  theme_bw() +
  labs(title = "Customer Gender Comparison",
       x = "Gender",
       y = "Count")

  # pie chart using gender proportions
gen <- table(customer_data$Gender)
gender_pct <- round(gen/sum(gen) * 100)

tbl <- tibble(gender_pct, gender = c("Female", "Male")) %>% 
  arrange(desc(gender)) %>% 
  mutate(prop = gender_pct) %>% 
  mutate(ypos = cumsum(prop)- 0.5*prop )

labs <- paste(tbl$gender, " (", tbl$gender_pct, "%)", sep="")

ggplot(data = tbl, aes(x = "", y = gender_pct, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "none") +
  geom_text(aes(y = ypos, label = labs),
            color = "black",
            size = 5) 

# Visualizing Age Distributions
