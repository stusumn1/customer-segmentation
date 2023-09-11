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
summary(customer_data$Age)

  # histogram
ggplot(data = customer_data, aes(Age)) +
  geom_histogram(binwidth = 10, aes(fill = Gender), color = "black") +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(n.breaks = 10) +
  theme(legend.position = "top", legend.title = element_blank()) +
  labs(title = "Customer Age Distribution",
       x = "Age",
       y = "Count")

  # simple box plot
ggplot(data = customer_data, aes(Age)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(labels = NULL) +
  labs(title = "Customer Age Distribution") +
  ggthemes::theme_fivethirtyeight()
  
# Visualizing Income Distribution
summary(customer_data$`Annual Income (k$)`)

  # histogram displaying income bracket
ggplot(data = customer_data, aes(`Annual Income (k$)`)) +
  geom_histogram(binwidth = 13, color = "black", fill = "lightblue") +
  stat_bin(binwidth = 13, geom = "text", color = "black", size = 3, aes(label = ..count..),
           position = position_stack(vjust = .5)) +
  scale_x_continuous(n.breaks = 10) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Annual Income Distribution",
       xlab = "Income (in k$)") +
  ggthemes::theme_clean()

# Visualizing Spending Score Distribution
  # simple boxplot
ggplot(data = customer_data, aes(`Spending Score (1-100)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Spending Score Distribution ")

  # histogram displaying Spending Score bins
ggplot(data = customer_data, aes(`Spending Score (1-100)`)) +
  geom_histogram(binwidth = 13, color = "black", fill = "lightblue") +
  stat_bin(binwidth = 13, geom = "text", color = "black", size = 3, aes(label = ..count..),
           position = position_stack(vjust = .5)) +
  scale_x_continuous(n.breaks = 10) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(title = "Spending Score Distribution",
       xlab = "Spending Score") +
  ggthemes::theme_clean()

