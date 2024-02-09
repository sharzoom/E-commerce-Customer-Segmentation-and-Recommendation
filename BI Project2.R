df <- read.csv("C:/Users/Admin/Desktop/SUBS/BI/Copy of Online Retail.csv")
df
summary(df)
str(df)
print(unique(df$Description))
missing_values <- colMeans(is.na(df))
missing_values

df <- na.omit(df)
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

revenue <- df$Quantity * df$UnitPrice

plot_clean1 <- retail %>% 
  group_by(Country) %>% 
  dplyr::summarise(n = n()) 


highchart() %>% 
  hc_chart(type ="column",
           options3d = list(enabled = TRUE, alpha = 15, beta = 15)) %>%
  hc_xAxis(categories = plot_clean1$Country) %>% 
  hc_add_series(data = plot_clean1$n, name = "Total Invoices") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(
    text="Total Invoices - Transaction per Country"
  ) %>%
  hc_chart(
    borderColor = '#EBBA95',
    borderRadius = 10,
    borderWidth = 1,
    backgroundColor = list(
      linearGradient = c(0, 0, 500, 500), stops = list(
        list(0, 'rgb(255, 255, 255)'),
        list(1, 'rgb(200, 200, 255)')
      )))
  
library(ggplot2)
library(tidyr)
data <- gather(df, key = "Variable", value = "Value")
ggplot(data, aes(x = Value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  geom_vline(aes(xintercept=median(Value)),
             color="brown2", linetype="dashed", linewidth=0.5) +
  geom_text(
    aes(x = median(Value), label = "median", y = 0.05),
    color = "brown2", vjust = -1, hjust = -0.2
  ) +
  labs(title = "Plots for all variables in the dataset", x = "variable", y = "Density")
warnings()
warnings()

ggplot(df0, aes_string(x = 'Ticket.Priority')) + 
  geom_bar(size = 1, colour = 'black', fill = '#adf7b6') +
  expand_limits(y = max(table(df0$Ticket.Priority)) * 1.1) +
  geom_text(stat = 'count', aes(label=..count..), vjust = -0.3) +
  labs(title = 'Ticket Priority Levels', 
       subtitle = 'Number of tickets raised at each priority level.',
       caption = 'All ticket priority levels seem evenly distributed.',
       x = 'Level', y = 'Count') +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        plot.subtitle = element_text(size = 10, face = 'plain'),
        plot.caption = element_text(size = 10, face = 'italic', hjust = 0),
        axis.text.x = element_text(size = 10, face = 'bold'), 
        axis.title.x = element_text(size = 13, face = 'bold'), 
        axis.text.y = element_text(size = 10, face = 'bold'), 
        axis.title.y = element_text(size = 13, face = 'bold'),
        plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"))