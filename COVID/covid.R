library (ggplot2)
library (rvest)
library (dplyr)
library (scales)
library (ggthemes)

get.data <- function(html, css_element) {
  
  data_html <- html_nodes(html, css_element)
  data <- noquote(html_text(data_html))
  data <- gsub(",", "", data)
  
  return(data)
}

url <- "https://www.theguardian.com/us-news/ng-interactive/2020/apr/02/coronavirus-map-of-the-us-latest-cases-state-by-state"
html <- read_html(url)
                    
State <- get.data(html, ".co-tbody td:nth-child(1)")
Confirmed_Cases <- as.numeric(get.data(html, ".co-td-cases"))
Deaths <- as.numeric(get.data(html, ".co-td-deaths"))


all_data <- data.frame(State, Confirmed_Cases, Deaths) %>%
  mutate(Mortality_Rate = Deaths/Confirmed_Cases) %>%
  mutate("2%" = (Deaths/(Confirmed_Cases*1.02))) %>%
  mutate("5%" = (Deaths/(Confirmed_Cases*1.05))) %>%
  mutate("10%" = (Deaths/(Confirmed_Cases*1.1))) %>%
  mutate("20%" = (Deaths/(Confirmed_Cases*1.2))) %>%
  mutate("30%" = (Deaths/(Confirmed_Cases*1.3))) %>%
  mutate("40%" = (Deaths/(Confirmed_Cases*1.4))) %>%
  mutate("50%" = (Deaths/(Confirmed_Cases*1.5))) %>%
  mutate("60%" = (Deaths/(Confirmed_Cases*1.6))) %>%
  mutate("70%" = (Deaths/(Confirmed_Cases*1.7))) %>%
  mutate("80%" = (Deaths/(Confirmed_Cases*1.8))) %>%
  mutate("90%" = (Deaths/(Confirmed_Cases*1.9))) %>%
  mutate("100%" = (Deaths/(Confirmed_Cases*2)))



x <- all_data %>%
  filter(State == "Pennsylvania")

x_trans <- as.data.frame(t(x[,4:16])) %>%
  rename(Mortality_Rate = V1) %>%
  mutate(Confirmed_Cases_Increase = c(0,.02,.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

plot <- ggplot(x_trans, aes(Confirmed_Cases_Increase, Mortality_Rate)) +
  geom_line(linetype = "dashed", color = "lightsteelblue") +
  geom_point(color = "dodgerblue1", size = 3) +
  scale_y_continuous(labels=percent_format()) +
  scale_x_continuous(labels=percent_format()) +
  labs(subtitle="Relative Mortality Rate Based on an Increase in Confirmed Cases", 
       y="Mortality Rate", 
       x="% Increase in Confirmed Cases", 
       title="What is the True Mortality Rate of COVID-19?") +
  theme_hc()

plot
