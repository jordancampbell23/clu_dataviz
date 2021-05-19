library(ggplot2)
library(tidyverse)
library(plotly)
library(apexcharter)


data <- data.frame(
  asset = factor(rep(c("Asset #1", "Asset #2"), each = 200)),
  return = c(rnorm(200, 0.01, 0.01), rnorm(200, -0.02, 0.02))
)

# 1
ggplot(data, aes(x = return)) +
  geom_histogram(bins = 25) 


# 2
ggplot(data, aes(x = return)) +
  geom_histogram(bins = 25, 
                 aes(color = asset, fill = asset)) 


# 3
data %>% 
  ggplot(aes(x = return, colour = asset, fill = asset)) +
  stat_density(alpha = 0.5) +
  scale_fill_manual(values=c("#ff6633", "#2879cb")) +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  xlab("Daily Returns") +
  ylab("Frequency") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_classic()




# 4 ggplot + Plotly

creatives <- read.csv(file="https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggplotly/Median_Earnings_of_Creative_Sector_Occupations__CLL.B.1.csv",
                      header=TRUE, sep=",", stringsAsFactors = FALSE)



creatives <- creatives %>% 
  rename(Median_2016 = X2016.Median.hourly.earnings, Median_2017 = X2017.Median.hourly.earnings) %>% 
  rowwise() %>% 
  mutate(Percent_Improvement = round((Median_2017-Median_2016)/Median_2016*100,2)) 


#Note - The chart is not interactive in .md format.  Please view on littlemissdata.com/blog/interactiveplots
scatterPlot <- creatives %>% 
  ggplot(aes(x = Median_2017, y = Percent_Improvement, 
             text = paste(
               "Occupation: ", Occupation, "\n",
               "2017: ", Median_2017, "\n",
               "2016: ", Median_2016, "\n",
               "% Improvement Year over Year: ", Percent_Improvement, "\n",
               sep = ""
             ))) + 
  geom_point(alpha=0.7, colour = "#51A0D5") + 
  labs(x = "Median Occupation Hourly Wage in 2017", 
       y = "% Improvement Year over Year (2016 to 2017)",
       title = "Austin Creative Occupations Median Hourly Wage") +
  geom_hline(yintercept=0, linetype="dashed", color = "#2C528C", size=0.5) +
  theme_classic()

ggplotly(scatterPlot, tooltip = "text")



# 5 Plotly

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec,
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Miles/(US) gallon',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig


# 6 apexcharter
df <- readr::read_csv("BTC-USD.csv")


df <- df %>%
  mutate(`Adj Close` = as.numeric(`Adj Close`))


df$Date <- as.Date(df$Date, format =  "%m/%d/%Y")

data("economics")


apex(data = df, type = "area", mapping = aes(x = Date, y = `Adj Close`)) %>%
  ax_title(text = "Price of Bitcoin") %>% 
  ax_subtitle(text = "May 9 2020 to May 9 2021")



