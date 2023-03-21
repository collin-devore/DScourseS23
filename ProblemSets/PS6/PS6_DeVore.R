library("tidyverse")
library("ggplot2")
library("ggthemes")

# Problem 3
  # Reading in the Data
airsat <- read.csv("Airline_Satisfaction_Data.csv")

  # Checking the Data
head(airsat)
tail(airsat)

  # Changing Necessary Column to All Capitals
airsat$satisfaction <- toupper(airsat$satisfaction)

  # Shortening satisfaction words
airsat$satisfaction <- gsub(" OR ", "/", airsat$satisfaction)

# Problem 4
  # Running Visualizations

    # Visualization 1: Satisfaction Donut Plot
      # With Heavy Inspiration from https://r-graph-gallery.com/128-ring-or-donut-plot.html, https://rfortherestofus.com/2022/09/how-to-make-a-donut-chart-in-ggplot/, and https://www.tutorialspoint.com/how-to-create-a-frequency-column-for-categorical-variable-in-an-r-data-frame#:~:text=Side%20ProgrammingProgramming-,To%20create%20a%20frequency%20column%20for%20categorical%20variable%20in%20an,is%20likely%20to%20be%20repeated.
      # Getting percentages for Donut Plot
airsat <- transform(airsat, Satisfaction_Frequency = ave(seq(nrow(airsat)), satisfaction, FUN = length))

      # Creating a New, Simpler Dataframe
dondat <- data.frame(Satisfaction = c("Satisfied", "Neutral or Dissatisfied"), Count = c(11403, 14573))

      # Creating the Max and Min of the rectangles
dondat$fraction = dondat$Count/sum(dondat$Count)
dondat$ymax = cumsum(dondat$fraction)

dondat$ymin = c(0, head(dondat$ymax, n=-1))

      # Labels
dondat$labelposition <- (dondat$ymax + dondat$ymin)/2
dondat$labels <- paste0(round(dondat$fraction, digits = 4) * 100, "%")

      # Donut Chart
ggplot(dondat, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Satisfaction)) +
  ggtitle("Satisfaction Rate") +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelposition, label = labels), size = 5) +
  scale_fill_brewer(palette = 14) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() 

ggsave("PS6a_DeVore.png", scale = 1, width = 6, height = 5)



    # Visualization 2: Bar Graph
      # Creating Comparable Bar Graph Variables
airsat$barvar <- paste(airsat$satisfaction,"-", airsat$Class)

      # Collapsing Data By Averages to Plot
airsatbar <- aggregate(cbind(Flight.Distance) ~ Class + barvar, airsat, FUN = mean, na.rm = TRUE, na.action = NULL)

      # Creating the Bar Graph
ggplot(airsatbar, aes(x = barvar, y = Flight.Distance)) +
  ggtitle("Flight Distance of Different Consumer Groups") +
  xlab("Group (Satisfaction - Class)") +
  ylab("Flight Distance") +
  geom_col(aes(fill = Class)) +
  scale_x_discrete(guide = guide_axis(angle = 65)) +
  theme_stata() +
  theme(legend.position = "right")

ggsave("PS6b_DeVore.png", scale = 1, width = 6, height = 5)



    # Visualization 3: Violin Plots
ggplot(airsat, aes(x = barvar, y = Flight.Distance)) +
  ggtitle("Flight Distance of Different Consumer Groups") +
  xlab("Group (Satisfaction - Class)") +
  ylab("Flight Distance") +
  geom_violin(aes(fill = Class)) +
  scale_x_discrete(guide = guide_axis(angle = 65)) +
  theme_stata() +
  theme(legend.position = "right")

ggsave("PS6c_DeVore.png", scale = 1, width = 6, height = 5)
