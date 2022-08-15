# Load and plot demographic information

# Set-up ------------------------------------------------------------------

# Install relevant packages (if you haven't already)
# install.packages("here", "dplyr", "ggplot2", "gridExtra")

# Load packages
library(here)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load data
source(here::here("analysis", "myProj_readData.R"))

# Create theme for blank axis in ggplot
blank_theme = theme(
  axis.text.y = element_text(size = 10, colour = "black"),
  axis.text.x = element_text(size = 10, colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.position = "none"
)


# Create plots ------------------------------------------------------------

# Make histogram of age data
agePlot <- ggplot(data = demographics, aes(x = Age)) +
  geom_histogram(bins = 8, fill = "#f5cb42", color = "black") +
  xlab("Age") +
  ylab("Count") +
  blank_theme +
  labs(title = "Age") +
  theme(plot.title = element_text(hjust = 0.5))

# Compile counts of gender data
gend <- demographics %>%
  group_by(Gender) %>% 
  dplyr::summarise(n = n())

gend$Gender[gend$Gender == "F"] <- "Female"
gend$Gender[gend$Gender == "M"] <- "Male"
gend$Gender[gend$Gender == "O"] <- "Other"

# Make pie-chart of gender data
genderPlot <- ggplot(data = gend, aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  blank_theme +
  labs(title = "Gender",
       fill = "Gender") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right",
        axis.ticks = element_blank(),
        panel.border = element_blank())


# Display plots -----------------------------------------------------------

# Create a grid to display both demographic plots
gridExtra::grid.arrange(agePlot, genderPlot, ncol=2)


