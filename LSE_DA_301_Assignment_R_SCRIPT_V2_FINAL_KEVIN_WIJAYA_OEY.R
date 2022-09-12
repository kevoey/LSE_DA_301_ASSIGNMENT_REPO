# Week 4 Assignment 

# Import the necessary libraries to view csv data.
library("tidyverse")

# Check the current working directory 
getwd()

# Set the current working Directory
setwd(dir="C:/Users/user/Desktop/LSE Data Analytics - Repository/Course 3 data/LSE_DA301_assignment_files")

# Recheck the current working directory.
getwd()

# Load the turtle_sales.csv data.
turtle_sales <- read.csv("turtle_sales.csv")

# View the turtle_sales DataFrame.
View(turtle_sales)

# Subset the turtle_sales DataFrame to retain relevant
# columns. 
turtle_sales_clean <- select(turtle_sales,
                             -Ranking,
                             -Year,
                             -Genre,
                             -Publisher)

# View the subsetted DataFrame.
View(turtle_sales_clean)

# Prepare a summary of the cleaned data.
summary(turtle_sales_clean)

# Assign the "Product" column as a factor
turtle_sales_clean <- mutate(turtle_sales_clean,
                             Product = as.factor(Product))

# Check that the Product numbers have been changed to a factor.
summary(turtle_sales_clean)

# Create boxplots, histograms, and scatterplots to 
# inform business decisions.

# Make a boxplot for North American Sales segmented by Platform.
qplot(NA_Sales, Platform,data=turtle_sales_clean, geom="boxplot")

# Make a boxplot for European Sales segmented by Platform.
qplot(EU_Sales, Platform,data=turtle_sales_clean, geom="boxplot")

# Make a boxplot for Global Sales segmented by Platform.
qplot(Global_Sales, Platform,data=turtle_sales_clean, geom="boxplot")

# Segmentation by Product was not possible as there were too
# many categories.

# Create scatterplots to show the relationship between Product and North
# American Sales.
qplot(Product, NA_Sales, colour = Platform, data=turtle_sales_clean)

# Create scatterplots to show the relationship between Product and European Sales.
qplot(Product,EU_Sales, colour = Platform, data=turtle_sales_clean)

# Create scatterplots to show the relationship between Product and Global Sales.
qplot(Product,Global_Sales, colour = Platform, data=turtle_sales_clean)

# Create scatterplots to show the relationship between North American and
# European Sales.
qplot(NA_Sales,EU_Sales,data=turtle_sales_clean, colour=Platform)

# Create histograms showing the distribution of each variable:

# Plaform
qplot(Platform,data=turtle_sales_clean)

# Product
qplot(Product,data=turtle_sales_clean)

# North American Sales
qplot(NA_Sales,data=turtle_sales_clean)

# European Sales
qplot(EU_Sales,data=turtle_sales_clean)

# Global Sales
qplot(Global_Sales,data=turtle_sales_clean)

# Groupby Sales and product id
group_by(Product,data=turtle_sales_clean) %>% 
  summarise(total_sales = sum(NA_Sales,EU_Sales,Global_Sales))

# Observations
# Consistent outliers in Wii games

# Change all the values under the Platform column as characters.
turtle_sales_clean$Platform <- as.character(turtle_sales_clean$Platform)

# Check that the data type has been changed.
summary(turtle_sales_clean$Platform)

# Categorise Platforms via Product family using sapply and switch.
turtle_sales_clean$Console_Family <- sapply(turtle_sales_clean$Platform,
                                            switch,
                                            '2600'="Atari",
                                            '3DS'="DS Console",
                                            DS="DS Console",
                                            GB="Gameboy",
                                            GBA= "Gameboy",
                                            GC= "GameCube",
                                            GEN= "Sega Genesis",
                                            N64= "Nintendo Game Console",
                                            NES= "Nintendo Game Console",
                                            PC= "PC",
                                            PS= "Playstation",
                                            PS2= "Playstation",
                                            PS3= "Playstation",
                                            PS4="Playstation",
                                            PSP="Playstation",
                                            PSV="Playstation",
                                            SNES="Nintendo Game Console",
                                            Wii="Wii",
                                            WiiU="Wii",
                                            X360="XBOX",
                                            XB="XBOX",
                                            XOne="XBOX")

# Categorise the Products via type.
turtle_sales_clean$Console_Type <- sapply(turtle_sales_clean$Platform,
                                            switch,
                                            '2600'="Home",
                                            '3DS'="Handheld",
                                            DS="Handheld",
                                            GB="Handheld",
                                            GBA="Handheld",
                                            GC= "Home",
                                            GEN= "Home",
                                            N64= "Home",
                                            NES= "Home",
                                            PC= "Home",
                                            PS= "Home",
                                            PS2= "Home",
                                            PS3= "Home",
                                            PS4="Home",
                                            PSP="Handheld",
                                            PSV= "Handheld",
                                            SNES="Dedicate",
                                            Wii="Home",
                                            WiiU="Home",
                                            X360="Home",
                                            XB="Home",
                                            XOne="Home")

# NB: Console FAmily and Console Types were sourced from Wikipedia

# View the newly updated DataFrame.
View(turtle_sales_clean)

# Make a boxplot
qplot(NA_Sales, Console_Family,
      data=turtle_sales_clean, 
      geom="boxplot", 
      colour = Console_Type)

# Most of the revenue comes from xbox, wii, playstation (home), pc, 
# and nintendo consoles; second to that being gameboy and DS.

# Create a boxplot by Console Type For EU Sales.
qplot(EU_Sales, Family,data=turtle_sales_clean, geom="boxplot", colour = Console_Type)

# Create a boxplot by Console Type For Global Sales.
qplot(Global_Sales, Family,data=turtle_sales_clean geom="boxplot", colour= Console_Type)

# similar trends exist across different sales regions. Perhaps could be segmented
# further using a stacked barchart
# to drill down to individual products

# Week 5 Assignment

# Convert the "Product" column in the DataFrame to
# a factor.
turtle_sales_clean <- mutate(turtle_sales_clean,
                             Product = as.factor(Product))

# Find the min,max and mean sales for North America.
min_NA_sales <- min(turtle_sales_clean$NA_Sales)
max_NA_sales <- max(turtle_sales_clean$NA_Sales)
mean_NA_sales <- mean(turtle_sales_clean$NA_Sales)

# Find the min,max and mean sales for Europe.
min_EU_sales <- min(turtle_sales_clean$EU_Sales)
max_EU_sales<- max(turtle_sales_clean$EU_Sales)
mean_EU_sales<- mean(turtle_sales_clean$EU_Sales)

# Find the min,max and mean sales for Global Sales.
min_Global_sales<- min(turtle_sales_clean$Global_Sales)
max_Global_sales <- max(turtle_sales_clean$Global_Sales)
mean_Global_sales <- mean(turtle_sales_clean$Global_Sales)

# Compile the descriptive statistics into a single DataFrame.
sales_summary <- t(data.frame(min_NA_sales,
                            max_NA_sales,
                            mean_NA_sales,
                            min_EU_sales,
                            max_EU_sales,
                            mean_EU_sales,
                            min_Global_sales,
                            max_Global_sales,
                            mean_Global_sales))

# Use the groupby function to find the total regional sales by product.
sales_by_pdt <- data.frame(group_by(turtle_sales_clean) %>% 
                             group_by(Product) %>% 
                            summarise(total_NA_sales = sum(NA_Sales),
                                   total_EU_sales = sum(EU_Sales),
                                   total_Global_sales=sum(Global_Sales)))

# Add Console_Family and Console_Type to sales_by_pdt.
sales_by_pdt <- left_join(sales_by_pdt,turtle_sales_clean)

# Remove redundant columns.
sales_by_pdt <- select(sales_by_pdt,
                       -Platform,
                       -NA_Sales,
                       -EU_Sales,
                       -Global_Sales)

# View the resulting DataFrame.
View(sales_by_pdt)

# Make a Scatterplot showing total North American Sales by
# product, segmented by Console type.
ggplotly(sales_by_pdt,
       aes(x=total_NA_sales,
           y=Product,
           col=Console_Type))+
  geom_point()+
  coord_flip()

# Change the total sales by product to "Long form"
# so that we can distinguish by sale type.
pdt_x_sale <- data.frame(pivot_longer(sales_by_pdt,
                                      cols=c(total_NA_sales,
                                             total_EU_sales,
                                             total_Global_sales),
                                      names_to = "Sale Type"))

# Import the ggplot library to prevent errors.
library(ggplot2)

# Use dev.off incase erros pop up when running the following plots.
dev.off()

# Create a faceted scatterplot showing the 
# breakdown of regional sales by
# Console_Family.
ggplot(data=pdt_x_sale,
       aes(x=Product,
           y=value,
           col=Sale.Type))+
  geom_point()+
  facet_wrap(~Console_Family)+
  scale_y_continuous(breaks=seq(0,70,10))+
  labs(title="Breakdown of Regional Sales by Console Family",
       subtitle="Source: turtle_sales.csv",
       x="Product Id",
       y="Revenue in millions GBP")+
  theme_bw()

# Create a faceted boxplot showing the 
# breakdown of regional sales by
# Console_Family.
ggplot(data=pdt_x_sale,
       aes(x=Console_Family,
           y=value,
           col=Console_Type))+
  geom_boxplot()+
  facet_wrap(~Sale.Type)+
  scale_y_continuous(breaks=seq(0,70,10))+
  labs(title="Boxplot of Sales by Region segmented Console Family & Console Type",
       subtitle="Source: turtle_sales.csv",
       x="Console Family",
       y="Revenue in millions GBP")+
  theme_bw()+
  coord_flip()

# Create a faceted histogram showing the 
# breakdown of regional sales by
# Console_Family.

ggplot(data=pdt_x_sale,
       aes(x=value,
           fill=Sale.Type))+
  geom_histogram()+
  facet_wrap(~Console_Family)+
  scale_y_continuous(breaks=seq(0,70,10))+
  labs(title="Breakdown of Regional Sales by Console Family",
       subtitle="Source: turtle_sales.csv",
       x="Product Id",
       y="Revenue in millions GBP")+
  theme_bw()

# Insight: 
# There's an inverse logarithmic relationship between
# product_id and sales. The lower the product_id,
# the higher the number of sales regardless of region.
# This trend is present across all Console Families.

# The Boxplots show that home devices and the previosuly mentioned consoles
# PC, Playstation etc have higher medians and more frequent outliers than
# other consoles. This means they indeed sell better than other consoles.

# Determine the normality of each sale type using 
# qqplots and a line of best fit.

# Determine the normality of North_American Sales.
qqnorm(turtle_sales_clean$NA_Sales)
qqline(turtle_sales_clean$NA_Sales)

# Determine the normality of European Sales.
qqnorm(turtle_sales_clean$EU_Sales)
qqline(turtle_sales_clean$EU_Sales)

# Determine the normality of Global Sales.
qqnorm(turtle_sales_clean$Global_Sales)
qqline(turtle_sales_clean$Global_Sales)

# All the graphs show most of the points roughly 
# following a straight line, which
# confirms normality.
# The points circle around the line of best fit
# but lie away from the line
# as theoreticla quantiles increase
# which does not confirm normality.

# Apply the shapiro.wilk test across all sales data
# to determine normality.
shapiro.test(turtle_sales_clean$NA_Sales)
shapiro.test(turtle_sales_clean$EU_Sales)
shapiro.test(turtle_sales_clean$Global_Sales)

# All the p-values from the Shapiro-Wilk test
# are less than 0.05. Therefore, we reject the
# null hypotheses that the data for all 3 sales columns
# are normally distributed. 

# The low p-value and not-so-ideal
# qqnorm plots show that the data for all sales columns
# are likely not normally distributed.

# Import the moments library to enable kurtosis and skewness.
library(moments)

# Determine the kurtosis values for North American Sales.
kurtosis(turtle_sales_clean$NA_Sales)

# Determine the kurtosis values for European Sales.
kurtosis(turtle_sales_clean$EU_Sales)

# Determine the kurtosis values for Global Sales.
kurtosis(turtle_sales_clean$Global_Sales)

# Determine the Skewness values for North American Sales.
skewness(turtle_sales_clean$NA_Sales)

# Determine the Skewness values for European Sales.
skewness(turtle_sales_clean$EU_Sales)

# Determine the Skewness values for Global Sales.
skewness(turtle_sales_clean$Global_Sales)

# The skewness levels for all three sales data
# are larger than 1, indicating positive skewness and
# that the distribution is highly right-skewed, highly biased
# to higher values and not normal.

# The kurtosis levels for all sales regions are higher than 3
# indicating the data is leptokurtic and will abundantly
# produce more extreme outliers than the normal
# distribution. 

# Therefore the data is not reliable or normal.

# Subset the turtle_sales_clean data to contain 
# NA_Sales, EU_Sales and Global_Sales only.
sales_subset_df <- select(turtle_sales_clean,
                          NA_Sales,
                          EU_Sales,
                          Global_Sales)

# Determine the correlation between all three sales types.
cor(sales_subset_df)

# High positive correlation between NA_SAles and EU_Sales.
# Moderately positive correlatoin between NA_Sales and EU_Sales.
# INdicating that NA_Sales and EU_Sales contribute to global sales
# but only moderately influence one another.

# Week 6 Assignment

# Determine the correlation between sales columns in the cleaned DataFrame.
cor(turtle_sales_clean_2)

# Create plots to show all the possible relationships between
# Sales from North America, Europe, and Global Sales.
plot(turtle_sales_clean_2$NA_Sales,turtle_sales_clean_2$EU_Sales,)
plot(turtle_sales_clean_2$NA_Sales,turtle_sales_clean_2$Global_Sales,)
plot(turtle_sales_clean_2$EU_Sales,turtle_sales_clean_2$Global_Sales,)

# Create linear regression models for all possible combinations
# of sales.
model1 <- lm(NA_Sales~EU_Sales,data=turtle_sales_clean_2) 
model2 <- lm(NA_Sales~Global_Sales,data=turtle_sales_clean_2)
model3 <- lm(EU_Sales~Global_Sales,data=turtle_sales_clean_2)

# Filter the turtle_sales DataFrame to only include numeric columns.
turtle_sales_clean_2_num <- keep(is.numeric(data=turtle_sales_clean_2))

# Determine the correlation between sales columns.
cor(turtle_sales_clean_2_num)

# Create multilinear regression models based on the DataFrame filtered
# by numeric columns for all possible combinations.
model4 <- lm(total_Global_sales~total_NA_sales+total_EU_sales,
             data=sales_by_pdt) 

# Compare to predicted values

# Create a dataframe with provided NA and EU Sales data.
global_sales_forecast <- data.frame(total_NA_sales=c(34.02,3.93,2.73,2.26,22.08),
                                    total_EU_sales=c(23.80,1.56,0.65,0.97,0.52))

# Predict the Global_Sales with the proivded data.
global_sales_forecast$predicted_global_Sales <- 
  predict(model4,
        newdata=global_sales_forecast)

# View the predicted values.
View(global_sales_forecast)

# Extract the actual values from the csv 
# and import manually to R.
global_sales_forecast <- data.frame(total_NA_sales=c(34.02,3.93,2.73,2.26,22.08),
                                    total_EU_sales=c(23.80,1.56,0.65,0.97,0.52))

# Join the dataframe with predicted values 
# to original dataset.
global_sales_forecast$Actual_Global_sales <- c(67.85,6.04,4.32,3.53,23.21)

# Find percentage accuracy of predictions.
# Find the percentage accuracy of predicted vs actual values.
global_sales_forecast$Prediction_accuracy <- (global_sales_forecast$predicted_global_Sales-global_sales_forecast$Actual_Global_sales)*100/global_sales_forecast$Actual_Global_sales

# Notes and observations.
# Multilinear regression still needs fine tuning as it
# overestimates predictions of global sales.
# Average percentage error 15.1%

# Other areas to explore
# Use ggplotly to make interactive charts 
# Explore homoscesdascity, breuschpagan tests, other R-Squared tests
