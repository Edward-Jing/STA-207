## ----fig.height=5, fig.width=5, include=FALSE-------------------------------------------------------------------------
library(multcompView)
library(naniar)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(nortest)
library(stats)
library(AER)
library(visdat)
library(plotly)
library(data.table)
library(devtools)
library(ggsci)


## ----echo=FALSE, warning=FALSE, message=FALSE, fig.width=10, fig.height=4, fig.align="center"-------------------------

data("STAR")
# Load data with some variables 
columns_long <- c("gender", "birth", "stark","star1", "star2", "star3", "mathk","math1", "math2", "math3", "schoolk","school1", "school2", "school3", "degree1", "degree2", "degree3", "ladder1", "ladder2", "ladder3", "experience1", "experience2", "experience3", "schoolid1", "schoolid2", "schoolid3", "system1", "system2", "system3")
data_long <- STAR[, columns_long]

# Drop Missing Values
nona_long=na.omit(data_long)

# Count how many student enrolls in each combination of class types for 3 years
alluvialdata <- nona_long %>% group_by(stark,star1, star2, star3) %>%summarise(Freq = n())  

# Construct new variables
alluvial_data <- as.data.frame(alluvialdata)
alluvial_data <- alluvial_data %>%
  mutate(
    stark=paste0(stark,'_k'),
    star1=paste0(star1,'_1'),
    star2=paste0(star2,'_2'),
    star3=paste0(star3,'_3')
  )


alluvial_data$color = c(rep('lightblue',19), rep('salmon',14), rep('lightgray',20))

# Data classification and rename columns

alluvial_data_k <- alluvial_data[,c(1,2,5,6)]
alluvial_data_k <- alluvial_data_k %>% rename(source = stark, target = star1)


alluvial_data_1 <- alluvial_data[,c(2,3,5,6)]
alluvial_data_1 <- alluvial_data_1 %>% rename(source = star1, target = star2)

alluvial_data_2 <- alluvial_data[,3:6]
alluvial_data_2 <- alluvial_data_2 %>% rename(source = star2, target = star3)

# Combine the data into one dataframe
sankeydata <- rbind(alluvial_data_k,alluvial_data_1, alluvial_data_2)
sankeydata <- data.table(sankeydata)
combined_sank = rbind(sankeydata[1:53,lapply(.SD,sum), by=list(source, target, color)], sankeydata[54:106,lapply(.SD,sum), by=list(source, target, color)],sankeydata[107:159,lapply(.SD,sum), by=list(source, target, color)])

# Create Links
links <- combined_sank
# Convert links as character
links$source <- as.character(links$source)
links$target<- as.character(links$target)

# Create nodes based on links
nodes <- data.frame(name = unique(c(links$source, links$target)))

# More clean-up
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

library(plotly)

# Assuming these are the JCO colors you want to use
jco_colors <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

# Modified your original code to use the 'jco_colors' for the 'color' attribute in 'node'
fig <- plot_ly(type = "sankey",
               orientation = "h",
               node = list(
                 label =  c("regularK", "smallK", "reg+aidK","regular1", "small1", "reg+aid1","regular2", "small2", "reg+aid2","regular3", "small3", "reg+aid3"),
                 color = rep(jco_colors, each = 3),
                 pad = 15,
                 thickness = 20,
                 line = list(color = "black", width = 0.5)),
               link = list(source = links$source, # Ensure 'links' is defined elsewhere in your script
                           target = links$target,
                           value = links$Freq,
                           color = links$color, # Ensure 'links$color' matches desired color logic
                           alpha = 0.7)) %>%
  layout(title = "Continuity of Program (Start from Kindergaten)", font = list(size = 10))

# Display plot
fig



## ----echo=FALSE, warning=FALSE, message=FALSE, fig.width=10, fig.height=4, fig.align="center"-------------------------

# Load data with some variables we are interested in 
columns_long <- c("gender", "birth", "star1", "star2", "star3", "math1", "math2", "math3", "school1", "school2", "school3", "degree1", "degree2", "degree3", "ladder1", "ladder2", "ladder3", "experience1", "experience2", "experience3", "schoolid1", "schoolid2", "schoolid3", "system1", "system2", "system3")
data_long <- STAR[, columns_long]

# Drop Missing Values
nona_long=na.omit(data_long)

# Count how many student enrolls in each combination of class types for 3 years
alluvialdata <- nona_long %>% group_by(star1, star2, star3) %>%summarise(Freq = n())  

# Construct new variables
alluvial_data <- as.data.frame(alluvialdata)
alluvial_data <- alluvial_data %>%
  mutate(
    star1=paste0(star1,'_1'),
    star2=paste0(star2,'_2'),
    star3=paste0(star3,'_3')
  )

# Set color for streams (links) in the alluvial diagram  
alluvial_data$color = c(rep('lightblue',9), rep('salmon',7), rep('lightgray',7))

# Data classification and rename columns
alluvial_data_1 <- alluvial_data[,c(1,2,4,5)]
alluvial_data_1 <- alluvial_data_1 %>% rename(source = star1, target = star2)
alluvial_data_2 <- alluvial_data[,2:5]
alluvial_data_2 <- alluvial_data_2 %>% rename(source = star2, target = star3)

# Combine the data into one dataframe
sankeydata <- rbind(alluvial_data_1, alluvial_data_2)
sankeydata <- data.table(sankeydata)
combined_sank = rbind(sankeydata[1:23,lapply(.SD,sum), by=list(source, target, color)], sankeydata[24:46,])

# Create Links
links <- combined_sank

# Convert links as character
links$source <- as.character(links$source)
links$target<- as.character(links$target)

# Create nodes based on links
nodes <- data.frame(name = unique(c(links$source, links$target)))

# More clean-up
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Create web-based interactive charting
fig <- plot_ly(type = "sankey",
               orientation = "h",
               node = list(
                 label = c("regular1", "small1", "reg+aid1","regular2", "small2", "reg+aid2","regular3", "small3", "reg+aid3"),
                 color = rep(c("#EFC000FF", "#868686FF", "#CD534CFF"),each = 3),
                 pad = 15,
                 thickness = 20,
                 line = list(color = "black", width = 0.5)),
               link = list(source = links$source,
                           target = links$target,
                           value = links$Freq,
                           color = links$color,alpha = 0.7))

# Format of the plot
fig <- fig %>% layout(title = "Continuity of Program(Start from First Grade)", font = list(size = 10))

# Display plot
fig


## ----echo=FALSE, fig.height=5, fig.width=8, fig.align='center'--------------------------------------------------------

rm(list = ls())

#read file
star <- read.table("STAR_Students.tab",sep="\t", header=TRUE)

# Keep the columns only relevant to the first grade students
columns <- c("gender","g1classtype","g1schid","g1surban","g1tchid","g1tgen","g1trace","g1thighdegree","g1tcareer","g1tyears","g1tmathss","race")
data <- star[,columns]

# Change the colnames
colnames(data) <- c("Gender", "Class Type in Grade 1", "School ID","School Urbanicity","Teacher ID", "Teacher Gender", "Teacher Race", "Teacher Highest Degree", "Teacher Career Ladder","Teaching Experience", "Math Scale Score in 1st Grade","Race")




## ----eval=FALSE, include=FALSE----------------------------------------------------------------------------------------
## # miss values
## missing_g1mathss <- data[is.na(data$g1mathss), ]
## 
## # race, g1surban, g1classtype
## calculate_percentage <- function(data, column_name) {
##   table <- table(data[[column_name]])
##   percentage <- prop.table(table) * 100
##   return(data.frame(Value = names(percentage), Percentage = percentage))
## }
## 
## # race
## race_distribution <- calculate_percentage(missing_g1mathss, "race")
## print("Race Distribution:")
## print(race_distribution)
## 
## # g1surban
## g1surban_distribution <- calculate_percentage(missing_g1mathss, "g1surban")
## print("G1surban Distribution:")
## print(g1surban_distribution)
## 
## # g1classtype
## g1classtype_distribution <- calculate_percentage(missing_g1mathss, "g1classtype")
## print("G1classtype Distribution:")
## print(g1classtype_distribution)
## 
## 


## ----fig.height=5, fig.width=12, message=FALSE, warning=FALSE, , fig.align='center', ,echo=FALSE----------------------
rm(list = ls())

#read file
star <- read.table("STAR_Students.tab",sep="\t", header=TRUE)

# Boxplot
all_g1 <- c("g1classtype",'g1tmathss', "G1SCHID", "G1TCHID", "G1TGEN", "G1TRACE","g1thighdegree", "g1tcareer","g1tyears","g1classsize","g1surban","race","gender","g1freelunch","gktmathss")
all_g1 <- tolower(all_g1)
boxdata<- star[, all_g1]

# Drop all the observations that have missing teacher id
boxdata <- boxdata[!is.na(boxdata$g1tchid),]

# Drop the school that does not have all 3 class types
drop_school <- c(244728, 244796, 244736, 244839)
boxdata <- boxdata[!(boxdata$g1schid %in% drop_school),]

# We will drop the observations that does not have math scores
boxdata <- boxdata[!is.na(boxdata$g1tmathss),]
boxdata <- boxdata[!is.na(boxdata$race),]
boxdata <- boxdata[!is.na(boxdata$g1freelunch),]

# Convert data type to factor for plotting
boxdata$g1surban <- factor(boxdata$g1surban,
                           levels = c("1", "2", "3","4"),
                           labels = c("Inner-City ", "Suburban", " Rural ", "Urban"))

# Reclassify race into "black" and "non-black"
boxdata$race <- factor(ifelse(boxdata$race == 2, "black", "non-black"))


library(dplyr)
library(ggplot2)
library(ggsci)


black_ratio <- boxdata %>%
  group_by(g1schid) %>%
  summarise(black_ratio = mean(race == "black", na.rm = TRUE)) %>%
  ungroup()

black_r <- boxdata %>%
  group_by(g1surban) %>%
  summarise(black_ratio = mean(race == "black", na.rm = TRUE)) 

boxdata <- boxdata %>%
  left_join(black_ratio, by = "g1schid")

# descending
boxdata <- boxdata %>%
  arrange(g1surban, desc(black_ratio))

school_stats <- boxdata %>%
  group_by(g1schid, g1surban) %>%
  summarise(
    black_ratio = mean(black_ratio),
    mean_math = mean(g1tmathss, na.rm = TRUE),
    sd = sd(g1tmathss, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_upper = mean_math + qt(0.975, df = n-1) * se,
    ci_lower = mean_math - qt(0.975, df = n-1) * se
  ) %>%
  ungroup() %>%
  arrange(g1surban, desc(black_ratio))



# order
school_stats$school_order <- seq_along(school_stats$g1schid)


# black ratio
ggplot(school_stats, aes(x = factor(school_order), y = mean_math, color = g1surban)) +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) + 
  geom_text(aes(label = round(mean_math, 2)), vjust = -0.5, size = 3) + 
  theme_bw() +
  scale_x_discrete(name = "Descending African American Ratio in Each Type of School", labels = round(school_stats$black_ratio, 2)) + 
  labs(title = " Math Score via Schools (with 95% Confidence Interval)",
       x = "Descending African American Ratio in Each Type of School",
       y = " Math Score",
       subtitle = paste0("Average Black Ratio in Each Location: Inner-City 95%, Suburban 33%, Rural 7%, Urban 13%"),
       color = "Urbanity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.subtitle = element_text(color = "darkgray", size = 9)) 



## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------
## 
## # Calculate and display proportions for 'g1classtype'
## g1classtype_proportions <- table(boxdata$g1classtype) / nrow(boxdata) * 100
## cat("Proportions for g1classtype:\n")
## print(g1classtype_proportions)
## 
## # Calculate and display proportions for 'g1tgen'
## g1tgen_proportions <- table(boxdata$g1tgen) / nrow(boxdata) * 100
## cat("\nProportions for g1tgen:\n")
## print(g1tgen_proportions)
## 
## # Calculate and display proportions for 'g1trace'
## g1trace_proportions <- table(boxdata$g1trace) / nrow(boxdata) * 100
## cat("\nProportions for g1trace:\n")
## print(g1trace_proportions)
## 
## # Calculate and display proportions for 'g1surban'
## g1surban_proportions <- table(boxdata$g1surban) / nrow(boxdata) * 100
## cat("\nProportions for g1surban:\n")
## print(g1surban_proportions)
## 
## # Calculate and display proportions for 'g1classtype'
## g1gender_proportions <- table(boxdata$gender) / nrow(boxdata) * 100
## cat("Proportions for g1gender:\n")
## print(g1gender_proportions)
## 
## # Calculate and display proportions for 'g1classtype'
## g1gender_race <- table(boxdata$race) / nrow(boxdata) * 100
## cat("Proportions for g1race:\n")
## print(g1gender_race)


## ----fig.align='center', fig.height=4, fig.width=7, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
# Load necessary libraries
library(ggplot2)
library(ggsci)

# Generate boxplot for 1st grade math scores by school urbanicity
ggplot(boxdata, aes(x = g1surban, y = g1tmathss, fill = g1surban)) +
  geom_boxplot() +
  scale_fill_jco() + # Apply JCO color palette
  labs(title = "1st Grade Math Scores by School Urbanicity",
       x = "School Urbanicity",
       y = "Math Score") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_fill_jco()



## ----fig.align='center', fig.height=4, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
# Load necessary libraries
library(ggplot2)
library(ggsci)



# Generate boxplot for 1st grade math scores for black and white students only
ggplot(boxdata, aes(x = race, y = g1tmathss, fill = race)) +
  geom_boxplot() +
  scale_fill_manual(values = c("black" = "black", "non-black" = "white")) +
  labs(title = "1st Grade Math Scores by Race (Black and White Students)",
       x = "Race",
       y = "Math Score") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_fill_jco() # Use JCO color palette from ggsci for aesthetic colors



## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)



# Calculate count and average math scores by race and g1surban
heatmap_data <- boxdata %>%
  group_by(race, g1surban) %>%
  summarise(count = n(),
            avg_math_score = mean(g1tmathss, na.rm = TRUE)) %>%
  ungroup()

# Plot the heatmap
ggplot(heatmap_data, aes(x = race, y = g1surban, fill = count)) +
  geom_tile() + # Create the heatmap tiles
  geom_text(aes(label = paste0("Count: ", count, "\nAvg Math Score: ", round(avg_math_score, 1))),
            color = "white", size = 3, fontface = "bold", lineheight = 0.9) + 
  scale_fill_gradient(low = "#efc000ff", high = "red", name = "Student Count") + # Color gradient for the values
  labs(title = "Student Count and Average Math Score by Race and School Urbanicity",
       x = "Race",
       y = "School Urbanicity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), # Rotate x-axis labels for better readability
        axis.text.y = element_text(angle = 0)) # Ensure y-axis labels are horizontal for clarity



## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------

# Re-categorize g1freelunch for clarity
boxdata$g1freelunch <- factor(boxdata$g1freelunch,
                              levels = c(1, 2),
                              labels = c("Free Lunch", "Non-free Lunch"))


# Calculate the count of students and average math scores by g1freelunch and race
heatmap_data_freelunch <- boxdata %>%
  group_by(g1freelunch, race) %>%
  summarise(count = n(),
            avg_math_score = mean(g1tmathss, na.rm = TRUE)) %>%
  ungroup()

# Plot the heatmap with count and average math score displayed
ggplot(heatmap_data_freelunch, aes(x = race, y = g1freelunch, fill = count)) +
  geom_tile(color = "white") + # Adding a border color for clarity
  geom_text(aes(label = paste("Count:", count, "\nAvg Math Score:", round(avg_math_score, 1))),
            color = "white", size = 3,fontface = "bold", lineheight = 0.9) +
  scale_fill_gradient(low = "#efc000ff", high = "red", name = "Student Count") +
  labs(title = "Count and Average Math Score by Free Lunch Eligibility and Race",
       x = "Race",
       y = "Free Lunch Eligibility",
       fill = "Count") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(angle = 0))



## ----fig.align='center', fig.height=4, fig.width=6, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------

# Calculate the average math scores by race and classtype
avg_math_scores_race <- boxdata %>%
  group_by(race, g1classtype) %>%
  summarise(avg_math_score = mean(g1tmathss, na.rm = TRUE)) %>%
  ungroup()

# Convert the summarized data back to a data frame for the interaction plot
avg_math_scores_race_df <- as.data.frame(avg_math_scores_race)

# Create the interaction plot
interaction.plot(x.factor = avg_math_scores_race_df$g1classtype,
                 trace.factor = avg_math_scores_race_df$race,
                 response = avg_math_scores_race_df$avg_math_score,
                 type = "b", # Use both points and lines
                 legend = TRUE,
                 xlab = "Class Type",
                 ylab = "Average Math Score",
                 main = "Interaction of Race and Class Type on Math Scores",
                 trace.label = "Race",
                 col = as.integer(avg_math_scores_race_df$race),
                 pch = as.integer(avg_math_scores_race_df$race),
                 xaxt = "n") # Prevent default x-axis labels

# Add custom x-axis labels for clarity, if necessary
axis(1, at = 1:length(unique(avg_math_scores_race_df$g1classtype)), labels = c("Small","Regular","Regular+Aid"))


## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
library(ggplot2)
library(dplyr)
library(ggsci) # For scientific journal color palettes

# Assuming 'boxdata' is your dataframe and 'g1tmathss' is your column of interest

# Calculate mean and standard deviation of g1mathss
mean_g1mathss <- mean(boxdata$g1tmathss, na.rm = TRUE)
sd_g1mathss <- sd(boxdata$g1tmathss, na.rm = TRUE)

ggplot(boxdata, aes(x = g1tmathss)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#0073C2FF", color = "#868686FF") +
  labs(title = "Distribution of First Grade Math Scores with Normal Curve",
       x = "G1 Math Scores",
       y = "Density",
       subtitle = paste("Mean (μ) =", round(mean_g1mathss, 2), "Standard Deviation (σ) =", round(sd_g1mathss, 2))) +
  stat_function(fun = dnorm, args = list(mean = mean_g1mathss, sd = sd_g1mathss), color = "#efc000ff", size = 1)  +
  theme_bw() +
  theme(plot.subtitle = element_text(color = "darkgray", size = 9))



## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------------------
library(lme4)

# get as factors
boxdata$g1classtype <-as.factor(boxdata$g1classtype)
boxdata$g1schid <- as.factor(boxdata$g1schid)
boxdata$g1tchid <- as.factor(boxdata$g1tchid)

# get model
model <- lmer(g1tmathss ~ g1classtype + g1schid + race + (1|g1tchid), data = boxdata)

# summary model
summary(model)


## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------------------

# Null model without class type effect
model_null <- lmer(g1tmathss ~ g1schid + race + (1|g1tchid), data = boxdata)

# Likelihood ratio test
anova(model_null, model)


## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------------------
library(emmeans)
# Pairwise comparisons of class sizes with adjustment for multiple testing
pairwise_comp <- emmeans(model, pairwise ~ g1classtype, adjust = "tukey")
summary(pairwise_comp)


## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
library(ggplot2)

# emm data
emm_data <- data.frame(
  ClassType = factor(c("Small", "Regular", "Regular + Aid"), levels = c("Small", "Regular", "Regular + Aid")),
  EMean = c(535, 521, 523),
  SE = c(1.65, 1.66, 1.72),
  LCL = c(531, 518, 520),
  UCL = c(538, 525, 526)
)

# plot
ggplot(emm_data, aes(x = ClassType, y = EMean, ymin = LCL, ymax = UCL)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(title = "Estimated Marginal Means (EMMs) of Math Scores by Class Type",
       x = "Class Type",
       y = "Estimated Marginal Mean (EMM)",subtitle = "With 95% Confidence Interval") +scale_fill_jco() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(plot.subtitle = element_text(color = "darkgray", size = 9))


## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------

library(ggplot2)
library(ggsci)
library(lme4)

# get data
residuals_data <- residuals(model)


plot_data <- data.frame(observation = 1:length(residuals_data), residuals = residuals_data)

# get plot
ggplot(plot_data, aes(x = observation, y = residuals)) +
  geom_point(aes(color = residuals), alpha = 0.6) +  # jco
  theme_bw() +  
  labs(title = "Residuals Plot", x = "Observation", y = "Residuals") +
  theme(legend.position = "none")  



## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
# Extract the residuals from the model
residuals_model <- residuals(model)

# Plotting the QQ plot for the residuals
# Create a QQ plot with a main title
qqnorm(residuals_model, main = "QQ Plot of Residuals", pch = 1)

# Add a reference line in red
qqline(residuals_model, col = "red", lwd = 2) # lwd = 2 makes the line slightly thicker

# Customize plot for a themebw-like appearance
# Setting margins around the plot
par(mar = c(5, 5, 4, 2) + 0.1)

# Setting the background color to white and removing box around the plot
par(bg = "white")
par(bty = "n")

# Customizing the color of the plot's text and labels
par(col.lab = "black", col.main = "black", col.axis = "black", col.sub = "black")

# Customizing the font size for labels and main title
par(cex.lab = 1.2, cex.main = 1.4)

# Note: 'par' function changes are global, so it might affect subsequent plots.



## ----include=FALSE----------------------------------------------------------------------------------------------------
library(lme4)

# Assuming 'boxdata' contains both 'g1tmathss' (end of first grade scores) 
# and 'gktmathss' (start of first grade scores), create a new variable for the change in scores
boxdata$score_change = boxdata$g1tmathss - boxdata$gktmathss

# Fit the modified model with 'score_change' as the dependent variable
model_diff <- lmer(score_change ~ g1classtype + g1schid + race + (1|g1tchid), data = boxdata)

# View the summary of the model to analyze the effects
summary(model_diff)




## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------------------
# model_diff <- lmer(score_change ~ g1classtype + g1schid + race + (1|g1tchid), data = boxdata)

model_null_diff <- lmer(score_change ~ g1schid + race + (1|g1tchid), data = boxdata)

anova_result <- anova(model_null_diff, model_diff)

print(anova_result)



## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------------------------

# Pairwise comparisons of class sizes with adjustment for multiple testing
pairwise_comp_diff <- emmeans(model_diff, pairwise ~ g1classtype, adjust = "tukey")
summary(pairwise_comp_diff)



## ----fig.align='center', fig.height=5, fig.width=8, message=FALSE, warning=FALSE, ,echo=FALSE-------------------------
# Assuming the emmeans results are stored in a dataframe named 'emm_data'
# Let's create that dataframe
emm_data <- data.frame(
  ClassType = factor(c("Small", "Regular", "Regular + Aid"), levels = c("Small", "Regular", "Regular + Aid")),
  EMM = c(45.0, 40.6, 42.7),
  SE = c(1.85, 1.94, 2.01),
  LCL = c(41.4, 36.8, 38.8),
  UCL = c(48.7, 44.4, 46.6)
)

# Calculate the lower and upper bounds of the confidence intervals
emm_data$LowerCI <- emm_data$EMM - (1.96 * emm_data$SE)
emm_data$UpperCI <- emm_data$EMM + (1.96 * emm_data$SE)

# Load ggplot2 for plotting
library(ggplot2)

# Create the plot
ggplot(emm_data, aes(x = ClassType, y = EMM, ymin = LowerCI, ymax = UpperCI)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2) +
  labs(title = "Estimated Marginal Means (EMMs) of Math Score Change by Class Type",
       x = "Class Type",
       y = "Estimated Marginal Mean (EMM) with 95% CI") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) # Improve readability of x labels



