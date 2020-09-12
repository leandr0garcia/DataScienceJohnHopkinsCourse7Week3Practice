install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

#install.packages("glue")

# devtools::install_github("jhudsl/matahari")
install.packages("matahari")
library(matahari)

dance_start(value = FALSE, contents = FALSE)
dance_save("~/college_major_analysis.rds")

install.packages("GGally")
library(GGally)
head(college)
install.packages("summarytools")
library(summarytools)
dfSummary(college)
typeof(college)
head(college)
typeof(college)

g = ggpairs(college)
?ggpairs

# library (plyr)
# df <- ldply (college, data.frame)
# summary(df)
# df

college
View(college)
dfCollege <- data.frame(college)

length(names(dfCollege))
summary(dfCollege)
View(dfCollege)
dfCollege$major_category <- as.factor(dfCollege$major_category)
dfCollege$major <- as.factor(dfCollege$major)
dfCollege$major <- NULL
dfCollege$rank <- as.factor(dfCollege$rank)
dfCollege$rank <- NULL
dfCollege$major_code <- as.factor(dfCollege$major_code)
dfCollege$major_code <- NULL
summary(dfCollege)

?dev
dev.new(din=c(1200, 1200)) # avoiding the R studio plot viewer.
g = ggpairs(data = dfCollege, cardinality_threshold = 173)
g

ggsave(filename = "ggpairs_factored.png", device = png(width = 1024, height=1024), plot = g)



w = ggpairs(data = dfCollege, columns = c("median", "major_category", "perc_employed_fulltime_yearround"), cardinality_threshold = 16)
ggsave(filename = "ggpairs_median_vs_category.png", device = png(width = 1024, height=1024), plot = w)

fit <- lm(median ~ major_category + sample_size + total, data = dfCollege)
fit <- lm(median ~ major_category * sample_size + total, data = dfCollege)

## Linear model fit, group A is the reference
summary(fit)
summary(fit)$coef
unique(dfCollege$major_category)


my_plot <- ggplot(data=dfCollege, aes(x=major_category, y=median, col = major_category))
+ ggplot2::geom_point() + geom_smooth(method = "lm")
my_plot + geom_violin()
my_plot + geom_boxplot()


z = ggpairs(data = dfCollege, columns = c("median", "major_category"), aes())

custom_data <- data.frame(income = dfCollege$median, category = dfCollege$major_category)
library(reshape)
View(custom_data)
cast(data = custom_data, formula = category ~ income)
