df <- read.csv("/Users/papakilo/Documents/ML Data Files/BreastCancerData.csv")
df <- subset(df, select = -X )

#turn classifiction to Binary
df$diagnosis <- gsub("M","1",df$diagnosis)
df$diagnosis <- gsub("B","0",df$diagnosis)
df$diagnosis <- as.numeric(df$diagnosis)

#Remove the " . " from column names
colnames(df)[30] <- "concave_points_worst"
colnames(df)[10] <- "concave_points_mean"
colnames(df)[20] <- "concave_points_se"
#Scale data
maxs <- apply(df,2,max)
mins <- apply(df,2,min)
scaled.df <- scale(df,center = mins,scale = maxs-mins)
scaled.df <- data.frame(scaled.df)
scaled.df[1] <- NULL
#Train Test Split
library(caTools)
split <- sample.split(scaled.df$diagnosis, SplitRatio = 0.7)
train <- subset(scaled.df, split == T)
test <- subset(scaled.df, split == F)

library(neuralnet)
n <- names(train)
f <- as.formula(paste("diagnosis ~", paste(n[!n %in% "diagnosis"], collapse = " + ")))
nn <- neuralnet(f, data = train,hidden = c(16,8,4),linear.output = F,stepmax=1e6)
#plot(nn) to see a visualization of the nn
## Get your predicted values ** do not call the variable you are predicting
predicted.nn.value <- neuralnet::compute(nn,test[2:31])
predictions <- sapply(predicted.nn.value$net.result, round)


## show results
table <- table(predictions,test$diagnosis)
print(table)
## Plot Confustion matrix
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(106, 1, 3, 6)
df <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")