##################################################################################
#performing exploratory data analysis on yelp.com data in R
#Jason Martins - github - jmar316
##################################################################################

##################################################################################
#Read in data from yelp.com
#pre-processing
##################################################################################
yelp_data = read.table("yelp.dat.txt",header = TRUE, sep = ";", quote = "\"", fill = TRUE, comment.char = "")
summary(yelp_data)

##################################################################################
#plots a histogram of the "tip count" attribute.
hist(yelp_data$tip_count,main="Histogram of tip_count attribute",xlab="tip_count") 

#plots a histogram using the log values of "tip count"
hist(log(yelp_data$tip_count),main="Histogram of tip_count attribute",xlab="Log: tip_count")

#plots tip count attribute using density function, + logged values
tip_density = density(yelp_data$tip_count) 
tip_density_log = density(log(yelp_data$tip_count)) 
plot(tip_density,main="Density plot of tip_count attribute",xlab=" tip_count")
plot(tip_density_log,main="Density plot of tip_count attribute (log)",xlab="Log: tip_count")

##################################################################################
#analysis determined that review_count was the continuous attribute with largest range
#plots a histogram of values, with appropriate title for clarity
##################################################################################

range_of_review_count = max(yelp_data$review_count) - min(yelp_data$review_count)
hist(yelp_data$review_count,main="Histogram of review_count attribute",xlab="review_count",breaks=50)

##################################################################################
#among the discrete attributes with fewer than 10,000 unique values
#finds the attribute with the maximum number of values and plot a barplot to show the frequency of each value
##################################################################################

#find column with most number of unique occurances
for (i in 1:ncol(yelp_data)){
     temp_length = nrow(unique(yelp_data[i]))
     print(paste(colnames(yelp_data[i]),temp_length))
}

attributes_vector = as.vector((yelp_data$attributes))
barplot(table(attributes_vector),main="Frequency of Attributes", ylab="Frequency")

##################################################################################
#considers the four continuous attributes: latitude, longitude, stars, likes. 
#calculates the pairwise correlations among the four attributes
#scatterplot of attrib pair with largest positive correlation 
#scatterplot of attrib pair with largest negative correlation  

# report: both outputs are expected, longitude and latitude are very closely related 
#while likes & longitude really have nothing to do with one another
##################################################################################
cor(yelp_data$latitude, yelp_data$longitude) #[1] 0.9555044 (largest)
cor(yelp_data$latitude, yelp_data$stars) #[1] 0.1306059
cor(yelp_data$latitude, yelp_data$likes) #[1] -0.04086409
cor(yelp_data$longitude, yelp_data$stars)#[1] 0.140871
cor(yelp_data$longitude, yelp_data$likes)#[1] -0.07586773 (biggest negative)
cor(yelp_data$stars, yelp_data$likes) #[1] 0.1215371

plot(yelp_data$latitude, yelp_data$longitude, main = "Attributes with largest positive correlation (0.9555044)", xlab = "Latitude", ylab='Longitude')
plot(yelp_data$longitude, yelp_data$likes, main = "Attributes with largest negative correlation (-0.07586773)", xlab = "Longitude", ylab='Likes')
 
##################################################################################
#created a binary variable for the Nightlife category, the binary variable will 
#record whether or not the review/example contains the category

#plots a boxplot of nightlife vs "stars" 
#and a boxplot of nightlife vs "likes" 
##################################################################################
yelp_data$nightlife_yn <- 0 
for (i in 1:length(yelp_data$categories)){
  grep_result = grepl("Nightlife", yelp_data$categories[i])
  if(grep_result == "TRUE"){
    yelp_data$nightlife_yn[i] <- 1
  }
}
boxplot(yelp_data$stars ~ yelp_data$nightlife_yn, main="Stars vs Nightlife Category", xlab= "Nightlife (0 = NO) (1 = YES)", ylab="Stars")
boxplot(yelp_data$likes ~ yelp_data$nightlife_yn, main="Likes vs Nightlife Category", xlab= "Nightlife (0 = NO) (1 = YES)", ylab="Likes")



##################################################################################
#created binary variables for the bars and diners category, the binary variable will 
#record whether or not the review/example contains the category.
#compared each with stars to see if there is a relationship between the two variables

#results expected because there are other aspects to the overall experience. For example an individual
#will not give 5 stars to an establishment just because it is a bar. Other things are taken into consideration, like
#location, customer service, timely-ness, etc.
##################################################################################
yelp_data$bars_yn <- 0
for (i in 1:length(yelp_data$categories)){
  grep_result = grepl("Bars", yelp_data$categories[i])
if(grep_result == "TRUE"){
  yelp_data$bars_yn[i] <- 1
}
}
yelp_data$diners_yn <- 0
for (i in 1:length(yelp_data$categories)){
  grep_result = grepl("Diners", yelp_data$categories[i])
if(grep_result == "TRUE"){
  yelp_data$diners_yn[i] <- 1
}
}
boxplot(yelp_data$stars ~ yelp_data$diners_yn, main="Stars vs Diners Category", xlab= "Diners (0 = NO) (1 = YES)", ylab="Stars")
boxplot(yelp_data$stars ~ yelp_data$bars_yn, main="Stars vs Bars Category", xlab= "Bars (0 = NO) (1 = YES)", ylab="Stars")
