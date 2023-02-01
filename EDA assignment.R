hotel_bookings<-read.csv("C:/Users/Owner new/Downloads/hotel_bookings - hotel_bookings.csv")
View(hotel_bookings)
dim(hotel_bookings)
str(hotel_bookings)
names(hotel_bookings)
attributes(hotel_bookings)
sum(is.na(hotel_bookings))
head(hotel_bookings)
tail(hotel_bookings)
hotel_bookings[5:10,]
hotel_bookings[5:10,"adults"]
summary(hotel_bookings)
mean(hotel_bookings$adults)
median(hotel_bookings$adults)
IQR(hotel_bookings$adults)
range(hotel_bookings$adults)
quantile(hotel_bookings$adults,c(0.10,0.60,0.70))
var(hotel_bookings$adults)
sd(hotel_bookings$adults)

library(Hmisc)
describe(hotel_bookings)

#----------Histogram
hist(hotel_bookings$adults)
hist(hotel_bookings$adults,col = "red",xlab = "Adults",main = "Histogram for adults")


#----------Density plot
plot(density(hotel_bookings$adults))

ggplot(data = hotel_bookings,aes(adults,fill=hotel))+
  geom_density()
# Explore categorical variables
table(hotel_bookings$adults)

pie(table(hotel_bookings$adults),
    main = "Pie chart for adults",col = c("white","blue"))

# Bar Plot
barplot(table(hotel_bookings$adults))

# Correlation
cor(hotel_bookings$adults,hotel_bookings$lead_time)
plot(hotel_bookings$adults,hotel_bookings$lead_time)


