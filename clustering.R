install.packages("rattle")

require(rattle)
require(NbClust)
require(dplyr)
require(data.table)

data(wine, package = "rattle")
head(wine)


png("WGSS_2015.png", width = 6, height = 6, units = 'in', res = 300)

df <- difftimes

wssplot(df)

dev.off()


library(NbClust)
set.seed(1234)

png("cluster.png", width = 6, height = 6, units = 'in', res = 300)

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

dev.off()


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

df <- (matrixC[-1])

#use my dataset

cluster_test_new <- na.omit(cluster_test_new)

cluster_test_copy[1] <- NULL

df <- open_matrix[-1]

wssplot(df)

png("Dindex_values.png", width = 8, height = 4, units = 'in', res = 300)

dev.off()

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

table(nc$Best.n[1,])

png("barplot_2015.png", width = 6, height = 6, units = 'in', res = 300)

barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

dev.off()

set.seed(1234)

fit.km <- kmeans(df, 3, nstart=25)   

fit.km$centers  

ct.km <- table(matrix$Type, fit.km$cluster)

non_matching <- subset(Oct_2013S, !(Oct_2013S$id %in% Oct_2014S$id))

new_matrix <- merge(matrix, Oct_2015S, by.x = "id", by.y = "id", all.x = TRUE)


png("W 45th & 8th.png", width = 6, height = 6, units = 'in', res = 300)


ggplot(third_most, aes(x=year, y=available_bike_count, fill=year)) + geom_boxplot() + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="year",
                    breaks=c("2013", "2014", "2015"),
                    labels=c("2013", "2014", "2015")) +
  geom_text(data = means, aes(label = available_bike_count, y = available_bike_count + 0.08)) +
  ggtitle("W 45th & 8th")

dev.off()

library(lattice)


test <- Json_Octobers[Json_Octobers$ stations, by.x = "station_id", by.y = "id", all.x = TRUE)




png("cluster_lines_2015.png", width = 10, height = 5, units = 'in', res = 300)



ggplot(data=melted2, aes(x=variable, y=value, color = cluster)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  xlab("hour") +
  ylab("availability factor") + 
  theme(axis.title.x = element_text(family="Courier")) +
  theme(axis.title.y = element_text(family="Courier")) +
  theme(legend.title=element_text(family="Courier")) +
  theme(axis.text.x = element_text(family = "Courier"))



png("cluster_lines_2015.png", width = 10, height = 5, units = 'in', res = 300)

  ggplot(data=melted, aes(x=variable, y=value, group=cluster, color=cluster)) +
  geom_point() + 
  geom_line(size = 1, alpha= 0.7) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "white"))+ 
  theme(axis.text.y = element_text(color = "white")) +
  theme(plot.title = element_text(family="sans", face = "bold", colour = "white")) +
  theme(axis.title.x = element_text(family="sans",face = "bold", color ="white")) +
  theme(axis.title.y = element_text(family="sans",face = "bold", color = "white")) +
  ggtitle("Mean centers of hourly availability") +
  xlab("hour") +
  ylab("load factor") +
  theme(panel.background = element_rect(fill = "#191919"))+
  theme(plot.background = element_rect(fill = "#191919")) +
  theme(legend.background =  element_rect(fill = "#191919")) +
  theme(legend.text = element_text(family = "sans", colour = "white")) +
  theme(legend.title = element_text(family = "sans", colour = "white")) +
  theme(legend.key = element_rect(fill = "#191919")) +	
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
  
dev.off()


ggplotly(g)


library(plotly)
p <- plot_ly(melted, x = variable, y = value,
             color = cluster)
p
