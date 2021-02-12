#importing data sets along with the file path

data_set_1 = read.csv(file = "E:/R workshop/ab_data.csv" , header = T)
head(data_set_1)
data_set_2 = read.csv(file = "E:/R workshop/countries.csv" , header = T)
head(data_set_2)

#viewing imported data sets (EDA)

View(data_set_1)
View(data_set_2)

nrow(data_set_1)
ncol(data_set_1)
colnames(data_set_1)

nrow(data_set_2)
ncol(data_set_2)
colnames(data_set_2)

#visualizing the division among the control and treatemnt groups
table(data_set_1$group)
barplot(table(data_set_1$group), main = 'Groups', col = c('Blue','Yellow'))

#Visualizing the landing page dimensions
table(data_set_1$landing_page)
barplot(table(data_set_1$landing_page), main = 'Landing Page', col = c('SkyBlue','Orange'))

#pie graph to visulaize the country-wise divisons in the dataset
table(data_set_2$country)
pie(table(data_set_2$country), main= 'Countries', col = c('yellow','pink','blue'))

#merging the datasets
data_set = merge.data.frame(data_set_2,data_set_1, by = 'user_id')
head(data_set)
#checking whether the groups are randomly assigned to the respective viewers 
data_set = cbind(data_set,  ran = rnorm(294478))
head(data_set)
ggplot(data_set, aes(x=ran, fill= group)) + geom_density(alpha=.2) + xlab("ran")

#checking missing values
sapply(data_set, function(x){sum(is.na(x))})
#checking for duplicate values in the dataset
library(sqldf)
data_duplicate <- sqldf("select user_id, timestamp, count(*) from data_set group by user_id, timestamp")
head(data_duplicate)
View(data_duplicate)
nrow(data_duplicate)

#testing for different cases

library(dplyr)
CA_old= filter(data_set, country == 'CA', landing_page == 'old_page')
table(CA_old$group,CA_old$converted)
fisher.test(matrix(c(855,6343,15,77),ncol=2), alternative = 'less')

CA_new= filter(data_set, country == 'CA', landing_page == 'new_page')
table(CA_new$group,CA_new$converted)
fisher.test(matrix(c(16,88,817,6484),ncol=2) , alternative= 'less')


UK_old= filter(data_set, country == 'UK', landing_page == 'old_page')
table(UK_old$group,UK_old$converted)
fisher.test(matrix(c(4364,31996,50,422),ncol=2) , alternative= 'less')

UK_new= filter(data_set, country == 'UK', landing_page == 'new_page')
table(UK_new$group,UK_new$converted)
fisher.test(matrix(c(64,417,4375,31731),ncol=2) , alternative= 'less')

US_old= filter(data_set, country == 'US', landing_page == 'old_page')
table(US_old$group,US_old$converted)
fisher.test(matrix(c(12270,89446,185,1216),ncol=2) , alternative= 'less',conf.level = 0.99)

US_new= filter(data_set, country == 'US', landing_page == 'new_page')
table(US_new$group,US_new$converted)
fisher.test(matrix(c(154,1189,12072,89832),ncol=2) , alternative= 'less')

old_page= filter(data_set, landing_page == 'old_page')
table(old_page$group,old_page$converted)
fisher.test(matrix(c(17489,127785,250,1715),ncol=2) , alternative= 'less')

new_page= filter(data_set, landing_page == 'new_page')
table(new_page$group,new_page$converted)
fisher.test(matrix(c(234,1649,17264,128047),ncol=2) , alternative= 'less')






