# we will be talking about data.frames
# Lets import some data
download.file(url = "https://ndownloader.figshare.com/files/2292169", destfile = "data_raw/portal_data_joined.csv")


library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")


head(surveys)

str(surveys)

dim(surveys)
nrow(surveys)
ncol(surveys)

tail(surveys)

names(surveys)
# equvivalent to
colnames(surveys)
rownames(surveys)


summary(surveys)

# indexing and subsetting
surveys[1, 6]

surveys[1, ]
surveys[ ,1]

surveys[c(1,2,3), c(5, 6)]
surveys[c(1,2,3),]


surveys[1:3, 5:6]

surveys[ , -1]


surveys [ , "sex"]
surveys ["sex"]
surveys$plot_id


surveys_200 <- surveys[200, ]
surveys_200
nrow(surveys)
surveys[34786, ]
tail(surveys)
surveys[nrow(surveys), ]

# middle row
nrow(surveys)/2

surveys [ nrow(surveys)/2, ]
