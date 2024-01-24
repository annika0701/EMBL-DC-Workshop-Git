
plt <- ggplot(data = surveys_complete,
       mapping = aes(x=weight, y= hindfoot_length)
)

plt
str(plt)

plt +
  geom_point()

plt +
  geom_point() +
  ggtitle("My first plot!")

# 1. define ggplot object
# plt <- ggplot(data = <data.frame>, mapping = <aestethics>)
# x aestetics
# y aestetics
# color aestetics
# shape aestetics
#2. add geometry layer(s)
# geometry functions have predictable names
# geom_{point, line, bar, histogram, violin, hex,....}

plt +
  ggtitle("Weight vs hindfoot length")

plt +
  geom_point()

install.packages("hexbin")
library(hexbin)
ggplot(data= surveys_complete, mapping = aes(x=weight, y= hindfoot_length))+
  geom_hex()

ggplot(data = surveys_complete,
              mapping = aes(x=weight, y= hindfoot_length))+
  geom_point(alpha = 0.1)    #alpha is transparency

ggplot(data = surveys_complete,
       mapping = aes(x=weight, y= hindfoot_length))+
  geom_point(alpha = 0.1, color = "blue") 

ggplot(data = surveys_complete,
       mapping = aes(x=weight, y= hindfoot_length))+
  geom_point(alpha = 0.25, aes (color = species_id))

ggplot(
  data = surveys_complete,
  mapping = aes(
    x=weight,
    y= hindfoot_length,
    color = species_id
  )
)+
  geom_point(alpha = 0.25)

# challenge 
# scatterplot weight vs species_id color by plot_type

ggplot(data = surveys_complete,
       mapping = aes(
         x= weight,
         y= species_id,
         color = plot_type
       ))+
  geom_point(alpha = 0.1)

ggplot(data = surveys_complete,
       mapping = aes(
         x= species_id,
         y= weight,
         color = plot_type
       ))+
  geom_point()



ggplot(data = surveys_complete,
       mapping = aes(
         x= species_id,
         y= weight)
       )+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha= 0.3, color = "salmon")  #adding a little value for each x coordinate

ggplot(data = surveys_complete,
       mapping = aes(
         x= species_id,
         y= weight)
)+
  geom_jitter(alpha= 0.3, color = "salmon")+
  geom_boxplot(outlier.shape = NA, fill =NA)

# Challenge: produce a violin plot of weight by species_id

ggplot(data = surveys_complete,
       mapping = aes(
         x= species_id,
         y= weight)
)+
  geom_violin() +
  scale_y_log10() +
  ylab("Weight(log10")

#challenge: make a boxplot + jittered scatter plot of hindfoot_length by spcies_id. 
# Boxplot should be infront of the dots and filled with white

ggplot(data = surveys_complete,
       mapping = aes(
         x= species_id,
         y= hindfoot_length)
)+
  geom_jitter(alpha= 0.3, color = "lightblue")+
  geom_boxplot(outlier.shape = NA)

ggplot(data = surveys_complete,
       mapping = aes(
         x= species_id,
         y= hindfoot_length)
)+
  geom_jitter(aes(color = plot_id))+
  geom_boxplot(outlier.shape = NA)

#how to define colors:
# "red", "green"
# rgb(red= .3, green= .3)
# #dedede hexadecimal code



yearly_count <- surveys_complete %>% count(year, genus)
view(yearly_count)

ggplot(data=yearly_count, 
       mapping = aes(
         x=year, 
         y=n, 
         group= genus))+
  geom_line()

ggplot(data=yearly_count, 
       mapping = aes(
         x=year, 
         y=n, 
         color = genus))+
  geom_line()



yearly_count %>% 
  ggplot(mapping = aes(x=year, y=n, color=genus))+
  geom_line()

yearly_count_graph <- surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(mapping = aes(x=year, y=n, color = genus))+
  geom_line()
yearly_count_graph


ggplot(data = yearly_count, mapping = aes(x=year, y=n))+
  geom_line()+
  facet_wrap(facets = vars(genus))


surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus))

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color = sex))+
  geom_line()+
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
    )

plt <- surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus)) +
  scale_color_manual(
    values = c("tomato", "dodgerblue"),
    labels = c("female", "male"),
    name= "sex")+
  xlab("Year of observation")+
  ylab("Number of individuals")+
  ggtitle("Observed genera over time")+
  theme_bw(base_size = 8)+
  theme(
    legend.position = "bottom", 
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

  plt
  ggsave(filename = "data/plot.pdf", plot = plt,
       width = 20,
       height = 20)
