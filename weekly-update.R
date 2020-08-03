# Bookings

ggplot(Bkgs_data,aes(x=factor(Month), y=`ACV Bkgs`, fill = `Service Type`))+  #Bookings by Month and SKU (Weekly Update)
  geom_bar(stat = "sum", position = "dodge")+
  #ylab("Total ACV Booking Dollars")+
  scale_fill_brewer(palette="Dark2")+
  ylim(0,160000)+
  scale_x_discrete("Month", labels = c("3" = "March","4" = "April", "5" = "May", "6" = "June", "7" = "July"))+
  #ggtitle("Total TCV Bookings Per Month")
  theme_classic()

ggplot(Bkgs_data,aes(y=`Area (group)`, x=PUPY))+  #Bookings PUPY boxplot (Weekly Update)
  geom_boxplot(width = .5, fill = "Dark Green") + 
  xlab("Selling Price PUPY")+ylab("Area")+
  ggtitle("Selling Price by Area")+
  theme_classic()

#Pipeline
cas_pipeline_summary <- cas_pipeline %>% #Pipeline by cust area
  group_by(cust_area,`probability (group)` ) %>% 
  summarise(size = sum(opportunity_line_total_price), total_opp = n())
#CAS Pipeline by Area
ggplot(cas_pipeline_summary, aes(x=cust_area, y=size, fill=`probability (group)`))+
  geom_bar(stat = "sum")+
  coord_flip()+
  xlab("Customer Region") + ylab("Total Dollar Opportunity")+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()