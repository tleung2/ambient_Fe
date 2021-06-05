   ### LOAD PACKAGES
library(tidyverse)
library(GGally)
library(viridis)
library(RColorBrewer)


   ### LOAD DATA chase_lakes_final and ambient_Fe
   ### Data named as ambient_Fe = ambient_Fe
   ### Data names as chase_Fe = chase_lakes_final

   ### Convert columns to numeric
chase_Fe[,c(11:33)] <-as.numeric(chase_Fe[,c(11:33)])
########################################################################
   ######################   CHECK NORMALITY   ######################
ggplot(chase_Fe, aes(x = Fe)) +
  geom_histogram()

   ### log Fe values to improve distribution
chase_Fe$logFe <-log(chase_Fe$Fe)

   ### Check distribution
   ### Still not normal distribution but better than earlier
ggplot(chase_Fe, aes(x = logFe)) +
  geom_histogram()


#######################################################################
   ######################   BOXPLOTS: Fe   ######################

   ### Subset 2018 data since this is most consistent
   ### and n is largest for this year
chase.2018<-subset(chase_Fe, Year == 2018)
chase.2019<-subset(chase_Fe, Year == 2019 & Fe > 0)
chase_Fe2<-chase_Fe %>%
  subset(!Site %in% c("Honey Creek Resort", "Union Grove", 
                    "Crandall's Beach", "Lake Wapello") & Fe > 0)

   ### make boxplots to look at the variance
   ### plot by order of variance

ggplot(data = chase_Fe2, 
       aes(y = Fe, x = reorder(Site2, Fe, FUN = median, .desc = FALSE, na.rm = FALSE))) +
  geom_boxplot(aes(fill = Waterbody)) +
  scale_fill_brewer(palette = "BrBG") +
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.position = "top",
        legend.key = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank())

#######################################################################
   #################    CORRELATION ANALYSIS     #################
set.seed(123)  ## ensures repetition

   ### 1) Group lakes together based on previous boxplot
   ### Lakes with similar Fe distribution need to be together

group1<- chase_Fe2 %>% 
  select(-c(1,3,6,8:12,14:34)) %>%
  subset(Site2 %in% c("Lake Darling","Prairie Rose",
                                       "Lake Mananwa", "Lake Keomah",
                                       "Lake of Three Fires")) %>%
  pivot_wider(names_from = "Site2", values_from = "Fe")

group2<- chase_Fe2 %>%
  select(-c(1,3,7,8:12,14:34)) %>%
  subset(Site %in% c("North Twin","Viking Lake", "Big Creek",
                                       "Clear Lake","Rock Creek","Brushy Creek",
                                       "Black Hawk","Green Valley")) %>%
  pivot_wider(names_from = "Site", values_from = "Fe")

group3<- chase_Fe2 %>%
  select(-c(1,3,6,8:12,14:34)) %>%
  subset(Site2 %in% c("Beeds Lake", "Lower Pine Lake")) %>%
  pivot_wider(names_from = "Site2", values_from = "Fe")

   
   ### 

   ### nmds of original data
   ### color by summer period
   ### need to pivot back to long format
lakes_fe4<-pivot_longer(lakes_fe3, 3:5, names_to = "summer_period", 
                        values_to = "TDFe")
   ### pivot wider to place each site as column                       
lakes_fe5<-pivot_wider(lakes_fe4, names_from = c(Site_Name), values_from = TDFe,
                       id_cols = c(summer_period))
   ### run nmds 
mds.res2<-metaMDS(lakes_fe5[,c(2:80)], k = 2, distance = "bray", 
                 maxit = 999)
mds.res2

#######################################################################
   ####################   PREPARE FOR GGPLOT   ####################

   ### 1) Extract nMDS output into a dataframe 
   ### Use the score () to extraxt site scores and convert to data frame
mds.scores<-as.data.frame(scores(meta.res))
mds.scores2<-as.data.frame(scores(mds.res2))

   ### 2) create solumn of site names from row names of meta.scores
mds.scores$site<-rownames(mds.res)
mds.scores2$site<-rownames(mds.res2)

   ### 3) add details to mds.scores dataframe
mds.scores$body<-lakes_fe3$Body
mds.scores$site_name<-lakes_fe3$Site_Name
mds.scores$start_fe<-lakes_fe3$Start
mds.scores$middle_fe<-lakes_fe3$Middle
mds.scores$end_fe<-lakes_fe3$End

mds.scores2$summer_period<-lakes_fe5$summer_period
   
   ### Save mds scores dataframe
save(mds.scores, file = "mds_scores_lakes_fe.rda")

   ### 4) Extract Species scores into dataframe 
   ### Use score () to extract species score from mds output 
   ### and convert to data frame
species.score<-as.data.frame(scores(mds.data, "species"))

   ### 5) create columns of species from the species score dataframe
species.score$species<-rownames(species.score)
   ### check species dataframe
head(species.score)


########################################################################
   ################   PLOT nMDS WITH GGPLOT   #####################
## -------------- Plot nMDS of references only  -----------------
## hulling only default refs, alpha = transparency w/ 0 being transparent
p1<-ggplot() +
  geom_point(data = mds.scores, aes(x=NMDS1, y=NMDS2, color = end_fe), size = 5) +
  #geom_text(data = grp.default, aes(x = NMDS1, y = NMDS2), size = 7, nudge_x = 0.1) +
  scale_color_viridis_c() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=18), 
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18), 
        legend.key=element_rect(fill='white'),
        legend.title = element_blank())
p1


##########################################################################
   ####################   SIGNIFICANCE TESTING   ####################

kruskal.test(lakes_fe$TDFe, lakes_fe$landform, correct=FALSE, na.rm = TRUE)
