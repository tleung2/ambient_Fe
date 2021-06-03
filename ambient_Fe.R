   ### LOAD PACKAGES
library(tidyverse)
library(vegan)
library(viridis)


   ### LOAD DATA chase_lakes_final and ambient_Fe
   ### Data named as ambient_Fe = ambient_Fe
   ### Data names as chase_Fe = chase_lakes_final

########################################################################
   ######################   CHECK NORMALITY   ######################
ggplot(ambient_Fe, aes(x = TDFe)) +
  geom_histogram()

ambient_Fe$logFe <-log(ambient_Fe$TDFe)

ggplot(ambient_Fe, aes(x = logFe)) +
  geom_histogram()




#######################################################################
   #################   TIDY DATA FOR nMDS   #####################

   ### Pivot dataframe wider (each lake has 3 Fe measurements across)
   ### id_cols --> include relevant data you want that is not pivoted
lakes_fe2 <- lakes_fe[-c(1)] %>%
  pivot_wider(names_from = Summer_Period, values_from = TDFe, 
              id_cols = c(Site_Name, Body))

lakes_fe3<-na.omit(lakes_fe2)
#######################################################################
   #######################    RUN nMDS    ########################
set.seed(123)
   ### nmds of pivoted data
   ### cannot color by summer period
mds.res<-metaMDS(lakes_fe3[,c(3:5)], k = 2, distance = "bray", 
                  maxit = 999)
meta.res
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
