   ### LOAD PACKAGES
library(tidyverse)
library(GGally)
library(viridis)
library(RColorBrewer)
library(Hmisc)
library(corrplot)

   ### Type in packages needed
load_pkg <- rlang::quos(tidyverse, GGally, RColorBrewer, Hmisc, corrplot)
   ### Load these packages
invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE))


   ### LOAD DATA chase_lakes_final and ambient_Fe
   ### Data named as ambient_Fe = ambient_Fe
   ### Data names as chase_Fe = chase_lakes_final
   ### Data names as all.2018 = all_lakes_2018_Fe

   ### Convert columns to numeric
chase_Fe[,c(11:33)] <-as.numeric(chase_Fe[,c(11:33)])
########################################################################
   ######################   CHECK NORMALITY   ######################
ggplot(all.2018, aes(x = avg_Fe)) +
  geom_histogram()
   ### Shapiro test
   ### if P > 0.05, then normally distributed
shapiro.test(all.2018$avg_Fe)  ### failed normal distribution test

   ### log Fe values to improve distribution
all.2018$log_avgFe <-log(all.2018$avg_Fe)

   ### Use Shapiro to check distribution of logged values
shapiro.test(all.2018$log_avgFe)

   ### Check distribution
   ### Still not normal distribution but better than earlier
ggplot(all.2018, aes(x = log_avgFe)) +
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
   ### plot by order of median using reorder() and FUN = median

ggplot(data = all.2018, 
       aes(y = avg_Fe, x = reorder(Site2, avg_Fe, FUN = median, .desc = FALSE, na.rm = TRUE))) +
  geom_boxplot(aes(fill = Waterbody2)) +
  #stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
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

   ##################  1) Group lakes together   ####################
   ### based on previous boxplot and pivot_wider so each lake is a column
   ### Lakes with similar Fe distribution need to be together
   
   ### group 1
group1<- chase_Fe2 %>% 
  select(-c(1,3,6,8:12,14:34)) %>%
  subset(Site2 %in% c("Lake Darling","Prairie Rose",
                                       "Lake Mananwa", "Lake Keomah",
                                       "Lake of Three Fires")) %>%
  pivot_wider(names_from = "Site2", values_from = "Fe")

   ### group 2
group2<- chase_Fe2 %>%
  select(-c(1,3,7,8:12,14:34)) %>%
  subset(Site %in% c("North Twin","Viking Lake", "Big Creek",
                                       "Clear Lake","Rock Creek","Brushy Creek",
                                       "Black Hawk","Green Valley")) %>%
  pivot_wider(names_from = "Site", values_from = "Fe")

   ### group 3
group3<- chase_Fe2 %>%
  select(-c(1,3,6,8:12,14:34)) %>%
  subset(Site2 %in% c("Beeds Lake", "Lower Pine Lake")) %>%
  pivot_wider(names_from = "Site2", values_from = "Fe")

   
   #################   2) ggpairs: correlation analysis  #################
   ### Run correlation for each group
ggpairs(group1[,c(4:7)])
ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

group2 %>%
  subset(Year == 2019) %>%
  select(-c(1:3,6,7)) %>%
  ggcorr(method = c("pairwise", "spearman")) +
  #ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


pivot.2018 %>%
  select(-c(1:2)) %>%
  ggpairs(method = c("pairwise", "spearman")) +
  #ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#######################################################################
   ##################    CORRELATION ANALYSIS    ##################

   ### Pivot wider 2018 Fe data so each lake is a column
pivot.2018<-all.2018 %>%
  na.omit() %>%
  select(c(4,5,7,13)) %>%
  pivot_wider(names_from = "Site2", values_from = "avg_Fe")

   ### Spearman correlation matrix
res<-rcorr(as.matrix(pivot.2018[,c(3:32)], type = "spearman"))
res
#######################################################################
   ##################   VISUALIZING CORRELATION   #################

   ### Be careful of where to put pvalue and r value
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(res$r, method = "color", 
         type = "upper", 
         order = "hclust", 
         tl.col = "black",
         col = brewer.pal(n = 11, name = "RdYlBu"),
         p.mat = res$P, sig.level = 0.05, 
         insig = "blank")


######################################################################
   ###############   SAVE CORRELATION ANALYSIS   #################

   ############   1) Flatten Function   ###############
   ### Create function to flatten correlation output
   ### cormat : matrix of the correlation coefficients
   ### pmat : matrix of the correlation p-values
   ### Make sure Hmisc package is turned on
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
   
   ############   2) Flatten correlation matrix   #################
   ### assign matrix as a dataframe
res.table<-flattenCorrMatrix(res$r, res$P)
   ############   3) Save correlation matrix   #################
   ### save inro R data
save(res.table, file = "spearman_corr_all_lakes.rda")
   ### save as .csv file
write.csv(res$P, file = "spearman_p_all_lakes.csv")

######################################################################
   ################   TIDYING CORRELATION RESULTS   ##############
res.table2<-subset(res.table, p < 5e-2)

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


   ### Mann-Whiteney test
   ### Is the range in Fe different between lakes?
   ### wilcox.test(dependent~independent)
wilcox.test(all.2018$avg_Fe, all.2018$Site2, exact = FALSE)

kruskal.test(lakes_fe$TDFe, lakes_fe$landform, correct=FALSE, na.rm = TRUE)
