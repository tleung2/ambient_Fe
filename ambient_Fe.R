   ### LOAD PACKAGES
library(tidyverse)
library(GGally)
library(viridis)
library(RColorBrewer)
library(Hmisc)
library(corrplot)
library(ggpmisc)
library(lme4)

   ### Type in packages needed
load_pkg <- rlang::quos(tidyverse, GGally, RColorBrewer, Hmisc, 
                        corrplot, ggpubr, ggpmisc,lme4, vegan)
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
ggplot(all.2018, aes(x = Fe)) +
  geom_histogram()
   ### Shapiro test
   ### if P > 0.05, then normally distributed
shapiro.test(all.2018$Fe)  ### failed normal distribution test

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


   ### ----  1)  Boxplots of DFe from 130 lakes  --------
   ### plot by order of median using reorder() and FUN = median
ambient_avgFe %>%
  na.omit() %>%
ggplot(aes(y = mean, x = reorder(Waterbody2, mean, FUN = median, .desc = FALSE, na.rm = TRUE))) +
  geom_boxplot(aes(fill = Waterbody2), na.rm = TRUE) +
  #stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
  scale_fill_brewer(palette = "BrBG") +
  labs(y = expression(paste('Average Total Dissolved Fe (', mu, 'mol/L)'))) +
  coord_flip() +
  facet_wrap(.~Waterbody2, ncol = 1, scale = "free") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16),
        legend.position = "N/A",
        legend.key = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank())

   ### ----  2)  Boxplots of DFe from subset 30 lakes  --------
all_2018 %>%
  ggplot(aes(y = avg_Fe, x = Month)) +
  geom_boxplot(aes(fill = "gray"), na.rm = TRUE) +
  #stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
  scale_fill_brewer(palette = "BrBG") +
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'g/L)'))) +
  #labs(x = 'Dissolved organic carbon (mg/L') +
  #coord_flip() +
  #facet_wrap(.~Month, ncol = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16, color = "black", hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, color = "black"),
        strip.text = element_text(size = 16),
        legend.position = "N/A",
        legend.key = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank())


#######################################################################
   #################    DESCRIPTIVE STATISTICS   #################

   ### ----  1) Summarise Fe in 130 lakes  ----------
ambient_avgFe %>%
  subset(Waterbody2 == "Artificial Lakes") %>%
  summarise(quant = quantile(TDFe, probs = c(0.25,0.5,0.75)),
            mean = mean(TDFe),
            min = min(TDFe))
             
   ### ----  2) Summarise subset of 30 lakes   ----------
avg.doc.2018<-all.2018 %>%
  na.omit() %>%
#ambient_avgFe <- ambient_Fe %>%
  select(c(7,19)) %>%
  group_by(Site2) %>%
  #mutate(mean = mean(avg_Fe))#%>%
  #ungroup()
  summarise(doc = mean(DOC))
write.csv(ambient_avgFe, "ambient_avgFe.csv")

all_2018 %>%
  subset(Week %in% c(1:15)) %>%
  na.omit() %>%
  group_by(Month) %>%
  summarise(quant = quantile(avg_Fe, probs = c(0.25,0.5,0.75)),
            mean = mean(avg_Fe),
            min = min(avg_Fe),
            max = max(avg_Fe))
 
sum(avg.doc.2018$doc < 6.67)

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


pivot.2018b %>%
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
  select(c(5,7,13)) %>%
  pivot_wider(names_from = "Week", values_from = "avg_Fe") 


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


   ### hclustering (like phylogenetic tree)
sing.hclust = hclust(Dmat,method="single")
myplclust(sing.hclust, labels=colnames(ncidat), lab.col=as.fumeric(colnames(ncidat)), main = "single linkage")
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

   ### Kruskal Willis test
   ### Is the range in Fe different between lakes?
   ### kruskal.test(dependent~independent)
kruskal.test(all.2018$avg_Fe~all.2018$Site2)
kruskal.test(ambient_Fe$TDFe~ambient_Fe$Waterbody2)
kruskal.test(ambient_Fe$TDFe~ambient_Fe$Site_Name)

   ### For pairwise comparison, number of observations must be the same
   ### Removing week 1 and 2 and other NA sites
all.2018b <-all.2018 %>%
  select(c(4,5,7,13)) %>%
  subset(!(Week %in% c(1,2)) & !(Site2 %in% "George Wyth")) %>%
  na.omit()


pair.res<-pairwise.wilcox.test(all.2018b$avg_Fe, all.2018b$Site2, paired = TRUE,
                     p.adjust.method = "BH")
write.csv(pair.res$p.value, "pairwise_results_2018.csv")
   ### Compare median between 2 groups
   ### Significant difference if pvalue < 0.05
wilcox.test(ambient_avgFe$mean~ ambient_avgFe$Waterbody2)
kruskal.test(lakes_fe$TDFe, lakes_fe$landform, correct=FALSE, na.rm = TRUE)


#########################################################################
   #########################   CLUSTERING   #########################

   ### Plot Linear Regression: Fe over time
   ### Smooth moethod = loess means locally weighted regression
all.2018$Site2<-factor(all.2018$Site2,
                       levels = c("Backbone","Beeds Lake","George Wyth",
                                  "Green Valley","Lower Pine Lake",
                                  "West Okoboji", "Clear Lake", "Nine Eagles",
                                  "Lake of Three Fires", "Black Hawk",
                                  "Red Haw", "Rock Creek","Lake Macbride",
                                  "Rathburn Lake","Lake Wapello","Lake Ahquabi",
                                  "Lake Anita","Viking Lake","Lacey Keosauqua",
                                  "Big Spirit","Lake Manawa","Lake Darling",
                                  "Lake Keomah","Prairie Rose","Union Grove",
                                  "Springbrook","North Twin","Brushy Creek",
                                  "Blue Lake","Big Creek"))

all.2018 %>%
  subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
  ggplot(aes(x=Date, y=avg_Fe)) + 
  geom_point(colour="black", size = 2) + 
  stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 3) + ## Turns on r value
  labs(x = 'Week',
       y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  facet_wrap(~Site2, scales = "free_y", ncol = 5) +
  theme_classic() +
  theme(axis.text.y.left = element_text(size=12, color = "black"), 
        axis.text.x.bottom = element_text(size=12, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size = 12))

clusters2 <- hclust(dist(pivot.2018[, 2:16]), method = "complete")
plot(clusters2)

   ### -----  1)  optimal number of k clusters "elbow method"  -------
   ### First omit an Null values
pivot.2018b<-pivot.2018%>%
  select(c(1,4,6:16)) %>%
  na.omit()
   ### Find k group
fviz_nbclust(pivot.2018b[,c(2:13)], kmeans, method = "wss") +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.title = element_text(size = 28))

   ### ----   2)  Run k-means cluster analysis   ----------
set.seed(123)
clust.res2 <- kmeans(pivot.2018b[,2:13], 7, nstart = 30)
print(clust.res)
   
   ### ----  3)  Plot kmeans results  ----------
   ### Plot using default function
fviz_cluster(clust.res2, data = pivot.2018b[,c(2:13)]) +
  scale_fill_brewer(palette = "Set2") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title.y = element_text(size = 13, color = "black"),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 13, color = "black"),
        axis.line = element_line(color = "black"))


   ### Plotting kmeans with ggplot
trythis<-stats::prcomp(pivot.2018b[,c(2:13)], scale = FALSE, center = FALSE)
state_scores<-as.data.frame(scores(trythis))
state_scores$cluster <- clust.res$cluster
state_scores$state <- pivot.2018b$Site2
head(state_scores)
state_scores$cluster=factor(state_scores$cluster, levels = c("1","2","3"))

chull(state_scores %>% filter(cluster ==1) %>% select(PC1, PC2))

grp.1 <- state_scores[state_scores$cluster == 1, ][chull(state_scores %>% filter(cluster ==1) %>% select(PC1, PC2) ), ]  # hull values for cluster 1
grp.2 <- state_scores[state_scores$cluster == 2, ][chull(state_scores %>% filter(cluster ==2) %>% select(PC1, PC2) ), ]  # hull values for cluster 2
grp.3 <- state_scores[state_scores$cluster == 3, ][chull(state_scores %>% filter(cluster ==3) %>% select(PC1, PC2) ), ]  # hull values for cluster 3
all_hulls <- rbind(grp.1,grp.2,grp.3)
head(all_hulls)


   ### Remove subset() if graphing all 3 groups
#state_scores %>%
  #subset(cluster =="1") %>%
ggplot(data = state_scores) + 
  geom_point(data = subset(state_scores, cluster == 3), aes(x = PC1, y = PC2, color = as.factor(cluster)),size = 3) +
  geom_text(data = subset(state_scores, cluster == 3), aes(x = PC1, y = PC2, color = as.factor(cluster), 
                label = state),size = 5,  hjust = -0.1, vjust = 0.5)  +
  geom_polygon(data = grp.3, 
               aes(x = PC1, y = PC2, fill = as.factor(cluster),  #as.factor(cluster)
                   colour =  NA), alpha = 0.25) + 
                   #colour =  as.factor(cluster)), alpha = 0.25) + 
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black"),
        legend.position = "none")


   ### ---- 4)  Re-cluster k-means output by group  ----------
pivot.group2<-group2 %>%
  na.omit() %>%
  select(c(5,7,13)) %>%
  pivot_wider(names_from = "Week", values_from = "avg_Fe") 
fviz_nbclust(pivot.group2[,c(4,6:16)], kmeans, method = "wss")

clust.res2 <- kmeans(pivot.group2[,c(4,6:16)], 3, nstart = 30)
print(clust.res2)
fviz_cluster(clust.res2, data = pivot.group2[,c(4,6:16)])
#######################################################################
   ###################    LINEAR MIXED MODEL   ###################

   ### ---- 1) Create groups based on k-means results ------
group1<-subset(all.2018, Site2 %in% c("Backbone", "Beeds Lake",
                                      "Lower Pine Lake", "West Okoboji",
                                      "Clear Lake"))
group2<-subset(all.2018, Site2 %in% c("Green Valley", "George Wyth",
                                      "Brushy Creek","Big Creek", "Big Spirit",
                                      "Black Hawk","North Twin","Springbrook",
                                      "Blue Lake","Lake Manawa", "Union Grove",
                                      "Prairie Rose","Nine Eagles","Lake Ahquabi",
                                      "Rock Creek","Lacey Keosauqua"))
group3<-subset(all.2018, Site2 %in% c("Lake Macbride","Lake Anita",
                                      "Lake Wapello", "Rathburn Lake",
                                      "Red Haw","Lake Keomah","Lake Darling",
                                      "Lake of Three Fires",
                                      "Viking Lake"))

   ### ---- 2) Run Linear Mixed Model Analysis --------
mob1<-lm(avg_Fe~pH+Temperature+DO+DOC+Turbidity, data = group1)
summary(mob1)

mob2<-lm(avg_Fe~pH+Temperature+DO+DOC+Turbidity, data = group2)
summary(mob2)

mob3<-lm(avg_Fe~pH+Temperature+DO+DOC+Turbidity, data = group3)
summary(mob3)

   ### linear mixed model
group1<-na.omit(group1)
mixed.lmer1 <- lmer(avg_Fe ~ Week + (1|DOC)+(1|Turbidity)+(1|DO) +(1|pH)+(1|Temperature), data = group1)
summary(mixed.lmer1)

mixed.lmer2 <- lmer(avg_Fe ~ DOC + pH + Temperature + DO + Turbidity + (1|DOC), data = group2)
summary(mixed.lmer2)

mixed.lmer3 <- lmer(avg_Fe ~ DOC + pH + Temperature + DO + Turbidity + (1|Week), data = group3)
summary(mixed.lmer3)

   ### Linear regression for week 1 to 8
wk1_8<-all.2018 %>%
  na.omit() %>%
  subset(Week >= 1 & Week <=8)
mixed.lmer2

#######################################################################
###################    LINEAR REGRESSION   #####################


   ### ----- 1)  Plot Linear Regression: Fe vs slope----------
   ### LR of Week vs Fe show decline Fe from Week 1-8
   ### Pulled slope from this LR 
   ### Run LR for slope vs lake volume

chase_slope %>%
  #subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
  ggplot(aes(x=reorder(Site2, slope), y=slope)) + 
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(stat = "identity", label = round(chase_slope$volume,0), nudge_y = -0.1) +
  #stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 3) + ## Turns on r value
  labs(y = expression(paste('Change in Fe (', mu, 'g/L) per week'))) +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  #facet_wrap(~Site2, scales = "free", ncol = 5) +
  theme_classic() +
  theme(axis.text.y = element_text(size=12, color = "black"), 
        axis.text.x = element_text(size=12, color = "black", 
                                   angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size = 12))
   

   ### ---- 2)  Plot Linear Regression: Fe vs. DOC  -----------
   ### This is for browning....

#group3 %>%
all_2018 %>%
  subset(Site == "Springbrook") %>%
  #subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
  ggplot(aes(y=DOC, x=Fe)) + 
  geom_point(stat="identity") +
  #geom_text(stat = "identity", label = round(chase_slope$volume,0), nudge_y = -0.1) +
  stat_smooth(method = 'lm', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 4) + ## Turns on r value
  labs(x = expression(paste('Dissolved Fe (', mu, 'g/L) per week')),
       y = 'Dissolved organic carbon (mg/L)') +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  facet_wrap(~Site2, scales = "free", ncol = 3) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size=14, color = "black"), 
        axis.text.x = element_text(size=14, color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size=14),
        strip.text = element_text(size = 14))

   ##############   PLOT: Fe Variability West Okoboji   ##############

   ### significance test: Is Fe different between sites?
kruskal.test(okoboji_sites$Fe, okoboji_sites$Site)
pairwise.wilcox.test(okoboji_sites$Fe, okoboji_sites$Site, paired = TRUE)

   ### Plot Fe over time & facet by site
okoboji_sites$Site<-factor(okoboji_sites$Site, levels = c("Triboji_Beach", "Pikes_Point_Beach",
                          "Gull_Point_Beach", "Emerson_Bay_Beach")) 
okoboji_sites %>%
  ggplot(aes(x = Date, y = Fe)) +
  geom_point(stat = "identity", size = 2) + 
  geom_path(stat = "identity", linetype = 2) +
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  facet_wrap(.~Site, ncol = 1) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title.y = element_text(size = 13, color = "black"),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 13, color = "black"),
        axis.line = element_line(color = "black"))
  
   ### ---- 3)  Plot Linear regression Fe vs time   -------------
all_2018 %>%
  subset(Site == "Springbrook") %>%
  #subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
  ggplot(aes(x=Date, y=Fe)) + 
  geom_point(stat="identity") +
  #geom_text(stat = "identity", label = round(chase_slope$volume,0), nudge_y = -0.1) +
  stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 4) + ## Turns on r value
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'g/L) '))) +
       #y = 'Dissolved organic carbon (mg/L)') +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  facet_wrap(~Site2, scales = "free", ncol = 3) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size=14, color = "black"), 
        axis.text.x = element_text(size=14, color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size=14),
        strip.text = element_text(size = 14))

   ### ---- 4)  k-means cluster: Fe trend line graph  -----------
   ###  Plot line graph Fe over time for each k-means group
   ###  Color each line by lake
#group1 %>%
#group2 %>%
group3 %>%
  subset(Site2 == "Lake Macbride") %>%
  ggplot(aes(x=Date, y=avg_Fe)) + 
  geom_point(stat="identity") +
  #geom_text(stat = "identity", label = round(chase_slope$volume,0), nudge_y = -0.1) +
  stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 4) + ## Turns on r value
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'g/L) '))) +
  #y = 'Dissolved organic carbon (mg/L)') +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  facet_wrap(~Site2, scales = "free", ncol = 3) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size=28, color = "black"), 
        axis.text.x = element_text(size=28, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=28),
        strip.text = element_text(size = 28))

#########################################################################
   ##############   AREA PLOt: Community composition   ##############
   ### Use BM_data_final
fire <- BM_data[grepl("2020", BM_data[["unique_ID"]]) | grepl("Three", BM_data[["Site"]]), ]

### Pivot df to make column for chla value and taxa
colnames(fire)[9] <- "PE_chla"  ## rename PE column to take uo "-" sign
fire.pivot <- fire %>%
  select(c(1:9)) %>%  ## grabs samples for week 6 to 15
  pivot_longer(6:9, names_to = "taxa",  
              values_to = "chla")
  ### Assign taxa levels
fire.pivot$taxa=factor(fire.pivot$taxa, levels = c("green_chla","brown_chla",
                                                   "PE_chla", "cyano_chla"))
                         #"'Green' group","'Brown' group","'Red' group","'Blue' group")

   ### Plot stacked area plot
ggplot(fire.pivot, aes(x=date, y=chla, fill=taxa)) + 
  geom_area() +
  scale_fill_manual(values = c("#66CC33","#CC9966","#CC3399","#00CCCC"),
                    labels = c("'Green' group", "'Brown' group",
                               "'Red' group", "'Blue' group"),
                    name = "Phytoplankton group") +
  labs(y = expression(paste('PhytoPAM Chl a (', mu, 'g/L)')),
    #y = expression(paste('Relative PhytoPAM Chl a (%)')),
    x = "Date") +
  facet_wrap(.~Site) +
  theme(panel.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 16),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

