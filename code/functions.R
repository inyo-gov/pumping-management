# line point wrangling ----
#' Title
#'
#' @param icwd_wide
#'
#' @return
#' @export
#'
#' @examples
pivot_longer_icwd <- function(icwd_wide){
  icwd_wide %>%
    gather(Transect, Cover, T1:T24) %>%
    filter(!is.na(Cover))%>%
    mutate(Transect = str_replace(Transect, "\\T",""),
           Transect = as.numeric(Transect))

# now that the data is in long format, filter out the NAs associated with 'trace species' and rows with zero hits which is a byproduct of having the data in wide format.
# zero transects prevent this from being adequate
}

# add attributes for simple output - annual data transfer to LADWP
#' Title
#'
#' @param long
#' @param cYear
#' @param species
#' @param entity
#'
#' @return
#' @export
#'
#' @examples
add_species_agency_plotid <- function(long,cYear,species,entity){
  long %>% left_join(species, join_by(Code == Code))%>%
    mutate(source = 'Joint Monitoring 2015-current year',
           source.abr = 'jm',
           Year = cYear,
           Entity = entity,
           plotid = paste(Parcel,Transect,source.abr,sep = '_'),
           plotid.full = paste(Parcel,Transect,source.abr,Entity,sep = '_'),
           Cover = as.numeric(Cover))%>%
    select(Parcel, Code, Transect, Cover, Year, Entity, plotid, Species, CommonName, Order, Family, Genus, Lifecycle, Lifeform, Veg_Type, source, source.abr, Phreatophyte, plotid.full) %>%
    arrange(Parcel, Transect, Code)
}

#
#' Title
#'
#' @param processed
#' @param cYear
#' @param entity
#'
#' @return
#' @export
#'
#' @examples
save_csv_and_return_path <- function(processed, cYear, entity) {
  processed %>% write_csv(paste0("output/",entity,"_lpt_",cYear,".csv"))
  return(paste0("output/",entity, "_lpt_",cYear,".csv"))
}

# this will update the data folder for next year's updates with a new
# uptodate master file.
save_master_csv_and_return_path <- function(processed, cYear, entity) {
  processed %>% write_csv(paste0("data/lpt_MASTER_",cYear,".csv"))
  return(paste0("data/lpt_MASTER_",cYear,".csv"))
}

# filter out invasive species LELA2 for transect summaries.
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
filt_lela <- function(data){
  data %>% filter(Species != "LELA2",Species !="LELA")#
}

# revert to single parcels name
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mult_to_single_parcel_name <- function(x){
  x$Parcel[x$Parcel=="TIN028_FSP022_FSP019"]<-"TIN028"
  x$Parcel[x$Parcel=="TIN028_FSP019_FSP022"]<-"TIN028"
  x$Parcel[x$Parcel=="MAN006_IND229"]<-"MAN006"
  x$Parcel[x$Parcel=="LAW137_PLC210"]<-"LAW137"
  x$Parcel[x$Parcel=="LAW108_FSL047"]<-"LAW108"
  x$Parcel[x$Parcel=="LAW109_FSL048"]<-"FSL048"
  x$Parcel[x$Parcel=="IND163_BEE017"]<-"IND163"
  x$Parcel[x$Parcel=="IND139_MAN005"]<-"IND139"
  x$Parcel[x$Parcel=="IND024_BLK103"]<-"IND024"
  x$Parcel[x$Parcel=="FSP004_BGP188"]<-"FSP004"
  x$Parcel[x$Parcel=="FSP006_BGP182"]<-"FSP006"
  x$Parcel[x$Parcel=="ABD012_BLK029"]<-"ABD012"
  x$Parcel[x$Parcel=="BLK002_TIN061"]<-"BLK002"
  x$Parcel[x$Parcel=="TIN061_BLK002"]<-"BLK002"
  x$Parcel[x$Parcel=="BIS055_FSL214"]<-"BIS055"
  x$Parcel[x$Parcel=="BIS025_FSL157"]<-"BIS025"
  x$Parcel[x$Parcel=="BIS026_FSL156"]<-"BIS026"
  x$Parcel[x$Parcel=="BLK002_TIN061"]<-"BLK002"


return(x)
}








# summarise cover types to transect, add wvcom values for some transects
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
summarise_to_transect <- function(x, y){
  tran.sums <- x %>% group_by(Parcel,Year,Transect,source.abr,plotid,Lifecycle,Lifeform)%>%
    summarise(Cover=sum(Cover))# summarise for each e.g. lifecycle/lifeform annual/perennial grass

  tran.tlc <- tran.sums %>%
    group_by(Parcel,Year,plotid) %>% # sum all cover across all lifecycle/lifeform
    summarise(tot.live.cover = sum(Cover))

  pft.wide<-tran.sums %>% spread(Lifeform,Cover)
  pft.wide[is.na(pft.wide)] <- 0

  # create total cover variable
  pft.wide <- pft.wide%>%
    mutate(Cover= Grass + Herb + Shrub + Herb_Shrub + Tree,
           Shrub = Shrub + Herb_Shrub + Tree)

  pft.wide.wtot.cov <- pft.wide %>% filter(Lifecycle=="Perennial")%>%
    left_join(tran.tlc, by = c("Parcel","Year","plotid"))

  bind_add_proportion <- bind_rows(pft.wide.wtot.cov, y) %>%
    mutate(pShrubTran = Shrub / Cover,
           pGrassTran = Grass / Cover,
           pHerbTran = Herb / Cover) %>%
    mutate_if(is.numeric, ~replace_na(., 0))

  return(bind_add_proportion)
}

# summarise from transects to parcels
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
summarise_to_parcel <- function(x){
  p <- x %>% group_by(Parcel,Year)%>% summarise(
    PerHits=sum(Cover),
    ShrubHits=sum(Shrub),
    HerbHits=sum(Herb),
    GrassHits=sum(Grass),
    Cover=mean(Cover),
    Shrub=mean(Shrub),
    Herb=mean(Herb),
    Grass=mean(Grass),
    TLC=mean(tot.live.cover),

    pShrub=mean(pShrubTran),
    pGrass=mean(pGrassTran),
    pHerb=mean(pHerbTran),
    n.transects = n()) %>%
    mutate(NominalYear = case_when(Year %in% c(1985, 1986, 1987) ~ 1986,
                                   !Year %in% c(1985, 1986, 1987) ~ Year))

return(p)
}


n_tran_per_parcel <- function(lpt_data){

   x <- lpt_data %>% distinct(Parcel,Year,Transect) %>%
     arrange(Parcel,Year,Transect) %>%
     group_by(Parcel, Year) %>%
    summarise(nTransect= n())


  return(x)
}



# add deltas baseline to each year
#' Title
#'
#' @param parcels
#'
#' @return
#' @export
#'
#' @examples
add_parcel_deltas <- function(parcels){

  p <- parcels
  for (PID in unique(p$Parcel)) {
  #defines baselineRow variable as parcels with Nominal year as 1986
  baselineRow <- p[p$Parcel==PID & p$NominalYear==1986, ]

  #dim() function gets or sets the dimension of a matrix, array or data frame.
  #so the dim function asks if the baselineRow for the current parcel in the loop
  #has been updated with the baseline year. If it hasn't
  if (dim(baselineRow)[1]==0) next


  otherYears <- (p$Parcel==PID & !(p$NominalYear==1986))

  # calculate deltas by subtracting baseline cover from each of
  # other rows, leaving negative values indicating declines from baseline
  p$Cover.Delta[otherYears] <-
    p$Cover[otherYears] - baselineRow$Cover

  p$Shrub.Delta[otherYears] <-
    p$Shrub[otherYears] - baselineRow$Shrub

  p$Herb.Delta[otherYears] <-
    p$Herb[otherYears] - baselineRow$Herb

  p$Grass.Delta[otherYears] <-
    p$Grass[otherYears] - baselineRow$Grass
  }
  return(p)
}


add_parcel_deltas_yoy <- function(parcels, cYear){

  p <- parcels %>% filter(NominalYear == cYear | NominalYear == cYear -1)
  for (PID in unique(p$Parcel)) {
    #defines baselineRow variable as parcels with Nominal year as 1986
    baselineRow <- p[p$Parcel==PID & p$NominalYear== cYear - 1, ]

    #dim() function gets or sets the dimension of a matrix, array or data frame.
    #so the dim function asks if the baselineRow for the current parcel in the loop
    #has been updated with the baseline year. If it hasn't
    if (dim(baselineRow)[1]==0) next


    otherYears <- (p$Parcel==PID & !(p$NominalYear== cYear - 1))

    # calculate deltas by subtracting baseline cover from each of
    # other rows, leaving negative values indicating declines from baseline
    p$Cover.Delta[otherYears] <-
      p$Cover[otherYears] - baselineRow$Cover

    p$Shrub.Delta[otherYears] <-
      p$Shrub[otherYears] - baselineRow$Shrub

    p$Herb.Delta[otherYears] <-
      p$Herb[otherYears] - baselineRow$Herb

    p$Grass.Delta[otherYears] <-
      p$Grass[otherYears] - baselineRow$Grass
  }
  return(p)
}

# summary line point effort ----
count_parcels_all_years <- function(lpt_updated_master){
  lpt_updated_master %>%
    group_by(Year) %>%
    summarise(n = n_distinct(Parcel))
}

count_parcels_cyear <- function(n_parcels_all_years,cYear){
  n_parcels_sampled_cYear <- n_parcels_all_years %>%
    filter(Year == cYear)
  n_parcels_sampled <- n_parcels_sampled_cYear$n

  return(n_parcels_sampled)
}

count_transects_cyear <- function(lpt_updated_master,cYear){
  cyr_transects <- lpt_updated_master %>% filter(Year == cYear)%>%
    summarise(n = n_distinct(plotid))

  n_transects <- cyr_transects$n
  return(n_transects)
}

# wellfield control----
#' Title
#'
#' @param parcels_deltas
#' @param attributes
#'
#' @return
#' @export
#'
#' @examples
wellfield_control_means <- function(parcels_deltas, attributes){

  Parcels <- attributes %>% select(Parcel, Type) %>% left_join(parcels_deltas, by = "Parcel")

  Parcels %>% group_by(Type, NominalYear) %>%
    dplyr::summarize(
      count=n(),
      Cover=mean(Cover),
      Grass=mean(Grass),
      Herb=mean(Herb),
      Shrub=mean(Shrub)
      )
}



compute_trend_well_cont <- function(wellcont_wide){
  lmmod <- y ~ x

  wellcont_wide$Type[wellcont_wide$Type=="C"]<-"Control"
  wellcont_wide$Type[wellcont_wide$Type=="W"]<-"Wellfield"

  plot <- wellcont_wide %>%
    pivot_longer(Cover:Shrub, names_to = "Cover.Type", values_to = "Cover") %>%
    filter(!Cover.Type %in% c("Herb")) %>%
    ggplot(aes(x = NominalYear, y = Cover, color = Type))+
    geom_point()+
    geom_line()+
    geom_smooth(method='lm',se=FALSE,formula = lmmod)+
    xlab("Year") +
    ylab("Cover (0-100)") +
    # stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~~")), formula = lmmod, parse = TRUE,label.y.npc = 0.1) +
    stat_fit_glance(method = "lm",
                    method.args = list(formula = lmmod),
                    # label.x.npc = .1,
                    # label.y.npc = 1,
                    label.x=1989,
                    label.y=40,
                    geom = "text",
                    aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = "")))+
    facet_grid(Cover.Type~Type)+
    theme(legend.position="none")

  return(plot)
}

boxplot_well_cont <- function(parcels, attributes_pfix,cYear){
  # my_comparisons <- list(c(1986, cYear))

  # join attributes
  a <- attributes_pfix %>% select(Parcel,Type)
  parcels <- parcels %>% left_join(a, by = "Parcel")

  # get the parcels with both baseline and current year data
  # spreading separate years into columns provides NA in parcel rows without data
  # in that year.
  parcels.select.compare <- parcels %>%
    select(Parcel,NominalYear,Cover) %>%
    filter(NominalYear %in% c("1986",cYear))
    # filter(NominalYear %in% c("1986","1995","2000","2005","2010","2015","2020","2023"))
  parcels.spread <- parcels.select.compare %>%
    pivot_wider(names_from = NominalYear,
                values_from=Cover,
                names_prefix = "y")

  cYear_col <- paste0("y",cYear)
  Pairwise <- parcels.spread %>%
  filter(!is.na(cYear_col) &  !is.na(y1986))
  pairwise_cyear <- Pairwise %>%
  select(Parcel)


 plot <-  parcels %>% filter(Parcel %in% Pairwise$Parcel,
                             NominalYear %in% c("1986",cYear)) %>%
   select(Parcel,NominalYear,Year,Type,Cover,Shrub,Grass,TLC) %>%
   gather(Cover.Type,Cover,Cover:TLC)%>%
   filter(Cover.Type != 'TLC', !is.na(Type)) %>%
   ggplot(aes(x = Type, y = Cover, color = as.factor(NominalYear)))+
   geom_boxplot()+
   facet_wrap(~Cover.Type) +
   stat_compare_means(aes(group = NominalYear),
                      method='t.test',
                      paired = FALSE,
                      label = "p.signif",
                      label.y = c(80))
 return(plot)
 }

boxplot_wc_pft_year_statcompare <- function(attributes, parcels, comparison_year, reference_year){

  # attributes <-  attributes_pfix
  # comparison_year <-  "2022"
  # reference_year <-  "1986"

  a <- attributes %>% select(Parcel,Type) %>% unique()


  parcels2 <- parcels %>% unique() %>% left_join(a, by = "Parcel")

  # select columns and filter to comparison years
  parcels.select.compare <- parcels2 %>%
    select(Parcel,NominalYear,Cover) %>%
    filter(NominalYear %in% c(reference_year,comparison_year))

  # pivot wide with years as columns so we can keep rows with data in both current year and baseline.
  parcels.spread <- parcels.select.compare %>%
    pivot_wider(names_from = NominalYear,
                values_from = Cover,
                names_prefix = "y")

  # select only rows with baseline and current year data
  Pairwise <- parcels.spread %>%
    rowwise()%>%
    filter(!any(is.na(c_across(starts_with("y")))))

  # select parcel attributes to join
  att <- attributes %>% select(Parcel, Type, wellfield, GB_TYPE, Holland)

  # join attributes
  pa <- Pairwise %>% left_join(att, by = "Parcel")

  # split into wellfield and control dataframes
  # in 2022, 132 total, 91 wellfield, 41 control
  wpa <- pa %>% filter(Type == 'W') %>% nrow()# 91
  cpa <- pa %>% filter(Type == 'C') %>% nrow()# 41

  # now filter the data
  title.construct <- paste0("Group Comparison of Control (n=", cpa,") and Wellfield (n=", wpa,") Parcels \\\n sampled in both ",
                            reference_year," and ",comparison_year)

  p <-  parcels %>% unique() %>%
    filter(Parcel %in% Pairwise$Parcel,NominalYear %in% c(reference_year,comparison_year)) %>%
    select(Parcel,NominalYear,Year,Type,Cover,Shrub,Grass) %>%
    gather(Cover.Type,Cover,Cover:Grass)%>%
    filter(!is.na(Type)) %>% mutate(NomYear = as.factor(NominalYear))

  ggpaired_component <-ggpaired(p,
                                x = "NomYear",
                                y = "Cover",
                                id = 'Parcel',
                                color = "Type",
                                notch = TRUE,
                                label = "Parcel",
                                label.select = list(top.up = 2),
                                # label.select = list(criteria = "`y` > 2 & `y` < 5 & `x` %in% c('A', 'B')")
                                repel = TRUE,
                                font.label = list(size = 9, face = "plain"),
                                palette = "jco",
                                line.color = "gray",
                                line.size = 0.1,
                                # add = "jitter",
                                facet.by = c('Type','Cover.Type'),
                                # short.panel.labs = FALSE,
                                title = title.construct,
                                ylab = "Perennial Cover",
                                xlab = "Time Period Statistical Comparison",
                                panel.labs = list(Type = c("Control", "Wellfield"))
  )

  ggpaired_stat_compare_plot <- ggpaired_component +
    stat_compare_means(method='t.test',
                       paired = TRUE,
                       label = "p.signif"
                       # label = "p.format"
    )

  return(ggpaired_stat_compare_plot)
}


boxplot_wc_pft_timeseries_statcompare <- function(attributes, parcels){

  # attributes <-  attributes_pfix
  # comparison_year <-  "2022"
  # reference_year <-  "1986"

  a <- attributes %>% select(Parcel,Type) %>% unique()


  parcels2 <- parcels %>% unique() %>% left_join(a, by = "Parcel")

  # parcels %>% write_csv('parcels2022.csv')
  # get the parcels with both baseline and current year data
  # spreading separate years into columns provides NA in parcel rows without data
  # in that year.

  # select columns and filter to comparison years
  parcels.select.compare <- parcels2 %>%
    select(Parcel,NominalYear,Cover)
  # %>%
    # filter(NominalYear %in% c(reference_year,comparison_year))

  # pivot wide with years as columns so we can keep rows with data in both current year and baseline.
  parcels.spread <- parcels.select.compare %>%
    pivot_wider(names_from = NominalYear,
                values_from = Cover,
                names_prefix = "y")

  # select only rows with baseline and current year data
  Pairwise <- parcels.spread %>%
    rowwise()%>%
    filter(!any(is.na(c_across(starts_with("y")))))

  # select parcel attributes to join
  att <- attributes %>% select(Parcel, Type, wellfield, GB_TYPE, Holland)

  # join attributes
  pa <- Pairwise %>% left_join(att, by = "Parcel")

  # split into wellfield and control dataframes
  # in 2022, 132 total, 91 wellfield, 41 control
  wpa <- pa %>% filter(Type == 'W') %>% nrow()# 91
  cpa <- pa %>% filter(Type == 'C') %>% nrow()# 41
  # write to disk for excel users
  # pa %>% write_csv("output/wellfield_control_cover_2022_c41_w91.csv")
  #-------------------------------------------------------------
  #-------------------------------------------------------------
  # now filter the data
  title.construct <- paste0("Group Comparison of Control (n=", cpa,") and Wellfield (n=", wpa,") Parcels.")

  p <-  parcels %>% unique() %>%
    filter(Parcel %in% Pairwise$Parcel) %>%
    select(Parcel,NominalYear,Year,Type,Cover,Shrub,Grass) %>%
    gather(Cover.Type,Cover,Cover:Grass)%>%
    filter(!is.na(Type)) %>% mutate(NomYear = as.factor(NominalYear))

  ggpaired_component <-ggpaired(p,
                                x = "NomYear",
                                y = "Cover",
                                id = 'Parcel',
                                color = "Type",
                                notch = TRUE,
                                label = "Parcel",
                                label.select = list(top.up = 2),
                                # label.select = list(criteria = "`y` > 2 & `y` < 5 & `x` %in% c('A', 'B')")
                                repel = TRUE,
                                font.label = list(size = 9, face = "plain"),
                                palette = "jco",
                                line.color = "gray",
                                line.size = 0.1,
                                # add = "jitter",
                                facet.by = c('Type','Cover.Type'),
                                # short.panel.labs = FALSE,
                                title = title.construct,
                                ylab = "Perennial Cover",
                                xlab = "Time Period Statistical Comparison",
                                panel.labs = list(Type = c("Control", "Wellfield"))
  )

  ggpaired_stat_compare_plot <- ggpaired_component +
    stat_compare_means(method='t.test',
                       paired = TRUE,
                       label = "p.signif"
                       # label = "p.format"
    )
  # ggpaired_stat_compare_plot %>%
  # ggsave(paste0("output/wc-plots/wc_",reference_year,"v",comparison_year,".pdf"))
  # default is wilcoxon signed rank test
  # statcompare+ stat_compare_means(
  #                    paired = TRUE
  #     )
  # plot
  return(ggpaired_stat_compare_plot)
}



wellfield_control_means_rarefied <- function(parcels_deltas, attributes) {
  Parcels <- attributes %>%
    select(Parcel, Type) %>%
    left_join(parcels_deltas, by = "Parcel")

  Parcels %>%
    filter(Parcel %in% c(
      "BGP031",
      "BLK115",
      "FSL187",
      "IND096",
      "IND163",
      "LNP018",
      "MAN060",
      "PLC024",
      "PLC106",
      "PLC121",
      "PLC223",
      "UNW029",
      "UNW039",
      "BGP154",
      "BGP162",
      "BLK009",
      "BLK016",
      "BLK024",
      "BLK033",
      "BLK039",
      "BLK044",
      "BLK069",
      "BLK074",
      "BLK075",
      "BLK094",
      "BLK099",
      "IND011",
      "IND035",
      "IND106",
      "IND111",
      "IND132",
      "IND139",
      "IND231",
      "LAW063",
      "LAW065",
      "LAW085",
      "LAW107",
      "LAW120",
      "LAW122",
      "MAN006",
      "MAN007",
      "MAN037",
      "TIN028",
      "TIN068"
    )) %>%
    group_by(Type, NominalYear) %>%
    dplyr::summarize(
      count = n(),
      Cover = mean(Cover),
      Grass = mean(Grass),
      Herb = mean(Herb),
      Shrub = mean(Shrub)
    )
}


plot_wellfield_control <- function(wellcont_means_rarefied) {
  plot <-
    wellcont_means_rarefied %>%
    select(-Herb) %>%
    pivot_longer(Cover:Shrub, names_to = "type", values_to = "cover") %>%
    ggplot(aes(x = NominalYear, y = cover, color = Type)) +
    geom_point() +
    geom_line() +
    facet_wrap(~type, ncol = 2)

  # ggsave("wellfield_control_rarefied.png", plot, width = 7, height = 7)
  # return("wellfield_control_rarefied.png")
  return(plot)
}

#' Title
#'
#' @param dtw
#'
#' @return
#' @export
#'
#' @examples
#' # this functino per parcel -  needs to be embedded into loop

# time series plot functions----
plot.dtw <- function(PID, dtw) {

  # Extract the parcel of interest.

  pcldtw <- dtw %>%
    filter(Parcel == PID) %>%
    arrange(Year)
  pcldtw <- pcldtw %>% mutate(phreatic.zone = MIN - 3)
  n <- dim(pcldtw)[1]

  # Find the maximum value that DTW attained over the time span to
  # be plotted.  Allow for missing data.  Set up the plot limits.
  # specifying the max first, then min in ylim sets up the reverse scale we want for DTW plotting where 0 is at top or soil surface representation.

  # if (is.na(pcldtw$DTW)) {
  #   ylim <- c(15, 0)
  # } else {
  #   this.max.DTW <- max(pcldtw$DTW, na.rm = TRUE) + 1
  #   # this.min.DTW <- 0)
  #   ylim <- c(this.max.DTW, 0)
  # }
  this.max.DTW <- max(pcldtw$DTW, na.rm = TRUE) + 1
  this.min.DTW <- min(pcldtw$DTW, na.rm = TRUE) - 1
  # ylim <- c(15,0)
  ylim <- c(this.max.DTW,min(this.min.DTW,0))
  # ylim <- c(this.max.DTW,this.min.DTW)
  plot(xlim, ylim, xlab = "", ylab = "DTW BGS (feet)", xlim = xlim, ylim = ylim, yaxs = "i", type = "n", axes = F) # Draw solid lines at important depth points.

  # could control ylim in global chunk but if so, need to change above plot function and below axis setup
  # add the axes and a frame.
  axis(side = 1, at = seq(xlim[1], xlim[2]), labels = FALSE, tcl = -0.2)
  axis(side = 1, at = pretty(xlim))
  axis(side = 2, at = pretty(ylim), las = 2)
  abline(h = pretty(ylim), col = "lightgray")
  box()


  # abline(h = 5, lwd = 1, col = "blue")
  # text(rmarg, 6, 'D/E', adj=0, xpd=NA)


  abline(h = 6, lwd = 1, col = "darkgreen")
  text(rmarg, 7, 'C', adj=0, xpd=NA)

  abline(h = 12, lwd = 1, col = "lightgreen")
  text(rmarg, 13, 'B', adj=0, xpd=NA)

  # abline(h = 18, lwd = 1, col = "yellowgreen")
  # text(rmarg, 18, 'A', adj=0, xpd=NA)

  # text(rmarg, stats$Mean[1], 'Baseline', adj=0, xpd=NA)

  # abline(h=Parcel$DTW[1], lwd=1, col='blue')
  # text(rmarg, Parcel$DTW[1], '1985 DTW', adj=0, xpd=NA)


  # Assess reliability and present data only in cases where
  # the data are reliable or relative recovery reliable.
  # if (as.character(Attribute$DTW.Reliability) %in% c('Reliable', 'Relative Recovery Reliable' , 'Baseline Not Reliable', 'Current DTW Not Reliable','Current DTW Reliable','Need hydrograph Evaluation','NoData','Not Reliable','Baseline Reliable','Validation Needed)) {

  # Draw in a lines and points graph.
  lines(pcldtw$Year, pcldtw$DTW, type = "l", pch = 8, col = "blue")
  # lines(pcldtw$Year, pcldtw$DTW - 3, type = "l", pch = 5, col = "lightblue")
  # lines(Parcel$Year, Parcel$DTWcap1, type='l', pch=16, col="lightblue")
  # lines(pcldtw$Year, pcldtw$MIN, type='b', pch=16, col="darkblue")
  # lines(pcldtw$Year, pcldtw$MAX, type='b', pch=16, col="grey")
  # lines(pcldtw$Year, pcldtw$phreatic.zone, type='b', pch=16, col="blue")
  # } else {
  #   # Draw nothing.
  # }
  # Indicate the DTW reliability.
  # text(
  #   xlim[1] + (xlim[2] - xlim[1])*dtw.xy[1],
  #   dtw.ylim[1] * dtw.xy[2],
  #   Attribute$DTW.Reliability, adj=1, family='sans', font=4, cex=1.25
  # )
}


#' Title
#'
#' @param PID
#'
#' @return
#' @export
#'
#' @examples
plot.ndvi <- function(PID, rs) {

  # Extract the parcel of interest.
  #Parcel    <- subset(Covariates, Parcel==PID)
  rspcl <- rs %>% filter(Parcel == PID)


  # Parcel    <- subset(Covariates2, Parcel==PID)
  # Attribute <- subset(Attributes, Parcel==PID)

  # We are only plotting one number per year here.  Get the number of
  # years to plot.

  n <- dim(rspcl)[1]

  # YLIM
  # Check the y-axis limits.  Extend them if necessary.
  ymx <- max(rspcl$NDVI_SUR) + .01
  ymn <- min(rspcl$NDVI_SUR) - .01
  # ymin <- 0
  # ylim <- c(0, .6)
  ylim <- c(ymn, ymx)
  # ylim <- c(0, ymx)
  # ylim <- c(0,.6)

  # if (max(Parcel$NDVI_SUR, na.rm=TRUE) > -1.5) { ylim <- c(ymin, ym) }
  #
  # # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='NDVI [dimensionless]', xlim=xlim, ylim=ylim, type='n', yaxs='i', axes=F)

  # Jazz it up with pretty axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(rspcl$Year[i]-bar.space, 2), rep(rspcl$Year[i]+bar.space, 2))
    bar.y <- c(0, rspcl$NDVI_SUR[i], rspcl$NDVI_SUR[i], 0)

    polygon(bar.x, bar.y, col='grey')
  }

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.
  # Only do this if the data will be analyzed using Dunnett's method.

  bl <- rspcl %>% filter(Year %in% c("1984","1985","1986","1987"))

  # NdviMeanBl <- mean(bl$NDVI_SUR)
  # ndviMean <- mean(rspcl$NDVI_SUR)
  text(rmarg, mean(bl$NDVI_SUR), 'Baseline NDVI', adj=0, xpd=NA)
  abline(h=mean(bl$NDVI_SUR), lwd=1, col ='red')

  # text(rmarg, mean(rspcl$NDVI_SUR), 'mean NDVI', adj=0, xpd=NA)
  # abline(h=mean(rspcl$NDVI_SUR), lwd=1, col ='green')

  text(rmarg, median(rspcl$NDVI_SUR), 'median NDVI', adj=0, xpd=NA)
  abline(h=median(rspcl$NDVI_SUR), lwd=1, col ='lightgreen')

  text(rmarg, bl$Mean[1], 'Baseline NDVI', adj=0, xpd=NA)
  # abline(h=IQR(rspcl$NDVI_SUR), lwd=1, col ='purple')
  #text(rmarg, Parcel$NDVI_SUR[1], 'Baseline', adj=0, xpd=NA)


}



#' Title
#'
#' @param PID
#'
#' @return
#' @export
#'
#' @examples
plot.ppt <- function(PID, rs) {

  # Extract the parcel of interest.
  #Parcel    <- subset(Covariates, Parcel==PID)
  ppt <- rs %>% filter(Parcel == PID)


  # Parcel    <- subset(Covariates2, Parcel==PID)
  # Attribute <- subset(Attributes, Parcel==PID)

  # We are only plotting one number per year here.  Get the number of
  # years to plot.

  n <- dim(ppt)[1]

  # HARD-CODED NUMBER:
  # Check the y-axis limits.  Extend them if necessary.

  ymxppt <- max(ppt$PPT) + 10
  ymnppt <- min(ppt$PPT) - 10

  # ylims are defined in setup chunk
  ylim <- c(ymnppt,ymxppt)
  # ylim <- c(0,400)


  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='PPT [mm]', xlim=xlim, ylim= ylim, type='n', yaxs='i', axes=F)

  # Jazz it up with pretty axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  abline(h=mean(ppt$PPT), lwd=1, col ='lightblue')
  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(ppt$Year[i]-bar.space, 2), rep(ppt$Year[i]+bar.space, 2))
    bar.y <- c(0, ppt$PPT[i], ppt$PPT[i], 0)

    polygon(bar.x, bar.y, col='lightblue')
  }
  # abline(h=Parcel$PPT[1], lwd=1,col='blue')

  #text(rmarg, Parcel$PPT[1], 'Baseline', adj=0, xpd=NA)
}

# plot.perennial.cover()
#
# Function to create the perennial cover plot.

#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.shrubcover <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }

  # Extract the parcel of interest.

  Transect <- subset(transects, Parcel==PID)
  # Attribute3 <- subset(Attributes3, Parcel==PID)

  # Calculate mean and standard error of the mean for each year.

  # Cover.mean <- aggregate(Transect$Cover, list(Year=Transect$Year), FUN=mean)
  # colnames(Cover.mean) <- c('Year', 'Mean')
  # Grass.mean <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Grass))

  Shrub.mean <- Transect %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Shrub))


  # Cover.stderr <- aggregate(Transect$Cover, list(Year=Transect$Year), FUN=stderr)
  # colnames(Cover.stderr) <- c('Year', 'SEM')

  Shrub.stderr <- Transect  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Shrub))

  stats <- Shrub.mean%>%left_join(Shrub.stderr, by="Year") %>% arrange(Year)

  # stats <- merge(Cover.mean, Cover.stderr, by="Year") %>% arrange(Year)


  n <- dim(stats)[1]

  # Check the y-axis limits.  Extend them if necessary.
  this.max.shrub <- max(stats$Mean, na.rm=TRUE)+2
  this.min.shrub <- min(stats$Mean, na.rm=TRUE)-1
  ylim <- c(-1, this.max.shrub)
  # ylim <- c(this.min.shrub, this.max.shrub)
  # ylim <- c(0, 60)
  #ylim <- c(0, 104)

  #if (max(stats$Mean + stats$SEM + asterisk.offset, na.rm=TRUE) > 60) { ylim <- c(0, 104) }

  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Shrub Cover [%]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.
  # Only do this if the data will be analyzed using Dunnett's method.

  abline(h=stats$Mean[1], lwd=1,col='red')

  text(rmarg, stats$Mean[1], 'Baseline Shrub', adj=0, xpd=NA)


  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='yellowgreen')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 95% CI or 1.96 * standard error of the mean using the usual graphics.

    # lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1.96*(stats$SEM[i]), stats$Mean[i]+1.96*(stats$SEM[i])))
    # lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1.96*(stats$SEM[i]),2))
    # lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1.96*(stats$SEM[i]),2))

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1*(stats$SEM[i]), stats$Mean[i]+1*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1*(stats$SEM[i]),2))
  }

  # Now, rather than performing the one-way analysis of variance (weighted) followed
  # by Dunnett's method, we will simply perform a two-sample t-test with unequal
  # variances.  Note that the weighted analysis of variance is essentially
  # a generalization of this approach.

  ## remove this analyzable dependency to force in one sample tests
  #if (analyzable) {

  # We want to pick off the first year.  For safety's sake, we
  # sort the data to make sure they are in order, although
  # that seems to be a side effect of the code above.

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  # Just roll through the other years one by one.  This could probably
  # be done more compactly.  On the other hand, it should be clear
  # what is happening here.

  #  TO DO:  What *would* be better would be cleaning
  # up this clunky method of getting the years.  It would be best
  # to have a list (vector) containing the years to be analyzed.

  # periodically the error
  # Error in t.test.default(data2, mu = mu.d1) : 'mu' must be a single number


  for (i in 2:n) {

    # data1 <- Transect$Cover[Transect$Year==stats$Year[1]]#baseline
    # data1 <- Transect$Cover[Transect$Year==1986]#baseline
    data1 <- Transect$Shrub[Transect$Year < 1989]
    # data1 <- Transect$Cover[Transect$Year %in% c(1984,1985,1986,1987,1988)]

    mu.d1<-mean(data1)

    data2 <- Transect$Shrub[Transect$Year==stats$Year[i]]#all other years in time series

    # per the greenbook update in 2017, I use the two sample t-test for parcels with greater than four transects and non-zero variance; and the one sample t-test with mu = mean of small sample or the live cover value reduced to perennial only from the wvcom variable.

    ifelse (length(data1) <= 4,#if parcel has less than five transects or was assigned a single wvcomm live cover variable as the baseline cover, conduct a one sample t-test on reduced live cover based on proportion of perennial grass and perennial total cover assigned to the parcel.

            fit <- t.test(data2, mu = mu.d1),
            fit <- t.test(data1,data2)# otherwise conduct the two sample t-test, defaulting to welch-satherwaite method to account for unequal variance.
    )

    pvalues[i] <- fit$p.value
  }

  # Add an asterisk according to whether the comparison with
  # baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      # text(stats$Year[i], stats$Mean[i]+2.0*(stats$SEM[i])+ asterisk.offset, '*', adj=.6)
      text(stats$Year[i], stats$Mean[i]+1.0+ asterisk.offset, '*', adj=.6)
    }
  }

}


# plot.perennial.cover()
#
# Function to create the perennial cover plot.

#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.cover <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }

  # Extract the parcel of interest.

  Transect <- subset(transects, Parcel==PID)
  # Attribute3 <- subset(Attributes3, Parcel==PID)

  # Calculate mean and standard error of the mean for each year.

  # Cover.mean <- aggregate(Transect$Cover, list(Year=Transect$Year), FUN=mean)
  # colnames(Cover.mean) <- c('Year', 'Mean')

  Cover.mean <- Transect %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Cover))

  # Cover.stderr <- aggregate(Transect$Cover, list(Year=Transect$Year), FUN=stderr)
  # colnames(Cover.stderr) <- c('Year', 'SEM')

  Cover.stderr <- Transect  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Cover))

  stats <- Cover.mean%>%left_join(Cover.stderr, by="Year") %>% arrange(Year)

  # stats <- merge(Cover.mean, Cover.stderr, by="Year") %>% arrange(Year)


  n <- dim(stats)[1]

  # Check the y-axis limits.  Extend them if necessary.
  this.max.cover <- max(stats$Mean, na.rm=TRUE)+2
  this.min.cover <- min(stats$Mean, na.rm=TRUE)-1

  ylim <- c(this.min.cover, this.max.cover)
  # ylim <- c(0, 60)
  #ylim <- c(0, 104)

  #if (max(stats$Mean + stats$SEM + asterisk.offset, na.rm=TRUE) > 60) { ylim <- c(0, 104) }

  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Perennial Cover [%]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.
  # Only do this if the data will be analyzed using Dunnett's method.

  abline(h=stats$Mean[1], lwd=1,col='blue')

  # text(rmarg, stats$Mean[1], 'Baseline', adj=0, xpd=NA)


  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='brown')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 95% CI or 1.96 * standard error of the mean using the usual graphics.

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1.96*(stats$SEM[i]), stats$Mean[i]+1.96*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1.96*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1.96*(stats$SEM[i]),2))

  }

  # Now, rather than performing the one-way analysis of variance (weighted) followed
  # by Dunnett's method, we will simply perform a two-sample t-test with unequal
  # variances.  Note that the weighted analysis of variance is essentially
  # a generalization of this approach.

  ## remove this analyzable dependency to force in one sample tests
  #if (analyzable) {

  # We want to pick off the first year.  For safety's sake, we
  # sort the data to make sure they are in order, although
  # that seems to be a side effect of the code above.

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  # Just roll through the other years one by one.  This could probably
  # be done more compactly.  On the other hand, it should be clear
  # what is happening here.

  #  TO DO:  What *would* be better would be cleaning
  # up this clunky method of getting the years.  It would be best
  # to have a list (vector) containing the years to be analyzed.

  # periodically the error
  # Error in t.test.default(data2, mu = mu.d1) : 'mu' must be a single number


  for (i in 2:n) {

    # data1 <- Transect$Cover[Transect$Year==stats$Year[1]]#baseline
    # data1 <- Transect$Cover[Transect$Year==1986]#baseline
    data1 <- Transect$Cover[Transect$Year < 1989]
    # data1 <- Transect$Cover[Transect$Year %in% c(1984,1985,1986,1987,1988)]

    mu.d1<-mean(data1)

    data2 <- Transect$Cover[Transect$Year==stats$Year[i]]#all other years in time series

    # per the greenbook update in 2017, I use the two sample t-test for parcels with greater than four transects and non-zero variance; and the one sample t-test with mu = mean of small sample or the live cover value reduced to perennial only from the wvcom variable.

    ifelse (length(data1) <= 4,#if parcel has less than five transects or was assigned a single wvcomm live cover variable as the baseline cover, conduct a one sample t-test on reduced live cover based on proportion of perennial grass and perennial total cover assigned to the parcel.

            fit <- t.test(data2, mu = mu.d1),
            fit <- t.test(data1,data2)# otherwise conduct the two sample t-test, defaulting to welch-satherwaite method to account for unequal variance.
    )

    pvalues[i] <- fit$p.value
  }

  # Add an asterisk according to whether the comparison with
  # baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      text(stats$Year[i], stats$Mean[i]+2.0*(stats$SEM[i])+ asterisk.offset, '*', adj=.6)
    }
  }

}



# plot.perennial.cover()
## Function to create the perennial cover plot.
#PID<-"BLK044"
#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.grass <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }
  # Extract the parcel of interest.

  grass.pcl <- subset(transects, Parcel==PID)
  # Attribute3 <- subset(Attributes3, Parcel==PID)

  # Calculate mean and standard error of the mean for each year.

  # rewrite old code using dplyr pipes. reads more logically

  #old way
  #Grass.mean <- aggregate(Transect$Grass, list(Year=Transect$Year), FUN=mean)
  #colnames(Grass.mean) <- c('Year', 'Mean')

  #new
  Grass.mean <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Grass))


  # Grass.stderr <- aggregate(Transect$Grass, list(Year=Transect$Year), FUN=stderr)
  # colnames(Grass.stderr) <- c('Year', 'SEM')

  Grass.stderr <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Grass))


  stats <- Grass.mean%>%left_join(Grass.stderr, by="Year") %>% arrange(Year)


  # Get the number of years to plot.
  n <- dim(stats)[1]

  # this.max.cover <- max(stats$Mean, na.rm=TRUE)
  # this.min.cover <- min(stats$Mean, na.rm=TRUE)

  this.max.grass <- max(stats$Mean, na.rm=TRUE)+2
  this.min.grass <- min(stats$Mean, na.rm=TRUE)-1

  #ylim is finite

  # ylim <- c(0, 60)
  # ylim <- c(this.min.grass, this.max.grass)
  ylim <- c(-1, this.max.grass)

  # Check the y-axis limits.  Extend them if necessary.

  # if (max(stats$Mean + stats$SEM + asterisk.offset, na.rm=TRUE) > 60) { ylim <- c(0, 104) }

  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Perennial Grass [%]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  abline(h=stats$Mean[1], lwd=1,col='red')
  # abline(h = 6, lwd = 1, col = "darkgreen")
  text(rmarg, stats$Mean[1], 'Baseline Grass', adj=0, xpd=NA)

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.




  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='darkgreen')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 2 x standard error of the mean using the usual graphics.

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1*(stats$SEM[i]), stats$Mean[i]+ 1*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1*(stats$SEM[i]),2))
    # lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1.96*(stats$SEM[i]), stats$Mean[i]+ 1.96*(stats$SEM[i])))
    # lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1.96*(stats$SEM[i]),2))
    # lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1.96*(stats$SEM[i]),2))
  }

  # Now, rather than performing the one-way analysis of variance (weighted) followed
  # by Dunnett's method, we will simply perform a two-sample t-test with unequal
  # variances.  Note that the weighted analysis of variance is essentially
  # a generalization of this approach.

  # if (analyzable) {

  # We want to pick off the first year.  For safety's sake, we
  # sort the data to make sure they are in order, although
  # that seems to be a side effect of the code above.

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  # Just roll through the other years one by one.  This could probably
  # be done more compactly.  On the other hand, it should be clear
  # what is happening here.

  # TO DO:  What *would* be better would be cleaning
  # up this clunky method of getting the years.  It would be best
  # to have a list (vector) containing the years to be analyzed.

  # blyears<-c(1984,1985,1986,1987,1988)
  for (i in 2:n) {#start at 2 because 1 is the baseline year and this for loop is only for each comparison to baseline
    #Transect$Grass[Transect$Year==stats$Year[1]]
    #str(stats)
    #length(data1)
    # data1 <- Transect$Grass[Transect$Year==stats$Year[1]]#baseline
    data1 <- grass.pcl$Grass[grass.pcl$Year < 1989]
    # data1 <- Transect$Grass[Transect$Year %in% c(1984,1985,1986,1987,1988)]#baseline

    mu.d1<-mean(data1)



    # could have a covariate indicating  baseline year and flags for comparisons, ordinal perhaps
    data2 <- grass.pcl$Grass[grass.pcl$Year==stats$Year[i]]
    #mu <- data1
    # str(mu)
    #ifelse(length(data1) == 0), mu= 0,

    ifelse (length(data1) <= 4,#if parcel has single transect or was assigned the wvcomm variable as the baseline cover, conduct a one sample t-test. this should include the value of zero for grass where no grass detected during baseline.

            fit <- t.test(data2, mu = mu.d1),

            fit <- t.test(data1,data2)# otherwise conduct the two sample t-test, defaulting to welch-satherwaite method to account for unequal variance.
    )


    # fit <- t.test(data1, data2)

    pvalues[i] <- fit$p.value
  }
  # kable(pvalues)
  # Add an asterisk according to whether the comparison with
  # baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      text(stats$Year[i], stats$Mean[i]+1+ asterisk.offset, '*', adj=.5)
    }
  }
  # }
}


# assimilate 5 timeseries----

five_row_timeseries <- function(attributes_pfix, transects, dtw_pfix, rs_pfix, cYear){

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }


  # Set global configurations.

  r.squared.digits <- 2  ### Round the r-squared values to this many digits.
  p.value.digits   <- 4  ### Round the p-values to this many digits
  b.digits         <- 4  ### Round the estimates to this many digits



  # Configure global plot tuning parameters.

  bar.space <- 0.25        ### The space between each bar on the graphs.

  # can define year here for x-axis if doing one off pdf export
  xlim <- c(1985, 2020)   ### The x-axis limits for the graphing
  #xlim <- c(1985, cYear)   ### The x-axis limits for the graphing effort.
  ylim <- c(0, 62.4)       ### The y-axis limits for percentage cover.
  ylim.ndvi<-c(.1,.8)
  dtw.max <- 5             ### Maximum y-axis limits for the DTW measurements.
  dtw.split <- 6           ### Where to switch the y-axis for the DTW measurements.
  dtw.xy <- c(.95, .90)    ### DTW plot annotation location.
  asterisk.offset <- 4     ### How much to space the asterisks.
  rmarg <- cYear + 1.5     ### Tune stuff in the right margin.
  show.caption <- TRUE     ### Print a figure caption or not?

# AttributesPID <- attributes_pfix %>% filter(reinv == "r", Parcel %in% c("BLK094","BLK099"))

  # filter(!Parcel %in% c('BGP013','BLK006',"BGP204", "BGP205", "BLK008", "FSL179", "IND086", "LAW076", "LAW110","MAN038", "PLC113", "PLC220", "TIN006", "UNW074"))

PIDs <- unique(AttributesPID$Parcel)
# all.pid <- PIDs

# layout = "l-page",
# FILEpdf <- paste0(dir, '/output/TimeSeries2018.all.pdf')
FILEpdf <- paste0("output/ts5stack_",cYear,".pdf")
# library(plyr)
fig.num <- 1

pdf(FILEpdf)# opens the dev env
# png(FILEpng)
par(mfrow=c(5,1), oma=c(5,0,5,0), plt=c(.1,.85,.1,.85)) # set up the position of elements

# call functions
for (PID in PIDs) {
  # call functions
  plot.perennial.cover(PID, transects)
  plot.perennial.grass(PID, transects)
  plot.dtw(PID, dtw_pfix)
  plot.ndvi(PID, rs_pfix)
  plot.ppt(PID, rs_pfix)

  # caption variables
  # Parcel <- parcels %>% filter(Parcel==PID)

  Attribute3 <- attributes_pfix %>% filter(Parcel==PID)
  Transect.n <- transects %>% filter(Parcel==PID,Year==cYear)
  n.tran <-  Transect.n %>% group_by(Parcel) %>% dplyr::summarise(count = n()) # doing count summaries for e

  Descriptor <- paste(PID,'(W/C): ',Attribute3$Type,'| Type: ', Attribute3$GB_TYPE,'|', Attribute3$Holland, sep=' ')
  ESD <- paste(Attribute3$taxorder, Attribute3$compname,  '| ESD: ',Attribute3$Ecologic_3, sep=' ')
  geom <- paste('Geomorphic:',Attribute3$geomdesc)
  # last.year <- max(Parcel$Year, na.rm=TRUE)
  test.type <- Attribute3$test_type
  Caption <- paste('Figure ', fig.num, ': ', test.type,': Baseline (',Attribute3$bl.origin,') vs. reinventory (* p < 0.05).\n Baseline sample size (n = ',Attribute3$n86,'). Current year sample size (n = ',n.tran$count,').', ' Error bars = 95% CI.', sep='')
  mtext(side=3, outer=TRUE, line=0, geom)
  mtext(side=3, outer=TRUE, line=3, Descriptor)
  mtext(side=3, outer=TRUE, line=1.5, ESD)
  mtext(side=1, outer=TRUE, line=3, adj=0.15, family='serif', Caption)
  # update fig.num for next plot
  fig.num <- fig.num + 1
  }
dev.off()

return(FILEpdf)
}

# ttests----
#' Title
#'
#' @param transects
#' @param attributes_reinv
#'
#' @return
#' @export
#'
#' @examples
nest_transects <- function(transects, attributes_reinv){

  byparyear <- transects %>% filter(Parcel %in% attributes_reinv$Parcel) %>%
    group_by(Parcel,Year) %>%
    nest() %>%
    mutate(n = map_dbl(data,nrow))

  gb <- byparyear %>% filter(Year > 1990)
  bl <- byparyear %>% filter(Year < 1990)
  parcel_year_nested_transects <- gb %>% left_join(bl, by = 'Parcel')

  return(parcel_year_nested_transects)
}

#' Title
#'
#' @param data.x
#' @param data.y
#'
#' @return
#' @export
#'
#' @examples
#'
t2samp <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  failproof.t(data.x$Cover,data.y$Cover, conf.level = .95)
}

t2samp_grass <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  failproof.t(data.x$Grass,data.y$Grass, conf.level = .95)
}

# functional type added so we can do tests on cover or grass etc
# t2samp <- function(data.x, data.y, functional_type) {
#   failproof.t <- purrr::possibly(t.test, NA_real_)
#   failproof.t(data.x$functional_type,data.y$functional_type, conf.level = .95)
# }

#' Title
#'
#' @param data.x
#' @param data.y
#'
#' @return
#' @export
#'
#' @examples
t1samp <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  failproof.t(data.x$Cover,mu = mean(data.y$Cover), conf.level = .95)

}

t1samp_grass <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  failproof.t(data.x$Grass,mu = mean(data.y$Grass), conf.level = .95)

}

# new trial functional type
# t1samp <- function(data.x, data.y, functional_type) {
#   failproof.t <- purrr::possibly(t.test, NA_real_)
#   failproof.t(data.x$functional_type,mu = mean(data.y$functional_type), conf.level = .95)
#
# }


#' Title
#'
#' @param parcel_year_meta_2samp
#'
#' @return
#' @export
#'
#' @examples
# two_sample_ttest <- function(parcel_year_meta_2samp){
# parcel_year_meta_2samp %>% mutate(model = map2(data.x, data.y, t2samp)) %>%
#   mutate(glance = map(model, broom::glance)) %>%
#   unnest(glance) %>% mutate(significance = ifelse(p.value < 0.05, ifelse(statistic > 0  ,'ns','significant'),'ns'))
# }

two_sample_ttest <- function(parcel_year_meta_2samp){
  parcel_year_meta_2samp %>%
    mutate(model = map2(data.x, data.y, t2samp)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0  ,
                                        'ns',
                                        'significant'),
                                 'ns'),
           cover_type = 'Cover'
           )
}

two_sample_ttest_grass <- function(parcel_year_meta_2samp){
  parcel_year_meta_2samp %>%
    mutate(model = map2(data.x, data.y, t2samp_grass)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0  ,
                                        'ns',
                                        'significant'),
                                 'ns'),
           cover_type = 'Grass'
    )
}

#' Title
#'
#' @param parcel_year_meta_1samp
#'
#' @return
#' @export
#'
#' @examples
one_sample_ttest <- function(parcel_year_meta_1samp){
  parcel_year_meta_1samp %>%
    mutate(model = map2(data.x, data.y, t1samp)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    # hoist(model,"null.value") %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0 ,'ns','significant'),
                                 'ns'),
           cover_type = 'Cover'
           )
}

one_sample_ttest_grass <- function(parcel_year_meta_1samp){
  parcel_year_meta_1samp %>%
    mutate(model = map2(data.x, data.y, t1samp_grass)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    # hoist(model,"null.value") %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0 ,'ns','significant'),
                                 'ns'),
           cover_type = 'Grass'
    )
}

#' Title
#'
#' @param data
#' @param cYear
#'
#' @return
#' @export
#'
#' @examples
plot_1samptest_timeseries <- function(data,cYear,parcel.select){
  data <- data %>% filter(Parcel %in% parcel.select)
  plot <- data %>% ggplot(aes(x=Year.x))+
    ggtitle(paste("Perennial Cover (1991-",cYear,") Compared to Baseline (1984-1987).
 One-Sample t-test (mu = mean of baseline transects for n < 5, ns p > 0.05, significant p < 0.05"))+
  # ns (not significant, p > 0.05), significant (p < 0.05)")
  # +
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,color=significance,width=0.2))+
  geom_line(aes(y=estimate))+
  # geom_line(aes(y=baseline, color='Baseline'))+#baseline year
  scale_colour_viridis_d(option = 'magma')+# direction =1 (dark to light order); "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
  # scale_color_manual(values = c("blue", "red"))+
  ylim(0,100)+
  ylab("Perennial Cover (%)")+
  facet_wrap(~Parcel, ncol = 1)+
    theme(legend.position="top")
  # ggsave("plot_1samp.png", plot, width = 7, height = 7)
  # return("plot_1samp.png")
  return(plot)

}


#' Title
#'
#' @param data
#' @param cYear
#'
#' @return
#' @export
#'
#' @examples
plot_2samptest_timeseries <- function(data,cYear,parcel.select){
  data <- data %>% filter(Parcel %in% parcel.select)
  plot <- data %>% ggplot(aes(x=Year.x))+
  ggtitle(paste("Perennial Cover Time Series (1991-",cYear,") Relative to Baseline (1984-1987).
  Welch Two Sample t-test (ns p > 0.05, significant p < 0.05)"))+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,color=significance,width=0.2))+
  geom_line(aes(y=estimate1))+#reinventory year
  scale_color_manual(values = c("green","orange", "blue", "red"))+
    # scale_color_npg()+
  geom_line(aes(y=estimate, color='Deviation from Baseline\n (relative to zero)'))+#baseline year
  geom_line(aes(y=estimate2, color='Baseline'))+#baseline year
  # ylim(0,100)+
  ylab("Perennial Cover (%)")+
  facet_wrap(~Parcel,ncol = 1)+
    theme(legend.position="top")
  # ggsave("plot_2samp.png", plot, width = 7, height = 7)
  # return("plot_2samp.png")
  return(plot)

}

#' Title
#'
#' @param data2samp
#' @param data1samp
#'
#' @return
#' @export
#'
#' @examples
# bindttest_count_sig_runs <- function(data2samp, data1samp){
#   bound <- bind_rows(data2samp,data1samp)
#
#   count.runs <- bound  %>%
#     mutate(indicator = case_when(
#       significance == 'significant'~1,
#       significance == 'ns'~0)) %>%
#     group_by(Parcel, grp = with(rle(indicator), rep(seq_along(lengths), lengths))) %>%
#     mutate(counter = seq_along(grp)) %>%
#     ungroup() %>%
#     select(-grp) %>%
#     mutate(sig.counter = indicator*counter)
#
#
# }

# trial
bindttest_count_sig_runs <- function(data2samp_g, data1samp_g,data2samp_c, data1samp_c){
  bound <- bind_rows(data2samp_g,data1samp_g,data2samp_c,data1samp_c)

  count.runs <- bound  %>%
    mutate(indicator = case_when(
      significance == 'significant'~1,
      significance == 'ns'~0)) %>%
    group_by(Parcel,cover_type, grp = with(rle(indicator), rep(seq_along(lengths), lengths))) %>%
    mutate(counter = seq_along(grp)) %>%
    ungroup() %>%
    select(-grp) %>%
    mutate(sig.counter = indicator*counter)


}

parcel_testadd_sums <- function(data){
  data %>%
    group_by(Parcel,cover_type) %>%
    summarise(n = n(),
              sum.sig = sum(indicator),
              max.run = max(sig.counter),
              prop.sig = round(sum.sig/n,2))

}


#' join_summaries
#'
#' @param parcels_deltas
#' @param attributes_pfix
#' @param parcel_year_meta_combined_results
#' @param cYear
#'
#' @return
#' @export
#'
#' @examples
join_summaries <- function(parcels_deltas,attributes_pfix, parcel_year_meta_combined_results, parcel_test_sums, cYear){

deltas <- parcels_deltas %>%
  filter(Year == cYear) %>%
  select(Parcel, TLC, Cover, Cover.Delta, Grass, pGrass, Grass.Delta, Shrub, pShrub, Shrub.Delta)%>%
  mutate(across(Cover:Shrub.Delta, round, 1))

test <- parcel_year_meta_combined_results %>%
  filter(Year.x == cYear) %>%
  select(Parcel,Year.x,n.x,n.y,significance,sig.counter,cover_type) %>%
  pivot_wider(names_from = cover_type,
              names_glue="{cover_type}_{.value}",
              values_from = c(significance,sig.counter))

sums <- parcel_test_sums %>%
  pivot_wider(names_from = cover_type,
              names_glue="{cover_type}_{.value}",
              values_from = c(sum.sig,max.run,prop.sig))

par.delta.test.att <- deltas %>%
  left_join(test, by = 'Parcel') %>%
  left_join(attributes_pfix, by = 'Parcel') %>%
  left_join(sums, by = 'Parcel')

# datatable(par.delta.test.att)
return(par.delta.test.att)

}

count_wellfield_control_parcels <- function(data){
data %>% select(Parcel, Type) %>% group_by(Type) %>%  summarise(n = n_distinct(Parcel))
}
# summary table----

# add sig.counter,max.run, sum.sig, n
make_parcel_data_table <- function(deltas_ttest_att,cYear){
  table <- deltas_ttest_att %>%
    # mutate(p.value = round(p.value,3)) %>%
    filter(!Parcel %in% c("FSL044")) %>%
    select(Parcel,GB_TYPE,Holland,wellfield,Type, Cover.Delta,
           Cover_significance, Grass.Delta,Grass_significance,
           pGrass,pShrub,Cover,Grass,Shrub) %>%
    datatable(filter = 'top',
              options = list(
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 2, rightColumns = 0),
                pageLength = 5,
                autoWidth = TRUE,
                colReorder = TRUE)
              # ,
              # caption = paste("Baseline vs",cYear,"statistical significance summary."

              )


  # %>% htmlwidgets::saveWidget('parcel-summary.html')

  # return("parcel-summary.html")
  return(table)
}



# filter the main table further for a persistently significant deviation.
# I should probably just create another target for a highlighted list for
# executive summary and wrap the datatable options into a table function.

make_parcel_data_table_significant <- function(deltas_ttest_att,cYear){
  table <- deltas_ttest_att %>%
    filter(Cover_significance == 'significant' | Grass_significance == 'significant', Type == 'W') %>%
    # mutate(p.value = round(p.value,3)) %>%
    # filter(!Parcel %in% c("FSL044")) %>%
    select(Parcel,Cover.Delta, Cover_sig.counter, Grass.Delta,Grass_sig.counter, n, GB_TYPE,Holland,
           pGrass,pShrub,Cover,Grass,Shrub) %>%
    datatable(filter = 'top',
              options = list(
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 2, rightColumns = 0),
                pageLength = 5,
                autoWidth = TRUE,
                colReorder = TRUE)
              # ,
              # caption = paste("Baseline vs",cYear,"statistical significance summary."

    )
  return(table)
}

  make_parcel_data_table_chronic <- function(deltas_ttest_att,cYear){
    table <- deltas_ttest_att %>%
      filter(Cover_sig.counter > 5 | Grass_sig.counter > 5,  Type == 'W') %>%
      # mutate(p.value = round(p.value,3)) %>%
      # filter(!Parcel %in% c("FSL044")) %>%
      select(Parcel,Cover.Delta, Grass.Delta,Cover_sig.counter,Grass_sig.counter, n,Cover_prop.sig, Grass_prop.sig, GB_TYPE,Holland,
             pGrass,pShrub,Cover,Grass,Shrub) %>%
      datatable(filter = 'top',
                options = list(
                  dom = 'Blfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2, rightColumns = 0),
                  pageLength = 5,
                  autoWidth = TRUE,
                  colReorder = TRUE)
                # ,
                # caption = paste("Baseline vs",cYear,"statistical significance summary."

      )
  # %>% htmlwidgets::saveWidget('parcel-summary.html')

  # return("parcel-summary.html")
  return(table)
}

  # try the pivot wider to get columns of 'sig.cover' and 'sig.grass'
# join_ttest_shapefile <- function(parcels_shp, deltas_ttest_att){
#   d <- deltas_ttest_att %>% filter(cover_type = 'Cover')
#   out <- parcels_shp %>% left_join(d, by = c("PCL"="Parcel")
#                                    )
#   return(out)
# }


make_wellfield_table <- function(data, wf){
    data %>%
      filter( Type == 'W') %>%
      filter(wellfield == wf) %>%
      select(Parcel,wellfield,GB_TYPE, Holland,Cover.Delta, Grass.Delta,Cover_sig.counter,Grass_sig.counter, n,
             Cover,Grass,Shrub) %>%
      gt(groupname_col = "wellfield") %>%
      summary_rows(
        groups = TRUE,
        columns = Cover.Delta,
        fns = list(
          Average = "mean"),
        decimals = 1
      )%>%
      tab_footnote(
        footnote = "Perennial grass cover below baseline for five years or more.",
        locations = cells_body(
          columns = Grass_sig.counter,
          rows = Grass_sig.counter > 4
        )
      )%>%
      tab_footnote(
        footnote = "Total perennial cover below baseline for five years or more.",
        locations = cells_body(
          columns = Cover_sig.counter,
          rows = Cover_sig.counter > 4
        )
      )
  }

  grass_map<- function(cYear,parcels_shp_ttest, wf, or,streams,canals,laa,lakes, monit.sites){

    limit <- parcels_shp_ttest %>% filter(grepl(wf,wellfield))

    tpc.below <- limit %>% filter(Cover_significance == 'significant')

    pgr.below <- limit %>% filter(Grass_significance == 'significant')

    tmap_mode("plot")

  tmgrass <-

    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Grass"), breaks = c(0,10,20,30,40,Inf), palette = "Greens",title.col = "PCL",  id = "PCL")+

    tm_shape(pgr.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = .5)+
    tm_text("PCL", size = .6,  col = "black",shadow=FALSE,remove.overlap=FALSE, group = 'Labels', auto.placement = 1, bg.color = "white")+

    tm_shape(canals, group = 'Canals') +
    tm_lines(col = "blue", scale = .5, group = 'Canals')+


    tm_shape(streams, group = 'Streams') +
    tm_lines(col = "blue", scale = .8, group = 'Streams')+
    # tm_text("NAME", size = .6, col = "blue")+

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE",  col = "black", size=.8,remove.overlap=TRUE,shadow=FALSE,group = 'Labels',auto.placement = TRUE,bg.color = 'yellow') +
    tm_symbols(col = "yellow", scale = .1, title.col = "SITE",  id = "SITE")+

    tm_shape(or, group = 'River') +
    tm_lines(col = "blue", scale = .5, group = 'River')+


    tm_shape(laa, group = 'LAA') +
    tm_lines(col = "blue", scale = 1, group = 'LAA')


  return(tmgrass)
  }

  grass_map2<- function(cYear,parcels_shp_ttest, wf, or,streams,canals,laa,lakes, monit.sites){

    limit <- parcels_shp_ttest %>% filter(grepl(wf,wellfield))

    tpc.below <- limit %>% filter(Cover_significance == 'significant')

    pgr.below <- limit %>% filter(Grass_significance == 'significant')

    tmap_mode("plot")

    tmgrass <-




      tm_shape(limit, group = 'Wellfield - Parcels') +
      tm_polygons(col =c("Grass"), breaks = c(0,10,20,30,40,Inf), palette = "Greens",title.col = "PCL",  id = "PCL")+
      # tm_borders(col = 'red',lwd = .7)+
      # tm_text("PCL", size = .5,  col = "white",shadow=FALSE,remove.overlap=, group = 'Labels', auto.placement = 1, bg.color = 'black')+

      tm_shape(pgr.below, group = 'Wellfield - Parcels') +
      tm_borders(col = 'red',lwd = .5)+
      tm_text("PCL", size = .6,  col = "black",shadow=FALSE,remove.overlap=FALSE, group = 'Labels', auto.placement = 1, bg.color = "white")+

      tm_shape(canals, group = 'Canals') +
      tm_lines(col = "blue", scale = .5, group = 'Canals')+


      tm_shape(streams, group = 'Streams') +
      tm_lines(col = "blue", scale = .8, group = 'Streams')+
      # tm_text("NAME", size = .6, col = "blue")+

      tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
      tm_text("SITE",  col = "black", size=.8,remove.overlap=TRUE,shadow=FALSE,group = 'Labels',auto.placement = TRUE,bg.color = 'yellow') +
      tm_symbols(col = "yellow", scale = .1, title.col = "SITE",  id = "SITE")+

      tm_shape(or, group = 'River') +
      tm_lines(col = "blue", scale = .5, group = 'River')+

      tm_shape(laa, group = 'LAA') +
      tm_lines(col = "blue", scale = 1, group = 'LAA')

    return(tmgrass)
  }


# Maps----
#' Title
#'
#' @param cYear
#' @param parcels_shp_ttest
#' @param wf
#' @param or
#' @param streams
#' @param canals
#' @param laa
#' @param lakes
#' @param monit.sites
#'
#' @return
#' @export
#'
#' @examples
panel_map<- function(cYear,parcels_shp_ttest, wf, or,streams,canals,laa,lakes, monit.sites){

  # write function to handle custom plot with input as string e.g. 'Laws'
  limit <- parcels_shp_ttest %>% filter(grepl(wf,wellfield))

  tmap_mode("plot")

  tmgrass <-
    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Grass"), breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf), palette = "Greens",title.col = "PCL_merged",  id = "PCL_merged", group = "Wellfield - Parcels")

  tmgrassd <- tm_shape(limit, group = 'Wellfield - Parcels') +

    tm_polygons(col =c("Grass.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",group = "Wellfield - Parcels")+

    tm_shape(canals, group = 'Canals') +

    tm_lines(col = "blue", scale = .6, group = 'Canals')+


    tm_shape(streams, group = 'Streams') +

    tm_lines(col = "blue", scale = 1, group = 'Streams')+

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE",  col = "white", size=.5,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE",  id = "SITE",group = 'On/Off Monitoring Sites')+

    tm_shape(or, group = 'River') +

    tm_lines(col = "blue", scale = 1, group = 'River')+

    tm_shape(laa, group = 'LAA') +

    tm_lines(col = "blue", scale = 1, group = 'LAA')+

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')

  tmcov <- tm_shape(limit, group = 'Wellfield - Parcels') +

    tm_polygons(col =c("Cover"), breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf),palette = "Greens",title.col = "PCL_merged",  id = "PCL_merged",group = "Wellfield - Parcels")


  tmcovd <- tm_shape(limit, group = 'Wellfield - Parcels') +

    tm_polygons(col =c("Cover.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged", group = "Wellfield - Parcels")+

    # tm_shape(tpc.below, group = 'Wellfield - Parcels') +
    # tm_borders(col = 'red')+

    tm_shape(canals, group = 'Canals') +

    tm_lines(col = "blue", scale = .6, group = 'Canals')+

    tm_shape(streams, group = 'Streams') +

    tm_lines(col = "blue", scale = .7, group = 'Streams')+

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE",  col = "white", size=.5,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE",  id = "SITE",group = 'On/Off Monitoring Sites')+


    tm_shape(or, group = 'River') +

    tm_lines(col = "blue", scale = 1, group = 'River')+

    tm_shape(laa, group = 'LAA') +

    tm_lines(col = "blue", scale = 1, group = 'LAA')+

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')

map <-tmap_arrange(tmgrass, tmgrassd,tmcov,tmcovd,ncol=2)


return(map)


}

  # Maps view interactive----
  #' Title
  #'
  #' @param cYear
  #' @param parcels_shp_ttest
  #' @param wf
  #' @param or
  #' @param streams
  #' @param canals
  #' @param laa
  #' @param lakes
  #' @param monit.sites
  #'
  #' @return
  #' @export
  #'
  #' @examples
  panel_map_view<- function(cYear,parcels_shp_ttest, wf, or,streams,canals,laa,lakes, monit.sites){

    # write function to handle custom plot with input as string e.g. 'Laws'
    limit <- parcels_shp_ttest %>% filter(grepl(wf,wellfield))

    tpc.below <- limit %>% filter(Cover_significance == 'significant')

    pgr.below <- limit %>% filter(Grass_significance == 'significant')


    tmap_mode("view")


      tm_shape(limit,
               group = 'Wellfield - Parcels') +
      tm_polygons(col =c("Grass"),
                  breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf),
                  palette = "Greens",
                  title.col = "PCL_merged",
                  id = "PCL_merged",
                  popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"),
                  group = "Wellfield - Parcels")+

      tm_shape(pgr.below,
               group = 'Wellfield - Parcels') +
      tm_borders(col = 'red',lwd = 2)+
      tm_text("PCL", size = .5,  col = "white",shadow=TRUE,remove.overlap=FALSE, group = 'Labels', auto.placement = .2, bg.color = 'darkgreen')

      tm_shape(limit, group = 'Wellfield - Parcels') +
      tm_polygons(col =c("Grass.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

      tm_shape(pgr.below, group = 'Wellfield - Parcels') +
      tm_borders(col = 'red',lwd = 1)+

      # cover delta
      tm_shape(limit, group = 'Wellfield - Parcels') +
      tm_polygons(col =c("Cover"), breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf),palette = "Greens",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

      tm_shape(tpc.below, group = 'Wellfield - Parcels') +
      tm_borders(col = 'red',lwd = 2)+
      tm_text("PCL",  size = .5,col = "white",shadow=TRUE,remove.overlap=FALSE, group = 'Labels', auto.placement = .2, bg.color = 'darkgreen')

      tm_shape(limit, group = 'Wellfield - Parcels') +
      tm_polygons(col =c("Cover.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

      tm_shape(tpc.below, group = 'Wellfield - Parcels') +
      tm_borders(col = 'red')+

      # on/off sites
      tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
      tm_text("SITE",  col = "white", size=.5,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'blue') +
      tm_symbols(col = "blue", scale = .05, title.col = "SITE",  id = "SITE",popup.vars = c("SITE","TYPE"),group = 'On/Off Monitoring Sites')+

      # background reference layers
      tm_shape(canals, group = 'Canals') +
      tm_lines(col = "blue", scale = .6, group = 'Canals')+

      tm_shape(streams, group = 'Streams') +
      tm_lines(col = "blue", scale = 1, group = 'Streams')+

      tm_shape(or, group = 'River') +
      tm_lines(col = "blue", scale = 1, group = 'River')+

      tm_shape(laa, group = 'LAA') +
      tm_lines(col = "blue", scale = 1, group = 'LAA')+

      tm_shape(lakes, group = 'Lakes') +
      tm_polygons(col = "blue", scale = 1, group = 'Lakes')


    return(map)


  }



#' Title
#'
#' @param dtw_data_p
#' @param staid_p
#'
#' @return
#' @export
#'
#' @examples
  plot_wtbm<- function(dtw_data_p, staid_p){

    dtw<-dtw_data_p%>%filter(staid == staid_p)
    dtw.z <- zoo(dtw$dtw, order.by = dtw$date)
    dtw.ribbon<-zoo(dtw$ribbon, order.by = dtw$date)

    p <- dygraph(dtw.z, group = "a", main = paste0(staid_p,"DTW (ft bgs)")) %>% dyRangeSelector() %>%
      dyAxis("y", label = "DTW (ft)", valueRange = c(25, -2))%>%
      dyRoller(rollPeriod = 5)%>%

      # dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>%
      dyLimit(0, color = "brown","ground surface", labelLoc = "left")%>%
      dyLimit(6, color = "green","grass root zone", labelLoc = "left")%>%
      dyLimit(12, color = "brown", "shrub root zone", labelLoc = "left")%>%
      dyShading(from = "1987-4-1", to = "1993-4-1")%>%
      dyShading(from = "1999-4-1", to = "2005-4-1")%>%
      dyShading(from = "2007-4-1", to = "2010-4-1")%>%
      dyShading(from = "2012-4-1", to = "2017-4-1")%>%
      dyShading(from = "2020-4-1", to = "2023-4-1")%>%
      dyRibbon(data = dtw.ribbon, palette = c('green','lightgreen','tan'), top = 0.2, bottom = 0.05)
    print(p)
    #
  }

  # ndvi plot
#' Title
#'
#' @param path
#' @param parcel
#'
#' @return
#' @export
#'
#' @examples
  ndvi_dygraph_parcel_plot <- function(path, parcel){
    parcel_path <- paste0(path,plot_parcel,"/",plot_parcel,"_landsat_daily.csv")


    ndvi <- read_csv(parcel_path)
    ndvi<-ndvi%>%filter(QA == 0)
    ndvi.z <- zoo(x=ndvi$NDVI_SUR, order.by = ndvi$DATE)

    # tm.an<-tm2 %>% filter(ABBR_NAME == "IND CR @ JCT STA" )
    # tm.an<-zoo(x=tm.an$read, order.by=tm.an$date)
    dygraph(ndvi.z,group="a") %>% dyRangeSelector()%>%
      dyAxis("y", label = "NDVI (Dimensionless)")
  }



  #' Combine monitoring well dtw hydrograph with ndvi timeseries. User specifies pair using parcel code and mw staid.
  #'
  #' @param path
  #' @param parcel
  #' @param parcel
  #'
  #'
  #' @return
  #' @export
  #'
  #' @examples
  ndvi_dtw_syncplot <- function(path, parcel, staid_p, dtw_data_p, group_name){

    landsat_path <- paste0(path,plot_parcel,"/",plot_parcel,"_landsat_daily.csv")
    gridmet_path <- paste0(path,plot_parcel,"/",plot_parcel,"_gridmet_monthly.csv")


    dtw<-dtw_data_p%>%filter(staid == staid_p)
    dtw.z <- zoo(dtw$dtw, order.by = dtw$date)
    dtw.ribbon<-zoo(dtw$ribbon, order.by = dtw$date)
    #--------------------
    ppt <- read_csv(gridmet_path)
    ndvi <- read_csv(landsat_path)
    ndvi<-ndvi%>%filter(QA == 0)

    ndvi.z <- zoo(x=ndvi$NDVI_SUR, order.by = ndvi$DATE)
    ppt.z <- zoo(x=ppt$PPT, order.by = ppt$DATE)
    # tm.an<-tm2 %>% filter(ABBR_NAME == "IND CR @ JCT STA" )
    # tm.an<-zoo(x=tm.an$read, order.by=tm.an$date)
    #-------------------
    dyg1 <- dygraph(ndvi.z,group= group_name) %>% dyRangeSelector()%>%
      dyAxis("y", label = "NDVI (Dimensionless)") %>% dyRoller(rollPeriod = 5)%>%
      dyCrosshair(direction = "vertical")


    dyg3 <- dygraph(ppt.z,group= group_name) %>% dyRangeSelector()%>%
      dyAxis("y", label = "PPT (cm)")%>%
      dyBarChart()%>% dyRoller(rollPeriod = 5)%>%
      dyCrosshair(direction = "vertical")



    dyg2 <- dygraph(dtw.z, group = group_name, main = paste0(staid_p,"DTW (ft bgs)")) %>% dyRangeSelector() %>%
      dyAxis("y", label = "DTW (ft)", valueRange = c(25, -2))%>%
      # dyRoller(rollPeriod = 5)%>%

      # dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>%
      dyLimit(0, color = "brown","ground surface", labelLoc = "left")%>%
      dyLimit(6, color = "green","grass root zone", labelLoc = "left")%>%
      dyLimit(12, color = "brown", "shrub root zone", labelLoc = "left")%>%
      dyShading(from = "1987-4-1", to = "1993-4-1")%>%
      dyShading(from = "1999-4-1", to = "2005-4-1")%>%
      dyShading(from = "2007-4-1", to = "2010-4-1")%>%
      dyShading(from = "2012-4-1", to = "2017-4-1")%>%
      dyShading(from = "2020-4-1", to = "2023-4-1")%>%
      dyRibbon(data = dtw.ribbon, palette = c('green','lightgreen','tan'), top = 0.2, bottom = 0.05) %>%
      dyRoller(rollPeriod = 5)%>%
      dyCrosshair(direction = "vertical")

    #------------
    dy_graphs = list(dyg1,dyg3,dyg2)
    htmltools::browsable(htmltools::tagList(dy_graphs))
  }


  presAnnotation <- function(dygraph, x, text) {
    dygraph %>%
      dyAnnotation(x, text, attachAtBottom = TRUE, width = 90)
  }


  #' Combine monitoring well dtw hydrograph with ndvi timeseries. User specifies pair using parcel code and mw staid.
  #'
  #' @param path
  #' @param parcel
  #' @param parcel
  #'
  #'
  #' @return
  #' @export
  #'
  #' @examples
  ndvi_syncplot <- function(path, plot_parcel,group_name){

    landsat_path <- paste0(path,"/",plot_parcel,"/",plot_parcel,"_landsat_daily.csv")

    #--------------------
    ndvi <- read_csv(landsat_path)
    ndvi<-ndvi%>%filter(QA == 0) %>% filter(NDVI_SUR>0)
    ndvi.z <- zoo(x=ndvi$NDVI_SUR, order.by = ndvi$DATE)

    #-------------------
dygraph(ndvi.z,group= group_name) %>%
      # dyRangeSelector()%>%
      dyAxis("y", label = "NDVI (Dimensionless)") %>%
      dyRoller(rollPeriod = 1)%>%
      dyCrosshair(direction = "vertical")%>%
      dyBarChart()%>%
      presAnnotation("1987-1-1", text = plot_parcel)


  }

  #' Combine monitoring well dtw hydrograph with ndvi timeseries. User specifies pair using parcel code and mw staid.
  #'
  #' @param path
  #' @param parcel
  #' @param parcel
  #'
  #'
  #' @return
  #' @export
  #'
  lpt_pft_syncplot <- function(data, plot_parcel,group_name){

    lpt <- data %>% filter(Parcel == plot_parcel)%>% mutate(year = strptime(as.numeric(Year), "%Y"),
                                                            date = ymd(year))

    grass.z <-  zoo(x=lpt$Grass, order.by = lpt$date)
    shrub.z <-  zoo(x=lpt$Shrub, order.by = lpt$date)
    pft_bind <- cbind(shrub.z,grass.z)

    dygraph(data= pft_bind, group = group_name) %>%
      dyAxis("y", label = "% Canopy Cover") %>%
      dyCrosshair(direction = "vertical")%>%
      dySeries('grass.z',label = 'Perennial Grass') %>%
      dySeries('shrub.z',label = 'Shrub')

  }


  #' Combine monitoring well dtw hydrograph with ndvi timeseries. User specifies pair using parcel code and mw staid.
  #'
  #' @param path
  #' @param parcel
  #' @param parcel
  #'
  #'
  #' @return
  #' @export
  #'
  #' @examples
  ppt_syncplot <- function(path, plot_parcel, group_name){

    gridmet_path <- paste0(path,plot_parcel,"/",plot_parcel,"_gridmet_monthly.csv")


    #--------------------
    ppt <- read_csv(gridmet_path)
    ppt.z <- zoo(x=ppt$PPT, order.by = ppt$DATE)

dygraph(ppt.z,group= group_name) %>%
  # dyRangeSelector()%>%
      dyAxis("y", label = "PPT (cm)")%>%
      dyBarChart()%>%
    dyRoller(rollPeriod = 1)%>%
      dyCrosshair(direction = "vertical")%>%
  presAnnotation("1987-1-1", text = plot_parcel)

  }

  #' Combine monitoring well dtw hydrograph with ndvi timeseries. User specifies pair using parcel code and mw staid.
  #'
  #' @param path
  #' @param parcel
  #' @param parcel
  #'
  #'
  #' @return
  #' @export
  #'
  #' @examples
  dtw_syncplot <- function(staid_p, dtw_data_p, group_name){


    dtw<-dtw_data_p%>%filter(staid == staid_p)
    dtw.z <- zoo(dtw$dtw, order.by = dtw$date)
    dtw.ribbon<-zoo(dtw$ribbon, order.by = dtw$date)
    #--------------------

  dygraph(dtw.z, group = group_name, main = paste0(staid_p," DTW")) %>%
      # dyRangeSelector() %>%
      dyAxis("y", label = "DTW (ft)", valueRange = c(25, -2))%>%
      # dyRoller(rollPeriod = 5)%>%

      # dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>%
      dyLimit(0, color = "brown","ground surface", labelLoc = "left")%>%
      dyLimit(6, color = "green","grass root zone", labelLoc = "left")%>%
      dyLimit(12, color = "brown", "shrub root zone", labelLoc = "left")%>%
      dyShading(from = "1987-4-1", to = "1993-4-1")%>%
      dyShading(from = "1999-4-1", to = "2005-4-1")%>%
      dyShading(from = "2007-4-1", to = "2010-4-1")%>%
      dyShading(from = "2012-4-1", to = "2017-4-1")%>%
      dyShading(from = "2020-4-1", to = "2023-4-1")%>%
      dyRibbon(data = dtw.ribbon, palette = c('green','lightgreen','tan'), top = 0.2, bottom = 0.05) %>%
      dyRoller(rollPeriod = 1)%>%
      dyCrosshair(direction = "vertical")%>%
      presAnnotation("1987-1-1", text = staid_p)


  }

#' Title
#'
#' @param name
#' @param data
#' @param group_name
#'
#' @return
#' @export
#'
#' @examples
  monthly_flow_syncplot <- function(name,data,group_name){
    tm.an<-data %>% filter(ABBR_NAME == name)
    tm.an<-zoo(x=tm.an$read, order.by=tm.an$date)
    dygraph(tm.an,group=group_name) %>%
      # dyRangeSelector()%>%
      dyBarChart() %>%
      dyAxis("y", label = "Monthly flows (acre-ft)")%>%
      presAnnotation("1987-1-1", text = name)
  }

  #' Title
  #'
  #' @param name
  #' @param data
  #' @param group_name
  #'
  #' @return
  #' @export
  #'
  #' @examples
  monthly_flow_syncplot_staid <- function(staid_p,data,group_name){
    tm.an<-data %>% filter(staid == staid_p)
    tm.an<-zoo(x=tm.an$read, order.by=tm.an$date)
    dygraph(tm.an,group=group_name) %>%
      # dyRangeSelector()%>%
      dyBarChart() %>%
      dyAxis("y", label = "Monthly flows (acre-ft)")%>%
      presAnnotation("1987-1-1", text = staid_p)
  }

  monthly_uses_syncplot<- function(staid_p,data,group_name){
    tm.an<-data %>% filter(staid == staid_p)
    tm.an<-zoo(x=tm.an$read, order.by=tm.an$date)
    dygraph(tm.an,group=group_name) %>%
      # dyRangeSelector()%>%
      dyBarChart() %>%
      dyAxis("y", label = "Monthly Uses (acre-ft)")%>%
      presAnnotation("1987-1-1", text = staid_p)
  }

#' Title
#'
#' @param name
#' @param data
#' @param group_name
#'
#' @return
#' @export
#'
#' @examples
monthly_pumping_syncplot <- function(staid_p,data,group_name){
  tm.an<-data %>% filter(staid == staid_p)
  tm.an<-zoo(x=tm.an$read, order.by=tm.an$date)
  dygraph(tm.an,group=group_name) %>%
    # dyRangeSelector()%>%
    dyBarChart() %>%
    dyAxis("y", label = "Monthly Pumping (acre-ft)")%>%
    presAnnotation("1987-1-1", text = staid_p)
}




tozoo <- function(x, well_name) {
  zoo(x$total_pumping, x$roy2)
}

# multiple well stacked plot
plot_linked_wells <- function(data, site_id, linked_wells_df) {
  # Filter the linked_wells dataframe to only include wells linked to the specified site_id
  site_linked_wells <- linked_wells_df %>%
    filter(Site == site_id) %>%
    pull(Linked_Well)

  # Ensure site_linked_wells is a vector
  if (length(site_linked_wells) == 1) {
    site_linked_wells <- as.character(site_linked_wells)
  }


  # Filter the data to include only the wells linked to the specified site_id
  filtered_data <- data %>%
    filter(staid %in% site_linked_wells) %>%
    mutate(read2 = ifelse(read < 0, 0, read)) %>%
    group_by(roy, staid) %>%
    summarise(total_pumping = sum(read)) %>%
    select(roy, staid, total_pumping) %>%
    mutate(roy2 = as.Date(paste0(roy, "-04-01"), format = "%Y-%m-%d"))
  # Format the roy2 column in filtered_data to %Y-%m-%d format
  # filtered_data$roy2 <- format(filtered_data$roy2, "%Y-%m-%d")


  # Replace NA values with 0
  filtered_data[is.na(filtered_data)] <- 0

  # Check if there is data available for plotting
  if (nrow(filtered_data) == 0) {
    message("No data available for plotting site ", site_id)
    return(NULL)
  }

  # Merge the filtered data by site_id
  merged_data <- do.call(merge, lapply(split(filtered_data, filtered_data$staid), tozoo))


  # Print the structure of merged_data
  # print(str(merged_data))

  # Plot the merged data
  dygraph(merged_data, group = "a") %>%
    dyStackedBarGroup(site_linked_wells) %>%
    dyAxis("y", label = "Total Annual Pumping (AF)") %>%
    dyOptions(stackedGraph = TRUE)  # Set stackedGraph option to TRUE
}

# Define a function to plot single-well data as a bar chart
plot_linked_wells_single <- function(data, site_id, linked_wells_df) {
  # Filter the linked_wells dataframe to only include wells linked to the specified site_id
  site_linked_wells <- linked_wells_df %>%
    filter(Site == site_id) %>%
    pull(Linked_Well)

  # Ensure site_linked_wells is a vector
  if (length(site_linked_wells) == 1) {
    site_linked_wells <- as.character(site_linked_wells)
  }

    filtered_data <- data %>%
    filter(staid %in% site_linked_wells) %>%
    mutate(read2 = ifelse(read < 0, 0, read)) %>%
    group_by(roy, staid) %>%
    summarise(total_pumping = sum(read)) %>%
    select(roy, staid, total_pumping) %>%
    mutate(roy2 = as.Date(paste0(roy, "-04-01"), format = "%Y-%m-%d"))

    # print(filtered_data)

  # Check if there is data available for plotting
  if (nrow(filtered_data) == 0) {
    message("No data available for plotting site ", site_id)
    return(NULL)
  }

    # Merge the filtered data by site_id
    merged_data <- do.call(merge, lapply(split(filtered_data, filtered_data$staid), tozoo))


  # Plot the data as a bar chart with the title as the staid
  dygraph(merged_data, group = "a", main = site_linked_wells) %>%
    dyBarChart() %>%
    dyAxis("y", label = "Total Annual Pumping (AF)") %>%
    dyOptions(stackedGraph = TRUE)
}



#' Title
#'
#' @param dygraph
#'
#' @return
#' @export
#'
#' @examples
  dyBarChart <- function(dygraph) {
    dyPlotter(dygraph = dygraph,
              name = "BarChart",
              path = system.file("plotters/barchart.js",
                                 package = "dygraphs"))
  }


#' Title
#'
#' @param dygraph
#' @param direction
#'
#' @return
#' @export
#'
#' @examples
  dyCrosshair <- function(dygraph,
                          direction = c("both", "horizontal", "vertical")) {
    dyPlugin(
      dygraph = dygraph,
      name = "Crosshair",
      path = system.file("plugins/crosshair.js",
                         package = "dygraphs"),
      options = list(direction = match.arg(direction))
    )
  }
  # VWR----


  #' calculate weighted average vwr per lai
  #' for the non-focal species without T rates
  #'
  #' @param vwr
  #'
  #' @return weighted average vwr per leaf area index dataframe
  #' @export
  #'
  #' @examples
  weighted_avg <- function(vwr){
    vwr %>% filter(species != 'OTHER') %>% filter(all.hits >0) %>%
      group_by(site,period) %>%
      summarise(w_avg_vwr_per_lai = sum(vwr)/sum(lai)) %>%
      mutate(species = 'OTHER')
  }

  #' join vwr and weighted avg vwr for 'other species' total and total the two for a
  #'
  #' @param vwr
  #' @param weighted.avg.
  #'
  #' @return vwr totals in dataframe
  #' @export
  #'
  #' @examples
  vwr_total <- function(vwr, weighted.avg.){
    vwr %>% left_join(weighted.avg., by = c('site', 'species', 'period')) %>%
      mutate(other.vwr = w_avg_vwr_per_lai * lai,
             total.vwr = case_when(!is.na(vwr)~vwr,
                                   !is.na(other.vwr)~other.vwr),
             site.f = factor(site, levels = c("LW1",
                                              "LW2",
                                              "LW3",
                                              "BC1",
                                              "BC2",
                                              "BC3",
                                              "BP1",
                                              "BP2",
                                              "BP3",
                                              "BP4",
                                              "TA3",
                                              "TA4",
                                              "TA5",
                                              "TA6",
                                              "TAC",
                                              "TS1",
                                              "TS2",
                                              "TS3",
                                              "TS4",
                                              "TSC",
                                              "IO1",
                                              "IO2",
                                              "IC1",
                                              "IC2",
                                              "SS1",
                                              "SS2",
                                              "SS3",
                                              "SS4",
                                              "BG2",
                                              "BGC")
             )
      )

  }


  #' total VWR from species to site level
  #' and for each period
  #' @param vwr.total
  #' @param cYear
  #'
  #' @returnwide wide data frame with period as columns and VWR by site rows
  #' @export
  #'
  #' @examples
  vwr_site_total_period <- function(vwr.total,cYear){
    vwr.total %>% filter(all.hits > 0) %>%
      select(site.f,period,species,lai,total.vwr) %>%
      group_by(site.f,period) %>%
      summarise(site.vwr = sum(total.vwr)) %>%
      pivot_wider(names_from = period, values_from = site.vwr)
  }

  plot_awc_dygraph <- function(data,site_id){
    awc2<-data %>% filter(site == site_id)

    awc.z <- zoo(cbind(awc=awc2$awc,vwr=awc2$vwr), order.by=awc2$date)

    dygraph(awc.z, group = 'a') %>%
      dyAxis("y", label = "(cm)") %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE)
  }


  plot_dtw_dygraph <- function(data,site_id){
    dtw2<-data %>% filter(site == site_id)
    dtw.z<-dtw2 %>% select(date,dtw) %>% zoo(dtw2$dtw, order.by=dtw2$date)

    dygraph(dtw.z, group = 'a') %>% dyRangeSelector(height = 20, strokeColor = "")%>%
      dyAxis("y", label = "Depth (m)",valueRange = c(10, 0)) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE)

  }


  plot_on_off <- function(data, site_id) {
    on.off2 <- data %>% filter(site == site_id)

    on.off1z <- on.off2 %>% select(date, on.off.1) %>%
      zoo::zoo(., order.by = .$date)

    dygraph(on.off1z, group = 'a', main = 'open bar denotes on-status') %>%
      dyAxis("y", valueRange = c(1, 8))  %>%
      dyOptions(stepPlot = TRUE, drawPoints = TRUE, pointSize = 2, fillGraph = TRUE, includeZero = TRUE) %>%
      dyLegend(show = "follow")
  }
  # plot_on_off <- function(data,site_id){
  #   on.off2<-data %>% filter(site == site_id)
  #
  #   on.off1z<-on.off2 %>% select(date,on.off.1) %>% zoo(on.off2$on.off.1, order.by=on.off2$date)
  #
  #   #on/off bar
  #   dygraph(on.off1z, group = 'a',main = 'Open bar denotes on-status') %>%
  #     dyAxis("y", valueRange = c(1, 11))  %>%
  #     dyOptions(stepPlot = TRUE,drawPoints = TRUE, pointSize = 2,fillGraph = TRUE,includeZero = TRUE)%>%dyLegend(show = "follow")
  #
  # }
