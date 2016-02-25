hepa <- read.csv("C:\\Users\\Kira\\SkyDrive\\Journocode\\Präsentationen\\Hepatitis.csv", sep=",")
library(dplyr)
library(reshape2)
library(ggplot2)


hepa <- melt(hepa, id.var=c("YEAR", "WEEK"))

colnames(hepa) <- c("Jahr", "Woche", "Staat", "Cases")

hepa$Cases <- as.numeric(hepa$Cases)

hdf <- hepa %>% 
       group_by(Staat, Jahr) %>% 
       summarise(c=if(all(is.na(Cases))) NA 
       else sum(Cases, na.rm=T))

hdf$Staat <- factor(hdf$Staat, levels=rev(levels(hdf$Staat)))

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}


levels(hdf$Staat) <- sapply(as.character(levels(hdf$Staat)), 
                            function(i) .simpleCap(gsub("\\.", " ", i)))                                                  

# hack together a colourbar
cols <- c(colorRampPalette(c("#d2e3f6", "#c9e2f6", "#95cbee", "#0099dc",
                             "#4ab04a", "#ffd73e"))(10),
          colorRampPalette(c("#eec73a", "#e29421",
                             "#e29421", "#f05336","#ce472e"),
                           bias=2)(90))

pdf("hepatitis_journocode.pdf", 8, 6)
ggplot(hdf, aes(y=Staat, x=Jahr, fill=c)) + 
  geom_tile(colour="white") + theme_minimal() +#linewidth=2, width=.9, height=.9
  scale_fill_gradientn(colours=cols, limits=c(0, 300),
                       breaks=seq(0, 3e2, by=1e2), 
                       na.value=rgb(246, 246, 246, max=255),
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             barheight=.5, label=T,
                                             barwidth=10)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(1966, 2012, by=10)) +
  geom_segment(x=1995, xend=1995, y=0, yend=51.5, size=.9, lineend = "round") +
  labs(x="", y="", fill="") +
  ggtitle("Hepatitis A") +
  theme(legend.position=c(.5, -.13),
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, hjust=1),
        axis.text.x=element_text(size=8),
        axis.line.x=element_line(colour="grey20", size=2),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(colour="grey20"),
        axis.ticks.length=grid::unit(.1, "cm"),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1)) +
  annotate("text", label="Vaccine introduced", x=1995, y=53, vjust=1, hjust=0,
           size=I(3))
dev.off()