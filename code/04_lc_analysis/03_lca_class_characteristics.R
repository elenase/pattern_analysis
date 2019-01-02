result_lca <- read.csv("data/lca/result_lca_complete_16.csv")
result_lca$X <- NULL


##### visualization #####

color <- c("#d3000f", "#34884a", "#dd9816", "#00534e", "#dea890", "#ab4d1a", "#b5b63e", 
           "#396035", "#3e321a", "#631b1b", "#db4e23", "#eae583", "#bd6578", "#b72b3d")
i=2
for (i in 1:16) {
class.spec <- paste0("class_",i)
data <- result_lca[(result_lca$class==class.spec & result_lca$V1 >= 1),]

ggplot(data=data, aes(x=reorder(V1, -V1), y=V1, fill=V4)) +
  geom_bar(colour="black", stat="identity", width=0.8) +
  geom_hline(yintercept = 1, color= "red", size=1, linetype="dashed") + 
  scale_fill_manual(values=rev(color),
                    name="Variable") +
  scale_x_discrete(labels=reorder(data$grouping, -data$V1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title = element_text(size=11, face="plain", color = "#1a1e1a", hjust = 1),
        plot.title = element_text(size=17, face="plain", color = "#1a1e1a", hjust = 0),
        plot.subtitle = element_text(size=9, face="plain", color = "#414d41", hjust = 0))+
  labs(x = "Traits", y = "Multiples of average class-conditional probabilities",
       title = paste0("Characteristics of Class ",i), 
       subtitle = "Data Source: UNHCR proGres data")

ggsave(paste0("out/lca/class_comparison/class",i,".png"), width = 26, height = 12, units = c("cm"), dpi = 300)
}

