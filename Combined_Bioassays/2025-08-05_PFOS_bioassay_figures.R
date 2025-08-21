




PFOS_RFU = read_excel("Contaminant_Bioassays/2025_combinations_master.xlsx", sheet = "PFOS_RFU")

PFOS_RFU$Group <- factor(PFOS_RFU$Group,
                              levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrients only", "nutrients + solvent", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                              labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

PFOS_box = ggplot() +
  geom_boxplot(aes(x = Group, 
                   y = `72h`),
               fill = "lightblue4",
               data = PFOS_RFU) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_box, filename = "PFOS_box.png",
       device = "png", height = 7, width = 11)



PFOS_bargraphs = read_excel("Bioassay poster graphs.xlsx", sheet = "Sheet4")

PFOS_bargraphs$Group <- factor(PFOS_bargraphs$Group,
                         levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrients only", "nutrients + solvent", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                         labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

RFU_PFOS = ggplot() +
  geom_col(aes(x = Group, 
               y = avg_delta),
           fill = "lightblue4",
           data = PFOS_bargraphs) +
  geom_errorbar(data = PFOS_bargraphs, aes(x = Group, 
                                     y = avg_delta,
                                     ymin = avg_delta-sd_delta,
                                     ymax = avg_delta+sd_delta)) +
  xlab("Treatment") +
  ylab("Change in RFU") +
  scale_y_continuous(limits = c(-2, 8)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(RFU_PFOS, filename = "RFU_PFOS.png",
       device = "png", height = 7, width = 11)



PFOS_pd = read_excel("Bioassay poster graphs.xlsx", sheet = "Sheet5")

PFOS_pd$Group <- factor(PFOS_pd$Group,
                               levels = c("PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                               labels = c("P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))
  
  
  
pd_PFOS = ggplot() +
  geom_col(aes(x = Group, 
               y = avg_pd),
           fill = "lightblue4",
           data = PFOS_pd) +
  geom_errorbar(data = PFOS_pd, aes(x = Group, 
                                     y = avg_pd,
                                     ymin = avg_pd-sd_pd,
                                     ymax = avg_pd+sd_pd)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-250, 325)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_PFOS, filename = "pd_PFOS.png",
       device = "png", height = 7, width = 11)



scale_y_continuous(limits = c(-250, 325)) +
  
  