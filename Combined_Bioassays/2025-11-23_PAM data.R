




all_FvFm = read_excel("2025_combinations_master.xlsx", sheet = "all_FvFm")

glimpse(all_FvFm)



carb_ANOVA <- aov(FvFm_C ~ Carbamazepine, data = all_FvFm)
summary(carb_ANOVA)

carb_REGW = REGW.test(carb_ANOVA, "Carbamazepine", DFerror, MSerror, alpha = 0.05, group=TRUE)
print(carb_REGW)


dic_ANOVA <- aov(FvFm_D ~ Diclofenac, data = all_FvFm)
summary(dic_ANOVA)

dic_REGW = REGW.test(dic_ANOVA, "Diclofenac", DFerror, MSerror, alpha = 0.05, group=TRUE)
print(dic_REGW)



PFOS_ANOVA <- aov(FvFm_P ~ PFOS, data = all_FvFm)
summary(PFOS_ANOVA)

PFOS_REGW = REGW.test(PFOS_ANOVA, "PFOS", DFerror, MSerror, alpha = 0.05, group=TRUE)
print(PFOS_REGW)




quin_ANOVA <- aov(FvFm_Q ~ Quinone, data = all_FvFm)
summary(quin_ANOVA)

quin_REGW = REGW.test(quin_ANOVA, "Quinone", DFerror, MSerror, alpha = 0.05, group=TRUE)
print(quin_REGW)

