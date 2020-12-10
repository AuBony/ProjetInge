

# import ----
load("../data/Mars.RData")
chat2$nb_bk <- as.numeric(chat2$nb_bk)
chat2$nb_bit <- as.numeric(chat2$nb_bit)

# Model break ----
str(chat2)

mod_nb_bk <- lm(nb_bk ~ ., data = chat2)
summary(mod_nb_bk)

mod_nb_bk_1 <- lm(nb_bk ~ nb_bit, data = chat2)
summary(mod_nb_bk_1)
anova(mod_nb_bk_1)

# Model Bite ----
mod_nb_bt <- lm(nb_bit ~ ., data = chat2)
summary(mod_nb_bt)
