load("data/data_agrocampus.RData")

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]

summary(chat)
table(chat$Cat_name, chat$Session)
table(chat$Cat_name, chat$Diet)
