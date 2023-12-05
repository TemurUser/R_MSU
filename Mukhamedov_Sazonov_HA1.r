# Авторы - Мухмедов Темур, Сазонов Олег
library(dplyr)
library(haven)
library(stargazer)
library(data.table)
library(digest)
library(gt)
library(lmtest)
library(sandwich)

data1 <- read_dta("/Users/TemurMdov/Downloads/replication_final/data/GEF_dataset_final.dta")

##### пункт 2.1.4
#выбираем нужные переменные
data <- data1 %>% select(c("children", "num_children", "marital_status", "income", 
                           "source_inc", "voted_FN", "opinion_unemp", "treatment"))
names(data)

#преобразуем переменные, как в Таблице 1
data <- data %>% mutate(single = ifelse(marital_status == 1, 1, 0))
data <- data %>% mutate(married = ifelse(marital_status == 2, 1, 0))
data <- data %>% mutate(Source_of_income_wage = ifelse(source_inc == 1, 1, 0))
data <- data %>% mutate(Source_of_income_social_benefits = ifelse(source_inc == 6, 1, 0))
data <- data %>% mutate(Source_of_income_social_pension = ifelse(source_inc == 4, 1, 0))
data <- data %>% select(-c("marital_status", "source_inc"))

#разобьем выборку на 4 группы согласно treatment
data1$treatment

group_0 <- data %>% filter(treatment == 0)
group_1 <- data %>% filter(treatment == 1)
group_2 <- data %>% filter(treatment == 2)
group_3 <- data %>% filter(treatment == 3)

group_0 <- group_0 %>% select(-c("treatment"))
group_1 <- group_1 %>% select(-c("treatment"))
group_2 <- group_2 %>% select(-c("treatment"))
group_3 <- group_3 %>% select(-c("treatment"))

group_control <- as.data.frame(group_0)
group_alt_fact <- as.data.frame(group_1)
group_facts <- as.data.frame(group_2)
group_fact_check <- as.data.frame(group_3)

#посчитаем средние в рандомизированных группах (столбцы 1-4 Таблицы 1)
mean_control <- group_0 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                      voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                      single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                      Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean_alt_fact <- group_1 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                       voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                       single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                       Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean_facts <- group_2 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                    voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                    single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                    Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean_fact_check <- group_3 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                         voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                         single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                         Source_of_income_social_pension = mean(Source_of_income_social_pension))

#считаем p-value для тестов на равенство средних между группами (столбцы 5-10 Таблицы 1)
control_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group_control[,i], group_alt_fact[,i], alternative="two.sided")$p.value
  control_vs_alt_fact = append(control_vs_alt_fact, p)
}

control_vs_fact_check <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group_control[,i], group_fact_check[,i], alternative="two.sided")$p.value
  control_vs_fact_check = append(control_vs_fact_check, p)
}


control_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group_control[,i], group_facts[,i], alternative="two.sided")$p.value
  control_vs_facts = append(control_vs_facts, p)
}

fact_check_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group_fact_check[,i], group_alt_fact[,i], alternative="two.sided")$p.value
  fact_check_vs_alt_fact = append(fact_check_vs_alt_fact, p)
}

fact_check_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group_fact_check[,i], group_facts[,i], alternative="two.sided")$p.value
  fact_check_vs_facts = append(fact_check_vs_facts, p)
}

alt_fact_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group_alt_fact[,i], group_facts[,i], alternative="two.sided")$p.value
  alt_fact_vs_facts = append(alt_fact_vs_facts, p)
}

#можем заполнить Таблицу 1
table1 <- data.frame(t(mean_alt_fact), t(mean_fact_check), t(mean_facts), t(mean_control), control_vs_alt_fact,
                     control_vs_fact_check, control_vs_facts, fact_check_vs_alt_fact, fact_check_vs_facts, alt_fact_vs_facts)
rownames(table1)[8:9] <- c("income_wage", "income_pension")

table1 %>% gt(rownames_to_stub = TRUE) %>% fmt_number(
  decimals = 2,
  use_seps = FALSE
) %>%
  tab_header(title = "Table 1 - Balancing test across randomized groups") %>%
  cols_label(t.mean_fact_check. = html("mean<br>fact_check"), t.mean_alt_fact. = html("mean<br>alt_fact"),
             t.mean_facts. = html("mean<br>facts"),
             t.mean_control. = html("mean<br>control"),
             control_vs_alt_fact = html("control_vs<br>alt_fact"),
             control_vs_fact_check = html("control_vs<br>fact_check"),
             control_vs_facts = html("control_vs<br>facts"),
             fact_check_vs_alt_fact = html("fact_check_vs<br>alt_fact"),
             fact_check_vs_facts = html("fact_check<br>vs_facts"),
             alt_fact_vs_facts = html("alt_fact<br>vs_facts")) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_body()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_title()) 

##### пункт 2.2.1
#разминочная рандомизация (по порядковому номеру)
T <- rep(0:3, 620)
data$T <- T

# аналогично предыдущему пункту, разбиваем выборку на группы согласно treatment, 
# считаем средние и p-value и выводим Таблицу 1

group_00 <- data %>% filter(T == 0)
group_01 <- data %>% filter(T == 1)
group_02 <- data %>% filter(T == 2)
group_03 <- data %>% filter(T == 3)

group_00 <- group_00 %>% select(-c("treatment", "T"))
group_01 <- group_01 %>% select(-c("treatment", "T"))
group_02 <- group_02 %>% select(-c("treatment", "T"))
group_03 <- group_03 %>% select(-c("treatment", "T"))

group2_control <- as.data.frame(group_00)
group2_alt_fact <- as.data.frame(group_01)
group2_facts <- as.data.frame(group_02)
group2_fact_check <- as.data.frame(group_03)

mean2_control <- group_00 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                        voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                        single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                        Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean2_alt_fact <- group_01 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                         voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                         single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                         Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean2_facts <- group_02 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                      voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                      single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                      Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean2_fact_check <- group_03 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                           voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                           single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                           Source_of_income_social_pension = mean(Source_of_income_social_pension))

control2_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_control[,i], group2_alt_fact[,i], alternative="two.sided")$p.value
  control2_vs_alt_fact = append(control2_vs_alt_fact, p)
}

control2_vs_fact_check <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_control[,i], group2_fact_check[,i], alternative="two.sided")$p.value
  control2_vs_fact_check = append(control2_vs_fact_check, p)
}

control2_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_control[,i], group2_alt_fact[,i], alternative="two.sided")$p.value
  control2_vs_alt_fact = append(control2_vs_alt_fact, p)
}

control2_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_control[,i], group2_facts[,i], alternative="two.sided")$p.value
  control2_vs_facts = append(control2_vs_facts, p)
}

fact2_check_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_fact_check[,i], group2_alt_fact[,i], alternative="two.sided")$p.value
  fact2_check_vs_alt_fact = append(fact2_check_vs_alt_fact, p)
}

fact2_check_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_fact_check[,i], group2_facts[,i], alternative="two.sided")$p.value
  fact2_check_vs_facts = append(fact2_check_vs_facts, p)
}

alt2_fact_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group2_alt_fact[,i], group2_facts[,i], alternative="two.sided")$p.value
  alt2_fact_vs_facts = append(alt2_fact_vs_facts, p)
}

table2 <- data.frame(t(mean2_alt_fact), t(mean2_fact_check), t(mean2_facts), t(mean2_control), control2_vs_alt_fact,
                     control2_vs_fact_check, control2_vs_facts, fact2_check_vs_alt_fact, fact2_check_vs_facts, alt2_fact_vs_facts)
rownames(table2)[8:9] <- c("income_wage", "income_pension")
table2 %>% gt(rownames_to_stub = TRUE) %>% fmt_number(
  decimals = 2,
  use_seps = FALSE
) %>%
  tab_header(title = "Table 2 - Balancing test across randomized groups") %>%
  cols_label(t.mean2_fact_check. = html("mean<br>fact_check"), t.mean2_alt_fact. = html("mean<br>alt_fact"),
             t.mean2_facts. = html("mean<br>facts"),
             t.mean2_control. = html("mean<br>control"),
             control2_vs_alt_fact = html("control_vs<br>alt_fact"),
             control2_vs_fact_check = html("control_vs<br>fact_check"),
             control2_vs_facts = html("control_vs<br>facts"),
             fact2_check_vs_alt_fact = html("fact_check_vs<br>alt_fact"),
             fact2_check_vs_facts = html("fact_check<br>vs_facts"),
             alt2_fact_vs_facts = html("alt_fact<br>vs_facts")) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_body()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_title()) 


### пункт 2.2.2
#вторая рандомизация (устойчивая к перестановке строк), 
# вся процедура аналогична предыдущим пунктам (только используем хеш-функцию)

hashes <- sapply(data1$responseid, function(x) {digest(x, algo="murmur32")})
result <- strtoi(substring(hashes, 2), base=16) 
result
T2 <- ifelse(result %% 100 < 25, 0, 
             ifelse(result %% 100 >= 25 & result %% 100 < 50, 1,
                    ifelse(result %% 100 >= 50 & result %% 100 < 75, 2, 3)))
data$T2 <- T2

group_10 <- data %>% filter(T2 == 0)
group_11 <- data %>% filter(T2 == 1)
group_12 <- data %>% filter(T2 == 2)
group_13 <- data %>% filter(T2 == 3)

group_10 <- group_10 %>% select(-c("treatment", "T2"))
group_11 <- group_11 %>% select(-c("treatment", "T2"))
group_12 <- group_12 %>% select(-c("treatment", "T2"))
group_13 <- group_13 %>% select(-c("treatment", "T2"))

group3_control <- as.data.frame(group_10)
group3_alt_fact <- as.data.frame(group_11)
group3_facts <- as.data.frame(group_12)
group3_fact_check <- as.data.frame(group_13)

mean3_control <- group_10 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                        voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                        single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                        Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean3_alt_fact <- group_11 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                         voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                         single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                         Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean3_facts <- group_12 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                      voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                      single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                      Source_of_income_social_pension = mean(Source_of_income_social_pension))
mean3_fact_check <- group_13 %>% summarise(children = mean(children), num_children = mean(num_children, na.rm = TRUE), income = mean(income),
                                           voted_FN = mean(voted_FN), opinion_unemp = mean(opinion_unemp),
                                           single = mean(single), married = mean(married), Source_of_income_wage = mean(Source_of_income_wage),
                                           Source_of_income_social_pension = mean(Source_of_income_social_pension))

control3_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_control[,i], group3_alt_fact[,i], alternative="two.sided")$p.value
  control3_vs_alt_fact = append(control3_vs_alt_fact, p)
}

control3_vs_fact_check <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_control[,i], group3_fact_check[,i], alternative="two.sided")$p.value
  control3_vs_fact_check = append(control3_vs_fact_check, p)
}

control3_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_control[,i], group3_alt_fact[,i], alternative="two.sided")$p.value
  control3_vs_alt_fact = append(control3_vs_alt_fact, p)
}

control3_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_control[,i], group3_facts[,i], alternative="two.sided")$p.value
  control3_vs_facts = append(control3_vs_facts, p)
}

fact3_check_vs_alt_fact <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_fact_check[,i], group3_alt_fact[,i], alternative="two.sided")$p.value
  fact3_check_vs_alt_fact = append(fact3_check_vs_alt_fact, p)
}

fact3_check_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_fact_check[,i], group3_facts[,i], alternative="two.sided")$p.value
  fact3_check_vs_facts = append(fact3_check_vs_facts, p)
}

alt3_fact_vs_facts <- c()

for (i in seq(1, 9, 1)) {
  p = t.test(group3_alt_fact[,i], group3_facts[,i], alternative="two.sided")$p.value
  alt3_fact_vs_facts = append(alt3_fact_vs_facts, p)
}

table3 <- data.frame(t(mean3_alt_fact), t(mean3_fact_check), t(mean3_facts), t(mean3_control), control3_vs_alt_fact,
                     control3_vs_fact_check, control3_vs_facts, fact3_check_vs_alt_fact, fact3_check_vs_facts, alt3_fact_vs_facts)

rownames(table3)[8:9] <- c("income_wage", "income_pension")
table3 %>% gt(rownames_to_stub = TRUE) %>% fmt_number(
  decimals = 2,
  use_seps = FALSE
) %>%
  tab_header(title = "Table 3 - Balancing test across randomized groups") %>%
  cols_label(t.mean3_fact_check. = html("mean<br>fact_check"), 
             t.mean3_alt_fact. = html("mean<br>alt_fact"),
             t.mean3_facts. = html("mean<br>facts"),
             t.mean3_control. = html("mean<br>control"),
             control3_vs_alt_fact = html("control_vs<br>alt_fact"),
             control3_vs_fact_check = html("control_vs<br>fact_check"),
             control3_vs_facts = html("control_vs<br>facts"),
             fact3_check_vs_alt_fact = html("fact_check_vs<br>alt_fact"),
             fact3_check_vs_facts = html("fact_check<br>vs_facts"),
             alt3_fact_vs_facts = html("alt_fact<br>vs_facts")) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_body()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_title()) 

### пункт 2.2.3
#множественное тестрирование гипотез (с помощью поправки Бонферрони)
#используем 10%-й уровень значимости
alpha5 = 0.05 / 6
alpha1 = 0.1 / 6
alpha01 = 0.01 / 6

#H0: все нулевые гипотезы верны (все средние между группами равны); если принимается, выводим No
Vector <- (as.data.frame(table1[,5:10] <= alpha1)) %>% 
  mutate(mult_hyp = ifelse(rowSums(as.data.frame(table1[,5:10] <= alpha1)) > 0, "Yes", "No"))

# выводим Таблицу 1 вместе с 11 столбцом
table1 <- data.frame(t(mean_alt_fact), t(mean_fact_check), t(mean_facts), t(mean_control), control_vs_alt_fact,
                     control_vs_fact_check, control_vs_facts, fact_check_vs_alt_fact, fact_check_vs_facts, alt_fact_vs_facts, Vector$mult_hyp)
rownames(table1)[8:9] <- c("income_wage", "income_pension")

table1$mult_hyp <- Vector$mult_hyp
table1 %>% gt(rownames_to_stub = TRUE) %>% fmt_number(
  decimals = 2,
  use_seps = FALSE
) %>%
  tab_header(title = "Table 1 - Balancing test across randomized groups") %>%
  cols_label(t.mean_fact_check. = html("mean<br>fact_check"), t.mean_alt_fact. = html("mean<br>alt_fact"),
             t.mean_facts. = html("mean<br>facts"),
             t.mean_control. = html("mean<br>control"),
             control_vs_alt_fact = html("control_vs<br>alt_fact"),
             control_vs_fact_check = html("control_vs<br>fact_check"),
             control_vs_facts = html("control_vs<br>facts"),
             fact_check_vs_alt_fact = html("fact_check_vs<br>alt_fact"),
             fact_check_vs_facts = html("fact_check<br>vs_facts"),
             alt_fact_vs_facts = html("alt_fact<br>vs_facts")) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_body()) %>%
  tab_style(
    style = cell_text(align = "center"), locations = cells_title()) 


##### пункт 2.3.1
# воспроизводим Таблицу А2

data1$Alt_fact <- ifelse(data1$treatment == 1, 1, 0)
data1$facts <- ifelse(data1$treatment == 2, 1, 0)
data1$fact_check <- ifelse(data1$treatment == 3, 1, 0)

data1 <- as.data.frame(data1)

# регрессии
# зависимая переменная - will vote for MLP
data1$willvote_FN

# для 1-й регрессии в качестве контрольных переменных используем только strata controls
mod1 <- lm(data = data1, willvote_FN  ~  Alt_fact + fact_check + facts
           + education + sexe + age)
# для 2-й регрессии в контрольные переменные добавляем individual controls
data1 <- data1 %>% mutate(single = ifelse(marital_status == 1, 1, 0))
data1 <- data1 %>% mutate(married = ifelse(marital_status == 2, 1, 0))
data1 <- data1 %>% mutate(Source_of_income_wage = ifelse(source_inc == 1, 1, 0))
data1 <- data1 %>% mutate(Source_of_income_social_benefits = ifelse(source_inc == 6, 1, 0))
data1 <- data1 %>% mutate(Source_of_income_social_pension = ifelse(source_inc == 4, 1, 0))
data1 <- data1 %>% select(-c("marital_status", "source_inc"))


mod2 <- lm(data = data1, willvote_FN  ~  Alt_fact + fact_check + facts
           + education + sexe + age
           + children + married + single + income 
           + Source_of_income_wage + Source_of_income_social_benefits 
           + Source_of_income_social_pension)

# для 3-й регрессии в контрольные переменные добавляем prior voting controls
data1 <- data1 %>% mutate(Hollande = ifelse(whomvoted_past == 1, 1, 0))
data1 <- data1 %>% mutate(Sarozy = ifelse(whomvoted_past == 2, 1, 0))
data1 <- data1 %>% mutate(Mellenchon = ifelse(whomvoted_past == 3, 1, 0))
data1 <- data1 %>% mutate(Le_Pen = ifelse(whomvoted_past == 4, 1, 0))
data1 <- data1 %>% mutate(other_candidate = ifelse(whomvoted_past == 8, 1, 0))
data1 <- data1 %>% mutate(did_not_vote = ifelse(whomvoted_past == 6, 1, 0))


mod3 <- lm(data = data1, willvote_FN  ~  Alt_fact + fact_check + facts
           + education + sexe + age
           + children + married + single + income 
           + Source_of_income_wage + Source_of_income_social_benefits 
           + Source_of_income_social_pension + voted_FN + Hollande
           + Sarozy + Mellenchon + Le_Pen + other_candidate + did_not_vote)
library(stargazer)
stargazer(mod1, mod2, mod3, type = "text", digits = 4)

# для 4-й регрессии в контрольные переменные добавляем interactions of prior voting with treatment,
# причем используем demeaned measures of past voting behaviour and treatment dummies - вычитаем средние из значений
data1 <- data1 %>% mutate(Hollande_d = Hollande - mean(Hollande))
data1 <- data1 %>% mutate(Sarozy_d = Sarozy - mean(Sarozy))
data1 <- data1 %>% mutate(Mellenchon_d = Mellenchon - mean(Mellenchon))
data1 <- data1 %>% mutate(Le_Pen_d = Le_Pen - mean(Le_Pen))
data1 <- data1 %>% mutate(other_candidate_d = other_candidate - mean(other_candidate))
data1 <- data1 %>% mutate(did_not_vote_d = did_not_vote - mean(did_not_vote))
data1 <- data1 %>% mutate(voted_FN_d = voted_FN - mean(voted_FN))


mod4 <- lm(data = data1, willvote_FN  ~  Alt_fact + fact_check + facts
           + education + sexe + age
           + children + married + single + income 
           + Source_of_income_wage + Source_of_income_social_benefits 
           + Source_of_income_social_pension + voted_FN + Hollande
           + Sarozy + Mellenchon + Le_Pen + other_candidate + did_not_vote 
           + voted_FN_d*Alt_fact + Hollande_d*Alt_fact
           + Sarozy_d*Alt_fact + Mellenchon_d*Alt_fact + Le_Pen_d*Alt_fact 
           + other_candidate_d*Alt_fact + did_not_vote_d*Alt_fact 
           + voted_FN_d*facts + Hollande_d*facts
           + Sarozy_d*facts + Mellenchon_d*facts + Le_Pen_d*facts 
           + other_candidate_d*facts + did_not_vote_d*facts 
           + voted_FN_d*fact_check + Hollande_d*fact_check
           + Sarozy_d*fact_check + Mellenchon_d*fact_check + Le_Pen_d*fact_check 
           + other_candidate_d*fact_check + did_not_vote_d*fact_check)

stargazer(mod1, mod2, mod3, mod4, type = "html", out = "will_vote_MLP.html", digits = 4, omit = c("education", "sexe", "age",
                                                                          "children","married", "single", "income",
                                                                          "Source_of_income_wage", "Source_of_income_social_benefits", 
                                                                          "Source_of_income_social_pension", "voted_FN", "Hollande",
                                                                          "Sarozy", "Mellenchon", "Le_Pen", "other_candidate", "did_not_vote", 
                                                                          "voted_FN_d", "Hollande_d",
                                                                          "Sarozy_d", "Mellenchon_d", "Le_Pen_d", 
                                                                          "other_candidate_d", "did_not_vote_d", 
                                                                          "voted_FN_d", "Hollande_d",
                                                                          "Sarozy_d", "Mellenchon_d", "Le_Pen_d",
                                                                          "other_candidate_d", "did_not_vote_d",
                                                                          "voted_FN_d", "Hollande_d",
                                                                          "Sarozy_d", "Mellenchon_d", "Le_Pen_d", 
                                                                          "other_candidate_d", "did_not_vote_d", "Constant"))


#при добавлении робастных стандартных ошибок значения почти те же
summary(mod4)
coeftest(mod4, vcov = vcovHC(mod4, type = 'HC0'))

# зависимая переменная - reason for migration: economic
names(data1)
data1$reason_mig

data1 <- data1 %>% mutate(economic = ifelse(reason_mig == 1, 1, 0))

mod11 <- lm(data = data1, economic  ~  Alt_fact + fact_check + facts
            + education + sexe + age)
summary(mod11)

mod12 <- lm(data = data1, economic  ~  Alt_fact + fact_check + facts  
            + education + sexe + age
            + children + married + single + income 
            + Source_of_income_wage + Source_of_income_social_benefits 
            + Source_of_income_social_pension)
summary(mod12)



mod13 <- lm(data = data1, economic  ~  Alt_fact + fact_check + facts 
            + education + sexe + age
            + children + married + single + income 
            + Source_of_income_wage + Source_of_income_social_benefits 
            + Source_of_income_social_pension + voted_FN + Hollande
            + Sarozy + Mellenchon + Le_Pen + other_candidate + did_not_vote)

mod14 <- lm(data = data1, economic  ~  Alt_fact + fact_check + facts
           + education + sexe + age
           + children + married + single + income 
           + Source_of_income_wage + Source_of_income_social_benefits 
           + Source_of_income_social_pension + voted_FN + Hollande
           + Sarozy + Mellenchon + Le_Pen + other_candidate + did_not_vote 
           + voted_FN_d*Alt_fact + Hollande_d*Alt_fact
           + Sarozy_d*Alt_fact + Mellenchon_d*Alt_fact + Le_Pen_d*Alt_fact 
           + other_candidate_d*Alt_fact + did_not_vote_d*Alt_fact 
           + voted_FN_d*facts + Hollande_d*facts
           + Sarozy_d*facts + Mellenchon_d*facts + Le_Pen_d*facts 
           + other_candidate_d*facts + did_not_vote_d*facts 
           + voted_FN_d*fact_check + Hollande_d*fact_check
           + Sarozy_d*fact_check + Mellenchon_d*fact_check + Le_Pen_d*fact_check 
           + other_candidate_d*fact_check + did_not_vote_d*fact_check)
stargazer(mod11, mod12, mod13, mod14, type = "html", out = "economic.html", digits = 4, omit = c("education", "sexe", "age",
                                                                                                  "children","married", "single", "income",
                                                                                                  "Source_of_income_wage", "Source_of_income_social_benefits", 
                                                                                                  "Source_of_income_social_pension", "voted_FN", "Hollande",
                                                                                                  "Sarozy", "Mellenchon", "Le_Pen", "other_candidate", "did_not_vote", 
                                                                                                  "voted_FN_d", "Hollande_d",
                                                                                                  "Sarozy_d", "Mellenchon_d", "Le_Pen_d", 
                                                                                                  "other_candidate_d", "did_not_vote_d", 
                                                                                                  "voted_FN_d", "Hollande_d",
                                                                                                  "Sarozy_d", "Mellenchon_d", "Le_Pen_d",
                                                                                                  "other_candidate_d", "did_not_vote_d",
                                                                                                  "voted_FN_d", "Hollande_d",
                                                                                                  "Sarozy_d", "Mellenchon_d", "Le_Pen_d", 
                                                                                                  "other_candidate_d", "did_not_vote_d", "Constant"))

#при добавлении робастных стандартных ошибок значения почти те же
summary(mod14)
coeftest(mod14, vcov = vcovHC(mod14, type = 'HC0'))


#зависимая переменная - agree with MLP's immigration policy
data1$disagree_FN
data1 <- data1 %>% mutate(agree = ifelse(disagree_FN == 1|disagree_FN == 2, 1, 0))

mod21 <- lm(data = data1, agree  ~  Alt_fact + fact_check + facts
            + education + sexe + age)


mod22 <- lm(data = data1, agree  ~  Alt_fact + fact_check + facts 
            + education + sexe + age
            + children + married + single + income 
            + Source_of_income_wage + Source_of_income_social_benefits 
            + Source_of_income_social_pension)
summary(mod22)



mod23 <- lm(data = data1, agree  ~  Alt_fact+ fact_check + facts 
            + education + sexe + age
            + children + married + single + income + Source_of_income_wage + Source_of_income_social_benefits 
            + Source_of_income_social_pension + voted_FN + Hollande
            + Sarozy + Mellenchon + Le_Pen + other_candidate + did_not_vote)

mod24 <- lm(data = data1, agree  ~  Alt_fact + fact_check + facts
           + education + sexe + age
           + children + married + single + income 
           + Source_of_income_wage + Source_of_income_social_benefits 
           + Source_of_income_social_pension + voted_FN + Hollande
           + Sarozy + Mellenchon + Le_Pen + other_candidate + did_not_vote 
           + voted_FN_d*Alt_fact + Hollande_d*Alt_fact
           + Sarozy_d*Alt_fact + Mellenchon_d*Alt_fact + Le_Pen_d*Alt_fact 
           + other_candidate_d*Alt_fact + did_not_vote_d*Alt_fact 
           + voted_FN_d*facts + Hollande_d*facts
           + Sarozy_d*facts + Mellenchon_d*facts + Le_Pen_d*facts 
           + other_candidate_d*facts + did_not_vote_d*facts 
           + voted_FN_d*fact_check + Hollande_d*fact_check
           + Sarozy_d*fact_check + Mellenchon_d*fact_check + Le_Pen_d*fact_check 
           + other_candidate_d*fact_check + did_not_vote_d*fact_check)

stargazer(mod21, mod22, mod23, mod24, type = "html", out = "agree.html", digits = 4, omit = c("education", "sexe", "age",
                                                                                                  "children","married", "single", "income",
                                                                                                  "Source_of_income_wage", "Source_of_income_social_benefits", 
                                                                                                  "Source_of_income_social_pension", "voted_FN", "Hollande",
                                                                                                  "Sarozy", "Mellenchon", "Le_Pen", "other_candidate", "did_not_vote", 
                                                                                                  "voted_FN_d", "Hollande_d",
                                                                                                  "Sarozy_d", "Mellenchon_d", "Le_Pen_d", 
                                                                                                  "other_candidate_d", "did_not_vote_d", 
                                                                                                  "voted_FN_d", "Hollande_d",
                                                                                                  "Sarozy_d", "Mellenchon_d", "Le_Pen_d",
                                                                                                  "other_candidate_d", "did_not_vote_d",
                                                                                                  "voted_FN_d", "Hollande_d",
                                                                                                  "Sarozy_d", "Mellenchon_d", "Le_Pen_d", 
                                                                                                  "other_candidate_d", "did_not_vote_d", "Constant"))
#при добавлении робастных стандартных ошибок значения почти те же
summary(mod24)