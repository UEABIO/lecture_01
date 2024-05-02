# An analysis of average June temperatures on the body size of a univoltine butterfly species

# Packages====
library(tidyverse)
library(janitor)
library(car)
library(lmtest)
library(emmeans)

# Dataset====

butterfly_df <- read_csv("../data/univoltine_butterfly.csv")

# Data cleaning====

colnames(butterfly_df)

butterfly_df <- janitor::clean_names(butterfly_df)

nrow(butterfly_df)
# number of rows of dataframe

hist(butterfly_df$rain_jun)
# outlier detected

butterfly_df %>%
  ggplot(aes(x = rain_jun)) +
  geom_histogram()

hist(butterfly_df$forewing_length)
# no outliers

hist(butterfly_df$jun_mean)
# no outliers

unique(butterfly_df$sex)
# typos detected

## Fix dataset===

butterfly_clean <- butterfly_df %>%
   mutate(sex = case_when(sex == "Female" ~ "Females",
                             sex == "Maes" ~ "Males",
                             .default = as.character(sex)))
unique(butterfly_clean$sex)

# How many males and females
butterfly_clean %>%
  group_by(sex) %>%
  summarise(n = n())

# Missingness

butterfly_clean %>%
  group_by(sex) %>%
  summarize(across(everything(), ~sum(is.na(.))))

skimr::skim(butterfly_clean)

# Exploratory Analysis

GGally::ggpairs(butterfly_clean,
                aes(colour = sex))

butterfly_clean %>%
  ggplot(aes(x = rain_jun,
             y = forewing_length,
             colour = sex))+
  geom_point()+
  scale_x_continuous(limits = c(0,100))


butterfly_clean %>%
  ggplot(aes(x = jun_mean,
             y = forewing_length,
             colour = sex))+
  geom_point()+
  geom_smooth(method = "lm")

# Model====

model <- lm(forewing_length ~ jun_mean + sex + rain_jun + jun_mean:sex,
            data = butterfly_clean)

summary(model)

plot(model)

performance::check_model(model, detrend = F)

# issues - multicollinearity
# issues - outlier

butterfly_clean[19,]


model2 <- lm(forewing_length ~ jun_mean + sex + rain_jun + jun_mean:sex,
            data = butterfly_clean[-19,])
summary(model2)

plot(model2)


## Check model fit

car::boxCox(model2)
# indicates a log transformation could be suitable?

lmtest::bptest(model2)

car::qqPlot(model2) # adds a confidence interval check

shapiro.test(residuals(model2))



# Could also check if outlier is still an issue when dependent variable transformed?
model3 <- lm(log(forewing_length) ~ jun_mean + sex + rain_jun + jun_mean:sex,
             data = butterfly_clean[-19,])
summary(model3)


b <- car::boxCox(model2)

lambda <- b$x[which.max(b$y)]
lambda


model4 <- lm(((forewing_length^lambda-1)/lambda) ~ jun_mean + sex + rain_jun + jun_mean:sex,
             data = butterfly_clean[-19,])
summary(model4)


# Test interactions
summary(model2)
drop1(model2, test = "F")


# Interaction term does not explain significant variance (report this)

model_2a <- lm(forewing_length ~ jun_mean + sex + rain_jun,
              data = butterfly_clean[-19,])

summary(model_2a)


## Summarise model===

model_sum <- emmeans::emmeans(model_2a, specs = ~jun_mean + sex,
                 at =list(jun_mean = c(11.8:16.4))) %>%
  as_tibble()


butterfly_clean %>%
  ggplot(aes(x = jun_mean,
             y = forewing_length,
             fill = sex))+
  geom_ribbon(data = model_sum,
              aes(x = jun_mean,
                  y = emmean,
                  ymin = lower.CL,
                  ymax = upper.CL,
                  fill = sex),
              alpha = .2)+
  geom_line(data = model_sum,
            aes(x = jun_mean,
                y = emmean,
                colour = sex),
            show.legend = FALSE)+
  geom_point(shape = 21,
             colour = "black",
             show.legend = FALSE)+
  scale_colour_manual(values = c("darkorange", "purple"))+
  scale_fill_manual(values = c("darkorange", "purple"))+
  labs(x = "Mean June Temperature (°C)",
       y = "Forewing length (mm)",
       fill = "Sex")+
  theme_classic()+
  theme(legend.position = "top")



