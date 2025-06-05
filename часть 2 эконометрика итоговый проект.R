#Получим данные из файла csv
data <- read.csv("education_career_success.csv")
library(tidyverse)
table(data$Gender)
#Сразу удаляем строки с полом = "Other"
data <- data[data$Gender != "Other", ]
gender_counts <- data %>% count(Gender)

print(gender_counts)
# Преобразуем Gender в фактор с актуальными уровнями (только "male" и "female")
data$Gender <- factor(data$Gender)
#Посмотрим на данные
View(data)

#-------------------------------------------

#Цель: посмотреть еще раз на нормальность/ненормальность распределения Starting_Salary
shapiro.test(data$Starting_Salary)
#Пробуем разными способами привести к нормальному распределению Starting_Salary
shapiro.test(log(data$Starting_Salary))
shapiro.test(log1p(data$Starting_Salary))
qqnorm(data$Starting_Salary)
qqline(data$Starting_Salary, col = "red") # По визуальной оценке — неподходит для строгих параметрических методов, если чувствительность критична (например, t-тест с малой выборкой или ANOVA без устойчивости к выбросам).
#Построим более привлекательный график для презентации
qqnorm(data$Starting_Salary, 
       main = "Normal QQ-Plot для стартовой зарплаты",
       )
qqline(data$Starting_Salary, col = "red")

shapiro.test(sqrt(data$Starting_Salary))
#Не удалось привести к нормальному распределение по переменной Starting_Salary

#------------------------------------------------------------
#Kruskal-Wallis
#Выбрали его, так как стартовая зарплата не распределена нормально, а категорий по Field_of_Study > 2
kruskal.test(Starting_Salary ~ Field_of_Study, data = data)
# Визуализация
library(ggplot2)

ggplot(data, aes(x = Field_of_Study, y = Starting_Salary, fill = Field_of_Study)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Starting Salary by Field of Study",
    x = "Field of Study",
    y = "Starting Salary (USD)"
  ) +
  theme(legend.position = "none")
# Не имеем право на уровне значимости 0.05 отвергнуть  H0
#---------------------------------------------
#Манна–Уитни
#Две группы по статусу предпринимателя- категориальная переменная и Starting_Salary - непрерывная количественная, не распределена нормально. 

wilcox.test(Starting_Salary ~ Entrepreneurship, data = data)
# Визуализация
library(ggplot2)

ggplot(data, aes(x = Entrepreneurship, y = Starting_Salary, fill = Entrepreneurship)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Starting Salary by Entrepreneurship",
    x = "Entrepreneurship",
    y = "Starting Salary (USD)"
  ) +
  theme(legend.position = "none")

# Не имеем право на уровне значимости 0.05 отвергнуть  H0
#---------------------------------------------
#Манна–Уитни
#Несколько значений по текущему грейду работы- категориальная переменная и Starting_Salary - непрерывная количественная, не распределена нормально. 
kruskal.test(Starting_Salary ~ Current_Job_Level, data = data)

library(ggplot2)

ggplot(data, aes(x = Current_Job_Level, y = Starting_Salary, fill = Current_Job_Level)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Starting Salary by Current_Job_Level",
    x = "Current_Job_Level",
    y = "Starting Salary (USD)"
  ) +
  theme(legend.position = "none")
# Не имеем право на уровне значимости 0.05 отвергнуть  H0
#---------------------------------------------
#Немного раведывательного анализа, не включенного в файл питон:
# Построение barplot с использованием base R для переменной пола - Gender
barplot(table(data$Gender),
        main = "Barplot по полу",
        xlab = "Пол",
        ylab = "Количество наблюдений",
        col = c("lightpink", "skyblue"))

# Barplot с использованием base R для переменной сферы обучения - Field_of_Study
barplot(table(data$Field_of_Study),
        main = "Barplot по сфере обучения",
        xlab = "Сфера обучения",
        ylab = "Количество наблюдений",
        col = "lightgreen",
        las = 2,        # поворачивает подписи на оси X
        cex.names = 0.8 # уменьшает размер текста
)
# C использованием ggplot2 повернем названия по ОХ
library(ggplot2)

ggplot(data, aes(x = Field_of_Study, fill = Field_of_Study)) +
  geom_bar() +
  labs(title = "Barplot по сфере обучения",
       x = "Сфера обучения",
       y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Построение barplot с использованием base R по статусу предпринимателя - Entrepreneurship
barplot(table(data$Entrepreneurship),
        main = "Barplot по статусу предпринимателя",
        xlab = "Статус предпринимателя",
        ylab = "Количество наблюдений",
        col = c("lightpink", "skyblue"))
