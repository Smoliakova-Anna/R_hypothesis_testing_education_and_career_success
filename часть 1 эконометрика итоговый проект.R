#Получим данные из файла csv
data <- read.csv("education_career_success.csv")
library(tidyverse)
#Сразу удаляем строки с полом = "Other", так как пол "Other" меньше других по кол-ву, а также может содержать тех, кто просто не указал пол (своеобразные нуль-значения)
# Построение barplot с использованием base R для полов для визуализации причины, почему удаляем 'Other'
barplot(table(data$Gender),
        main = "Barplot по полу",
        xlab = "Статус предпринимателя",
        ylab = "Количество наблюдений",
        col = c("lightpink", "skyblue", 'green'))

data <- data[data$Gender != "Other", ]
gender_counts <- data %>% count(Gender)

print(gender_counts)
# Преобразуем Gender в фактор с актуальными уровнями (только "male" и "female")
data$Gender <- factor(data$Gender)
#Посмотрим на данные
View(data)

#-------------------------------------------
#Цель: Проверить, есть ли связь между Job_Offers и Starting_Salary.
shapiro.test(data$Starting_Salary) #проверка нормальности для Starting_Salary. Starting_Salary не имеет нормальное распределение.
cor.test(data$Job_Offers, data$Starting_Salary, method = "kendall") # используем kendall метод
#Job_Offers — количественная дискретная переменная с небольшим числом уникальных значений (0,1,2,3,4,5).

#Starting_Salary — количественная непрерывная, но НЕ нормально распределена (Shapiro тест это подтвердил).
#Почему Кендалл? Кендалл (корреляция Кендалла) — хороший выбор для ранговых и/или дискретных количественных переменных с повторяющимися значениями
#Он устойчив к повторяющимся значениям и нелинейным монотонным связям.
#Пирсон в данном случае не подходит из-за нарушений нормальности и дискретности одной из переменных.
#Спирмен тоже вариант, но Кендалл часто предпочтительнее при наличии множества "узлов" повторений.
#Визуализируем работу:
library(ggplot2)

ggplot(data, aes(x = factor(Job_Offers), y = Starting_Salary)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Starting Salary by Number of Job Offers",
       x = "Number of Job Offers",
       y = "Starting Salary (USD)")

#Стат значима зависимотсь (на уровне значимости 0.05, но не на 0.01)
#------------------------------------------------------------
#проверим. Медианы стартовой з/п между двумя группами полов не равны?

data$Gender <- factor(data$Gender)

# Манна–Уитни тест - Две группы по полу - категориальная переменная и Starting_Salary - непрерывная количественная, не распределена нормально.
wilcox.test(Starting_Salary ~ Gender, data = data)
# Визуализируем
library(ggplot2)

ggplot(data, aes(x = Gender, y = Starting_Salary, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Starting Salary by Gender",
    x = "Gender",
    y = "Starting Salary (USD)"
  ) +
  theme(legend.position = "none")

# Не имеем право на уровне значимости 0.05 отвергнуть  H0

