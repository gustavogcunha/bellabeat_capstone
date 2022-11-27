# Instalar os pacotes:
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ggrepel")


# Carregando os pacotes:
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)


# Importando o Conjunto de Dados:
activity <- read.csv("Dataset/dailyActivity_merged.csv")
calories <- read.csv("Dataset/hourlyCalories_merged.csv")
intensities <- read.csv("Dataset/hourlyIntensities_merged.csv")
steps <- read.csv("Dataset/hourlySteps_merged.csv")
sleep <- read.csv("Dataset/sleepDay_merged.csv")
weight <- read.csv("Dataset/weightLogInfo_merged.csv")


# Para checar se os dados foram corretamente importados, vou utilizar as função head():
head(activity)
head(calories)
head(intensities)
head(steps)
head(sleep)
head(weight)


# Continuando a checagem dos dados, vou utilizar a função glimpse(), que  mostra o número de linhas, número de colunas, nomes de coluna e tipos de dados das colunas:
glimpse(activity)
glimpse(calories)
glimpse(intensities)
glimpse(steps)
glimpse(sleep)
glimpse(weight)


# Limpeza dos dados:


# Eu detectei problemas na formatação dos dados de data/hora. Desta forma, antes da análise, preciso convertê-los para o formato de data e hora e dividir em duas colunas separadas:
# activity:
activity$ActivityDate = as.POSIXct(activity$ActivityDate, format = "%m/%d/%Y", tz = Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%d/%m/%Y")
# calories:
calories$ActivityHour = as.POSIXct(calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
calories$date <- format(calories$ActivityHour, format = "%d/%m/%Y")
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
# intensities:
intensities$ActivityHour = as.POSIXct(intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
intensities$date <- format(intensities$ActivityHour, format = "%d/%m/%Y")
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
# sleep:
sleep$SleepDay = as.POSIXct(sleep$SleepDay, format = "%m/%d/%Y", tz = Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%d/%m/%Y")
# steps:
steps$ActivityHour = as.POSIXct(steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
steps$date <- format(steps$ActivityHour, format = "%d/%m/%Y")
steps$time <- format(steps$ActivityHour, format = "%H:%M:%S")
# weight:
weight$Date = as.POSIXct(weight$Date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
weight$date <- format(weight$Date, format = "%d/%m/%Y")
weight$time <- format(weight$Date, format = "%H:%M:%S")


# Número de participantes em cada conjunto de dados:
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(steps$Id)
n_distinct(weight$Id)


# Quantos dias foram utilizados para a coleta das informações dos participantes:
n_distinct(activity$date)
n_distinct(calories$date)
n_distinct(intensities$date)
n_distinct(sleep$date)
n_distinct(steps$date)


# Número de dias coletados por participante:
activity %>%
  group_by(Id) %>%
  summarise(Days = length(unique(date)))


# Quantidade de dias coletados agrupado por usuários:
user_day <- activity %>%
  group_by(Id) %>%
  summarise(Days = length(unique(date)))

user_day %>%
  group_by(Days) %>%
  summarise(User = length(unique(Id)))


# Verificando se há valores nulos nos conjuntos de dados:
# activity
activity %>%
  is.na() %>%
  sum()
# calories
calories %>%
  is.na() %>%
  sum()
# intensity
intensities %>%
  is.na() %>%
  sum()
# sleep
sleep %>%
  is.na() %>%
  sum()
# steps
steps %>%
  is.na() %>%
  sum()


# Verificando se há valores duplicados nos conjuntos de dados:
# activity
sum(duplicated(activity))
# calories
sum(duplicated(calories))
# intensity
sum(duplicated(intensities))
# sleep
sum(duplicated(sleep))
# steps
sum(duplicated(steps))


# Remover as linhas duplicadas:
sleep_novo <- sleep[!duplicated(sleep), ]


# Verificando se ainda há linhas duplicadas:
sum(duplicated(sleep_novo))


# Análise dos dados:


# Função summary(), que fornece um sumário estatístico descritivo dos dados quantitativos:
# Vamos explorar o número de passos por dia, a distânica por dia, o tempo sedentário em minutos e as calorias por dia
activity %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         Calories) %>%
  summary()

# Vamos explorar o número de minutos ativos por categoria
activity %>%
  select(VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes) %>%
  summary()

# Vamos explorar as calorias gastas por hora
calories %>%
  select(Calories) %>%
  summary()

# Vamos explorar as informações sobre o sono
sleep_novo %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


# Mesclando conjunto de dados:
activity_sleep <- merge(activity, sleep_novo, by = c('Id', 'date'))
head(activity_sleep)


# Verificando o número de participantes do conjunto de dados mesclado:
n_distinct(activity_sleep$Id)


# Como tínhamos apenas 24 IDs exclusivos para o conjunto de dados "sleep_novo", temos apenas aqueles IDs no conjunto combinado. Para obtermos todos os IDs exclusivos do conjunto de dados activity, usarei a mescla outer join:
new_activity_sleep <- merge(activity, sleep_novo, by = c('Id', 'date'), all = TRUE)
head(activity_sleep)


# Verificando o número de participantes do conjunto de dados mesclado:
n_distinct(new_activity_sleep$Id)


# Verificando a correlação entre o total de passos em relação as calorias:
ggplot(data = new_activity_sleep, aes(x = TotalSteps, y = Calories)) +
  geom_point() +
  stat_smooth(method = lm) +
  labs(title = "Total de Passos X Calorias")


# Verificando o percentual da intensidade de atividades:
# Calculando o total de minutos por intensidade das atividades
new_activity_sleep$VeryActiveMinutes %>%
  sum()
new_activity_sleep$FairlyActiveMinutes %>%
  sum()
new_activity_sleep$LightlyActiveMinutes %>%
  sum()
new_activity_sleep$SedentaryMinutes %>%
  sum()
# Criando um conjunto de dados sobre a atividade
df_active = data.frame("slices" = c(19895, 12751, 181244, 931738), "lbls" = c("Very Active", "Fairly Active", "Lightly Active", "Sedentary"))
# Calculando a porcentagem pela atividade
pct <- round(df_active$slices/sum(df_active$slices)*100, 2)
head(pct)
# Adicionando o percentual no conjunto de dados
df_active = data.frame("lbls" = c("Muito Ativo", "Moderadamente Ativo", "Levemente Ativo", "Sedentário"), "slices" = c(20029, 12852, 182023, 933903), "pct" = c(1.74, 1.11, 15.82, 81.33))
glimpse(df_active)
# Configurando as posições dos labels
df2 <- df_active %>% 
  mutate(csum = rev(cumsum(rev(pct))), 
         pos = pct/2 + lead(csum, 1),
         pos = if_else(is.na(pos), pct/2, pos))
# Gerando o gráfico de pizza
ggplot(df_active, aes(x = "" , y = pct, fill = fct_inorder(lbls))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos,label = paste0(pct, "%")),
                   size = 4.5,nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "")) +
  theme_void()


# Verificando a relação entre total de passos e a intensidade de atividade em minutos:
# Criando uma tabela com as atividades por minuto
Active_min <- gather(new_activity_sleep, key = "ActiveMinutes", value = "Minutes", 12:15)
# Alterando o nome das categorias
Active_min$ActiveMinutes[Active_min$ActiveMinutes == "FairlyActiveMinutes"] <- "Moderadamente Ativo"
Active_min$ActiveMinutes[Active_min$ActiveMinutes == "VeryActiveMinutes"] <- "Muito Ativo"
Active_min$ActiveMinutes[Active_min$ActiveMinutes == "LightlyActiveMinutes"] <- "Levemente Ativo"
Active_min$ActiveMinutes[Active_min$ActiveMinutes == "SedentaryMinutes"] <- "Sedentário"
# Média TotalSteps e Minutes
Active_min %>%
  select(TotalSteps, Minutes) %>%
  summary()
# Gerando o gráfico de disperção
ggplot(data = Active_min, aes(x = TotalSteps, y = Minutes, color = ActiveMinutes)) +
  geom_point(size=2) +
  geom_hline(aes(yintercept = mean(Minutes)), color = "Blue", size = 1) +
  geom_vline(aes(xintercept = mean(TotalSteps)), color = "red", size = 1) +
  scale_y_continuous(breaks = sort(c(seq(0,1500,250))), 
                     labels = paste0(sort(c(seq(0,1500,250)))),
                     sec.axis = sec_axis(trans=~., breaks = 304.7, labels=paste0(304.7))) +
  scale_x_continuous(breaks = sort(c(seq(0,50000,5000))), 
                     labels = paste0(sort(c(seq(0,50000,5000)))),
                     sec.axis = sec_axis(trans=~., breaks = 7638, labels=paste0(7638))) +
  labs(title="Total de Passos X Intensidade de Atividade (min)") +
  theme(legend.position="top") +
  theme(legend.title = element_blank()) +
  theme(axis.ticks.y.right = element_line(color = "Blue"),
        axis.text.y.right = element_text(color = "Blue")) +
  theme(axis.ticks.x.top = element_line(color = "red"),
        axis.text.x.top = element_text(color = "red"))
# Ampliar a visualização do gráfico anterior (Gerando o novo gráfico de disperção em escala logarítmica)
ggplot(data = Active_min, aes(x = TotalSteps, y = Minutes, color = ActiveMinutes)) +
  geom_point(size=2) +
  scale_y_log10(sec.axis = sec_axis(trans=~., breaks = 304.7, labels=paste0(304.7))) +
  scale_x_continuous(breaks = sort(c(seq(0,50000,5000))), 
                     labels = paste0(sort(c(seq(0,50000,5000)))),
                     sec.axis = sec_axis(trans=~., breaks = 7638, labels=paste0(7638))) +
  geom_hline(aes(yintercept = mean(Minutes)), color = "Blue", size = 1) +
  geom_vline(aes(xintercept = mean(TotalSteps)), color = "red", size = 1.1) +
  labs(title="Total de Passos X Intensidade de Atividade (min)") +
  theme(legend.position="top") +
  theme(legend.title = element_blank()) +
  theme(axis.ticks.y.right = element_line(color = "Blue"),
        axis.text.y.right = element_text(color = "Blue")) +
  theme(axis.ticks.x.top = element_line(color = "red"),
        axis.text.x.top = element_text(color = "red"))


# Verificando a relação entre o tempo total na cama e o tempo dormindo:
# Média TotalMinutesAsleep e TotalTimeInBed
new_activity_sleep %>%
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# Gerando o gráfico
ggplot(data = new_activity_sleep, aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
  geom_point(color = "black") +
  stat_smooth(method = lm) +
  geom_hline(aes(yintercept = 458.5), color = "orange", size = 1) +
  geom_vline(aes(xintercept = 419.2), color = "red", size = 1) +
  scale_y_continuous(breaks = sort(c(seq(0,1000,100))), 
                     labels = paste0(sort(c(seq(0,1000,100)))),
                     sec.axis = sec_axis(trans=~., breaks = 458.5, labels=paste0(458.5))) +
  scale_x_continuous(breaks = sort(c(seq(0,900,100))), 
                     labels = paste0(sort(c(seq(0,900,100)))),
                     sec.axis = sec_axis(trans=~., breaks = 419.2, labels=paste0(419.2))) +
  labs(title="Tempo na Cama X Tempo Dormindo (min)") +
  theme(axis.ticks.y.right = element_line(color = "orange"),
        axis.text.y.right = element_text(color = "orange")) +
  theme(axis.ticks.x.top = element_line(color = "red"),
        axis.text.x.top = element_text(color = "red"))


# Verificando a relação entre o intensidade da atividade ao longo do dia (em horas):
intensity <- intensities %>%
  group_by(time) %>%
  summarise(mean_intensity = mean(TotalIntensity))
# Gerando o gráfico
ggplot(data = intensity, aes(x = time, y = mean_intensity)) +
  geom_histogram(stat = "identity", fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Média de Intensidade X Tempo (horas)")


# Verificando a relação entre o tempo dormindo e o tempo sedentário:
ggplot(data = new_activity_sleep, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point(color = "black") +
  stat_smooth(method = lm) +
  labs(title="Tempo Dormindo X Tempo Sedentário (min)")