# IMPORTANDO AS TABELAS

tabela1 <- read_delim('01-2023_2.csv', delim = ";")
View(tabela1)

tabela2 <- read_delim('02-2023_2.csv', delim = ";")
View(tabela2)

tabela3 <- read_delim('03-2023_2.csv', delim = ";")
View(tabela3)

tabela4 <- read_delim('04-2023_2.csv', delim = ";")
View(tabela4)

# RENOMEANDO A COLUNA DA TABELA 2 POIS VEIO ERRADA

tabela2 <- rename(tabela2, dayweek_ended = datweek_ended)

# EXCLUINDO A COLUNA 'RIDE_TWO_DAYS' DAS TABELAS POIS ESTAVA DANDO PROBLEMA E É IRRELEVANTE

tabela1$ride_two_days <- NULL
tabela2$ride_two_days <- NULL
tabela3$ride_two_days <- NULL
tabela4$ride_two_days <- NULL

# UNINDO TODOS OS DADOS

dados <- union(tabela1, tabela2)
dados <- union(dados, tabela3)
dados <- union(dados, tabela4)

# INICIO DOS INSIGHTS

insight1 <- dados %>% group_by(member_casual) %>%  
  summarise(ride_time_médio_minutos = minute(seconds_to_period(mean(ride_time))))

insight2 <- dados %>% group_by(dayweek_started) %>%  
  summarise(ride_time_médio_minutos = minute(seconds_to_period(mean(ride_time))))

insight3 <- dados %>% group_by(member_casual, dayweek_started) %>%  
  summarise(ride_time_médio_minutos = minute(seconds_to_period(mean(ride_time))))

insight4 <- dados %>%
  group_by(member_casual) %>% 
  summarise(total_corridas = n())

insight5 <- dados %>%
  group_by(member_casual, rideable_type) %>%
  summarise(total_corridas = n())

insight6 <- dados %>% group_by(member_casual, rideable_type) %>%  
  summarise(ride_time_médio_minutos = minute(seconds_to_period(mean(ride_time))))

# CRIANDO OS GRÁFICOS


  ggplot(data = insight1, aes(x = member_casual, y = ride_time_médio_minutos)) +
    geom_bar(stat = 'identity') +
    labs(
      title = "Média Duração Passeio por Tipo Cliente",
      x = "Tipo Cliente",
      y = "Média Duração Passeio (Minutos)"
    )
  
  
  
  ggplot(data = insight2, aes(x = dayweek_started, y = ride_time_médio_minutos)) +
    geom_bar(stat = 'identity') +
    labs(
      title = "Média Duração Passeio por Dia da Semana",
      x = "Dia da Semana",
      y = "Média Duração Passeio (Minutos)"
    )
  
  
  
  ggplot(data = insight3, aes(x = dayweek_started, y = ride_time_médio_minutos, fill = member_casual)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = ride_time_médio_minutos, vjust = 2)) +
    scale_fill_manual(values = c("gray", "dark grey")) +
    labs(
      title = "Média Duração Passeio por Dia da Semana e Tipo Cliente ",
      x = "Dia da Semana",
      y = "Média Duração Passeio (Minutos)",
      fill = "Tipo Cliente"
    )
  
  
  
  ggplot(data = insight4, aes(x = member_casual, y = total_corridas)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = total_corridas, vjust = 3)) +
    labs(
      title = "Total de Passeios por Tipo Cliente ",
      x = "Tipo Cliente",
      y = "Total Passeios"
    )
  
  
  
  ggplot(data = insight5, aes(x = rideable_type, y = total_corridas, fill = member_casual)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = total_corridas, vjust = -6)) +
    scale_fill_manual(values = c("gray", "dark gray")) +
    labs(
      title = "Total de Passeios por Tipo de Bike e Tipo Cliente ",
      x = "Tipo Bike",
      y = "Total Passeios",
      fill = "Tipo Cliente"
    )
  
  

  
  ggplot(data = insight6, aes(x = rideable_type, y = ride_time_médio_minutos, fill = member_casual)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = ride_time_médio_minutos, vjust = 2)) +
    scale_fill_manual(values = c("gray", "dark gray")) +
    labs(
      title = "Média Duração Passeio por Tipo de Bike e Tipo Cliente ",
      x = "Tipo Bike",
      y = "Média Duração Passeio (Minutos) ",
      fill = "Tipo Cliente"
    )