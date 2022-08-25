
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gformparser

Скрипт сбора и форматирования результатов полученных из Google форм.

## Подготовка

Изначально необходимо установить язык R. Перейдите по
[ссылке](https://cran.r-project.org/), и скачайте установщик для своей
операционной системе.

![](http://img.netpeak.ua/alsey/1CJLAFG.png)

До запуска скрипта, необходимо расшарить докс (с правами редактирования)
с ответами на форму, и докс в который скрипт загрузит результаты на
посту сервисного аккаунта
`gformparser@webpromo-310616.iam.gserviceaccount.com`.

![](http://img.netpeak.ua/alsey/1CJM3NY.png)

## Установка нужных пакетов

После установки R запустите сам язык:
![](http://img.netpeak.ua/alsey/1CJLO9Z.png)

В открывшейся консоли запустите следующие команды для установки пакетов.

``` r
install.packages("devtools")
devtools::install_github("selesnow/gformparser")
```

Пакеты необходимо установить только один раз.

## Запуск процесса сбора и форматирование результатов Google форм

Для запуска процесса в R консоли используйте команду
`gformparser::parse_form_response()`, по следующему примеру:

``` r
parse_form_response(
  responses_dox_id = "https://docs.google.com/spreadsheets/d/1GPdwnre6O_BuS3sRJIDBPuhbHtLC28TapLmBxf816TI/edit#gid=1971596860",
  report_dox_id = "https://docs.google.com/spreadsheets/d/1cU5n58qst8g3R-Zm9tze5tqjqlsiCKtViEzDe8rPUgk/edit?resourcekey#gid=44788386"
)
```

В параметр `responses_dox_id` необходимо передать ссылку, или id
исходного докса с ответами на Google форму. В параметр `report_dox_id`
необходимо передать ссылку, или id результирующего докса, в который
будет загружен результат сбора и обработки данных. Если этот параметр не
указан, то функция загрузит результат в исходный докс на лист result.