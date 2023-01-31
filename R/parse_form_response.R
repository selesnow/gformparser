#' Парсер и форматирование результатов google формы
#'
#' @param responses_dox_id ID Докса с результатами формы
#' @param report_dox_id ID Докса в который надо выплюнуть результаты
#' @param response_sheet_mask Маска имён листов на которых хранятся результаты
#' @param service_key_path Путь к файлу сервисного аккаунта
#'
#' @return Возвращает информацию о новом доксе.
#' @export
parse_form_response <- function(
  responses_dox_id,
  report_dox_id = responses_dox_id,
  response_sheet_mask = 'Form Responses|Ответы на форму|Відповіді форми',
  service_key_path = system.file('service.json', package = 'gformparser')
) {

  cli::cli_alert_info('Auth in google spreadsheets')
  cli::cli_alert_warning("You need add share both dox to gformparser@webpromo-310616.iam.gserviceaccount.com")
  gs4_auth(path = service_key_path)

  cli::cli_alert_info('Load lists of heets')
  # считываем листы
  sheets <- sheet_names(responses_dox_id) %>%
            .[grepl(response_sheet_mask, .)]

  cli::cli_alert_info('Loaded {length(sheets)} sheets')

  # результирующий объект
  result <- list()

  # обрабатываем листы по очереди
  cli::cli_alert_info('Read data from all sheets')
  for ( sheet in sheets ) {

    sheet_data <- retry(
      range_read(responses_dox_id, sheet = sheet),
      until = ~ inherits(., "data.frame"),
      max_tries = 10,
      interval = sample(5:10, 1, replace = T)
      )

    if (nrow(sheet_data) == 0) {
      cli::cli_alert_warning('Sheet {sheet} is empty')
      next
    }

    prep_sheet_data <- sheet_data %>%
      pivot_longer(4:last_col(), names_to = "Responses", values_to = "Response value") %>%
      extract(col = 'Responses', into = 'User', regex = "\\[(.*)\\]", remove = FALSE) %>%
      extract(col = 'Responses', into = 'Responses', regex = "(.*)\\[", remove = TRUE) %>%
      mutate(User = str_squish(User),
        Responses = str_squish(Responses),
        date = as.Date(Timestamp)) %>%
      rename_with(to_snake_case) %>%
      mutate(
        response_score = case_when(
          response_value == "Точно нет" ~ 2,
          response_value == "Скорее нет, чем да" ~ 3,
          response_value == "Скорее да, чем нет" ~ 4,
          response_value == "Точно да" ~ 5
        )
      )

    result <- append(result, list(prep_sheet_data))

  }

  # объединяем результаты
  result <- bind_rows(result)

  # отправка в докс
  sheet_write(result, report_dox_id, sheet = 'results')

  cli::cli_alert_success('Well done')

  gs4_browse(report_dox_id)

  return(report_dox_id)

}
