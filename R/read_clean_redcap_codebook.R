


# Custom field name clean up function

clean_field_label <- function(df, field_label = field_label) {
  df %>%
    dplyr::mutate(
      field_label = stringr::str_replace({{ field_label }}, "<div class=\"rich-text-field-label\"><p>", ""),
      field_label = stringr::str_replace_all({{ field_label }}, "</p></div>", ""),
      field_label = stringr::str_replace_all({{ field_label }}, "</p>", ""),
      field_label = stringr::str_replace_all({{ field_label }}, "<br /><br />", ""),
      field_label = gsub("<.*", "", {{ field_label }}),
      field_label = stringr::str_replace_all({{ field_label }}, "-|\\(|\\)", ""),
      field_label = stringr::str_trim({{ field_label }})
    )
}


# Clean checkbox variables
clean_if_checkbox <- function(df){

  checkbox_choices <- df %>%
    dplyr::filter(field_type == "checkbox") %>%
    dplyr::select(variable, choices_calculations_or_slider_labels) %>%
    tidyr::separate(col = choices_calculations_or_slider_labels, into = letters, sep = "\\|") %>%
    tidyr::pivot_longer(a:z) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = stringr::str_replace(value, "-", "")) %>%
    dplyr::mutate(value = gsub(",.*", "", value)) %>%
    dplyr::mutate(new_var = paste(variable, value, sep = "_") %>%
             stringr::str_replace_all(" ", "")) %>%
    dplyr::select(variable, new_var)

  return(checkbox_choices)

}





# Tidy codebook

#' Clean Codebook
#'
#' @param dirty_codebook
#'
#' @return
#' @export
#'
#' @examples
#'
#'
clean_codebook <- function(dirty_codebook) {


  clean_codebook <- dirty_codebook %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      branching_logic = stringr::str_replace_all(branching_logic_show_field_only_if, fixed("\\"), "backslash"),
      branching_logic = stringr::str_replace_all(branching_logic, "\\[|\\]", ""),
      branching_logic = stringr::str_replace_all(branching_logic, "or", "|"),
      branching_logic = stringr::str_replace_all(branching_logic, " = ", "=="),
      branching_logic = stringr::str_replace_all(branching_logic, ">==", ">="),
      branching_logic = stringr::str_replace_all(branching_logic, "<==", "<="),
      branching_logic = stringr::str_replace_all(branching_logic, "and", "&"),
      branching_logic = stringr::stringr::str_replace_all(branching_logic, "<>", " != "),
      branching_logic = stringr::str_replace_all(branching_logic, "\\(", "___"),
      branching_logic = stringr::str_replace_all(branching_logic, "\\)", "")
    ) %>%
    clean_field_label() %>%
    rename(variable = 1)

  clean_codebook <- clean_codebook %>%
    dplyr::mutate(
      branching_logic = stringr::str_replace_all(branching_logic, "\n", ""),
      branching_logic = dplyr::if_else(stringr::str_detect(branching_logic, "& ___c"),
                                stringr::str_replace(branching_logic, "___", ""),
                                branching_logic
      )
    )


  checkbox_vars_clean <- clean_if_checkbox(clean_codebook)

  clean_codebook <- clean_codebook %>%
    dplyr::left_join(checkbox_vars_clean, by = "variable") %>%
    dplyr::mutate(variable = case_when(is.na(new_var) ~ variable,
                                TRUE ~ new_var)) %>%
    dplyr::select(-new_var)

  return(clean_codebook)
}





#' Clean and compile redcap hope home codebooks
#'
#' @param codebook_1
#' @param codebook_2
#'
#' @return
#' @export
#'
#' @examples
#'
#'
compiled_redcap_hopehome_codebook <- function(codebook_1 = "data_dictionaries/HopeHome3_DataDictionary_2023-06-29.csv",
                                              codebook_2 = "data_dictionaries/HH3LegacyBaselineSurvey_DataDictionary_2023-06-29.csv"){

  if(missing(codebook_1)){
    stop("Input codebook is missing.")
  }

  hh3_dict <- read.csv(codebook_1) %>%
    dplyr::mutate(cohort = "hope_home_3",
           base_fu = "baseline") %>%
    clean_codebook()


  hhl_dict <- read.csv(codebook_2) %>%
    dplyr::mutate(cohort = "hope_home_legacy",
           base_fu = "fu") %>%
    clean_codebook()

  full_dict <- hh3_dict %>%
    dplyr::bind_rows(hhl_dict)

  ### Clean question text to standardize the format

  full_dict <- full_dict %>%
    dplyr::mutate(field_label = stringr::str_replace_all(field_label, "\\(", ""),
           field_label = stringr::str_replace_all(field_label, "\\)", ""),
           field_label = stringr::str_replace_all(field_label, "\\.", ""),
           field_label = stringr::str_replace_all(field_label, "\\?", ""),
           field_label = stringr::str_replace_all(field_label, "\\'", ""),
           field_label = stringr::str_replace_all(field_label, "\\,", ""),
           field_label = stringr::str_to_lower(field_label),
           field_label = stringr::str_squish(field_label),
           field_label = stringr::str_trim(field_label),
           field_label = stringr::str_replace_all(field_label, " ", "_"),
           field_label = stringr::str_replace_all(field_label, ";", ""),
           field_label = stringr::str_replace_all(field_label, ":", ""),
           field_label = stringr::str_replace_all(field_label, "\n", ""),
           filed_label = stringr::str_to_lower(field_label),
           field_label = stringr::str_squish(field_label))


  # Update the question types

  full_dict <- full_dict %>%
    dplyr::mutate(field_type = dplyr::case_when(field_type %in% c("radio", "dropdow") ~ "multiple_choice",
                                  TRUE ~ field_type))


  ### Update the indicator lables used to match those in the HH 1/2 generated codebooks.

  full_dict <- full_dict %>%
    dplyr::rename(
      raw_var = variable,
      question = field_label,
      question_type = field_type
    ) %>%
    dplyr::mutate(
      file_name = "redcap_api"
    )


  full_dict <- full_dict %>%
    dplyr::mutate(survey = dplyr::case_when(
      form_name == "adl" ~ "adls",
      form_name == "mental_health_hh2bl_mentalhealth_133139_matrix_que" ~ "mental",
      form_name == "alcohol_hh2bl_alcohol_110114" ~ "alcohol"
    ))

  redcap_book <- full_dict %>%
    dplyr::filter(!is.na(survey)) %>%
    dplyr::select(raw_var,
           question,
           question_type,
           cohort,
           base_fu,
           survey,
           file_name)

  dropped <- redcap_book %>%
    dplyr::filter(question == "") %>%
    dplyr::select(dropped_from_redcap = raw_var)

  redcap_book <- redcap_book %>%
    dplyr::filter(question != "")

  print(dropped)


}
