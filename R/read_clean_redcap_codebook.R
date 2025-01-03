
# Custom field name clean up function

clean_field_label <- function(df, field_label = field_label) {
  df |>
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

  checkbox_choices <- df |>
    dplyr::filter(field_type == "checkbox") |>
    dplyr::select(variable, choices_calculations_or_slider_labels) |>
    tidyr::separate(col = choices_calculations_or_slider_labels, into = letters, sep = "\\|") |>
    tidyr::pivot_longer(a:z) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(value = stringr::str_replace(value, "-", "")) |>
    dplyr::mutate(value = gsub(",.*", "", value)) |>
    dplyr::mutate(new_var = paste(variable, value, sep = "_") |>
             stringr::str_replace_all(" ", "")) |>
    dplyr::select(variable, new_var)

  return(checkbox_choices)

}

# Clean checkbox choices

clean_if_checkbox_choice <- function(df){

checkbox_choices <- df |>
    dplyr::filter(field_type == "checkbox") |>
    dplyr::select(variable, choices_calculations_or_slider_labels) |>
    tidyr::separate(col = choices_calculations_or_slider_labels, into = letters, sep = "\\|") |>
    tidyr::pivot_longer(a:z) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(value = stringr::str_replace(value, "-", "")) |>
    tidyr::separate(value, c("var", "choice"), sep = ",") |>
    dplyr::mutate(var = stringr::str_trim(var),
           choice = stringr::str_trim(choice)) |>
    dplyr::mutate(new_var = paste(variable, var, sep = "_")) |>
    dplyr::select(variable, new_var, choice)

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


  clean_codebook <- dirty_codebook |>
    janitor::clean_names() |>
    dplyr::mutate(
      branching_logic = stringr::str_replace_all(branching_logic_show_field_only_if, stringr::fixed("\\"), "backslash"),
      branching_logic = stringr::str_replace_all(branching_logic, "\\[|\\]", ""),
      branching_logic = stringr::str_replace_all(branching_logic, " or ", "|"),
      branching_logic = stringr::str_replace_all(branching_logic, " = ", "=="),
      branching_logic = stringr::str_replace_all(branching_logic, ">==", ">="),
      branching_logic = stringr::str_replace_all(branching_logic, "<==", "<="),

      branching_logic = stringr::str_replace_all(branching_logic, "and", "&"),
      branching_logic = stringr::str_replace_all(branching_logic, "<>", " != "),
      branching_logic = stringr::str_replace_all(branching_logic, "\\(", "___"),
      branching_logic = stringr::str_replace_all(branching_logic, "\\)", "")

    ) |>
    clean_field_label() |>
    dplyr::rename(variable = 1)

  clean_codebook <- clean_codebook |>
    dplyr::mutate(
      branching_logic = stringr::str_replace_all(branching_logic, "\n", ""),
      branching_logic = dplyr::if_else(stringr::str_detect(branching_logic, "& ___c"),
                                stringr::str_replace(branching_logic, "___", ""),
                                branching_logic
      )
    )

  clean_codebook <- clean_codebook |>
    mutate(choices_calculations_or_slider_labels = stringr::str_to_lower(choices_calculations_or_slider_labels),
           choices_calculations_or_slider_labels = stringr::str_replace_all(choices_calculations_or_slider_labels, "'", ""))

  # Create yesno variable more broadly if the question has Yes No in combination with Don't know and refused

yesno_values <-   c("1, yes | 2, no",
     "1, yes | 2, no | 3, i dont know",
     "1, yes | 2, no | -1, dont know/refused",
     "1, yes | 2, no | -98, dont know/refused",
     "1, yes | 2, no | -98, dont know/refused",
     "1, yes | 2, no | -98, dont know",
     "1, yes | 2, no | -98, dont know | -99, refused",
     "1, yes | 2, no | 3, dont know",
     "1, yes | 0, no | -98, i dont know",
     "1, yes | 2, no | -98, i dont know" ,
     "1, yes | 2, no | -98, dont know/ refused",
     "1, yes | 0, no",
     "1, yes | 2, no | 3, cant do | 4, dont do" )

clean_codebook <- clean_codebook |>
  dplyr::mutate(field_type = case_when(choices_calculations_or_slider_labels %in% yesno_values ~ "yesno",
                                TRUE ~ field_type))

  checkbox_vars_clean <- clean_if_checkbox_choice(clean_codebook)

  clean_codebook <- clean_codebook |>
    dplyr::left_join(checkbox_vars_clean, by = "variable") |>
    dplyr::mutate(variable = dplyr::case_when(is.na(new_var) ~ variable,
                                TRUE ~ new_var),
                  choices_calculations_or_slider_labels = dplyr::case_when(is.na(choice) ~ choices_calculations_or_slider_labels,
                                                                           TRUE ~ choice)) |>
    dplyr::select(-new_var, - choice)

  return(clean_codebook)
}


#' Clean and compile redcap hope home codebooks
#'
#' @param codebook_1
#' @param codebook_2
#'
#' @return This function returns compiled codebooks from REDCap Hope Home 3 and Hope Home Legacy, that match up with the Hope Home I and II codebooks.
#' @export
#'
#' @examples
#'
#'
compiled_redcap_hopehome_codebook <- function(codebook_1,
                                              codebook_2 = NULL,
                                              codebook_3 = NULL){

if(missing(codebook_1)){
  stop("There is no REDCap codebook provided.")
}

  hh3_dict <- read.csv(codebook_1) |>
    dplyr::mutate(cohort = "hope_home_3",
           base_fu = "baseline") |>
    clean_codebook()

if(!missing(codebook_2)){
  hhl_dict <- read.csv(codebook_2) |>
    dplyr::mutate(cohort = "hope_home_legacy",
           base_fu = "fu") |>
    clean_codebook()
}


if(!missing(codebook_3) & !missing(codebook_2)){
  hh_fu <- read.csv(codebook_3) |>
    dplyr::mutate(cohort = "hh3_follow_up",
           base_fu = "fu") |>
    clean_codebook()

}

if(missing(codebook_2) & missing(codebook_3)){
  full_dict <- hh3_dict
}

if(missing(codebook_3)){
    full_dict <- hh3_dict |>
      bind_rows(hhl_dict)
}else{
    full_dict <- hh3_dict |>
      bind_rows(hhl_dict) |>
      bind_rows(hh_fu)
  }


  ### Clean question text to standardize the format

  full_dict <- full_dict |>
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

  full_dict <- full_dict |>
    dplyr::mutate(field_type = dplyr::case_when(field_type %in% c("radio", "dropdow") ~ "multiple_choice",
                                  TRUE ~ field_type))


  ### Update the indicator lables used to match those in the HH 1/2 generated codebooks.

  full_dict <- full_dict |>
    dplyr::rename(
      raw_var = variable,
      question = field_label,
      question_type = field_type
    ) |>
    dplyr::mutate(
      file_name = "redcap_api"
    )


  full_dict <- full_dict |>
    dplyr::mutate(survey = dplyr::case_when(
      form_name == "adl" ~ "adls",
      form_name == "mental_health_hh2bl_mentalhealth_133139_matrix_que" ~ "mental",
      form_name == "alcohol_hh2bl_alcohol_110114" ~ "alcohol",
      stringr::str_detect(form_name, "ADL|Mobility") ~ "adls",
                         stringr::str_detect(form_name, "ces") ~ "CESD",
                         stringr::str_detect(form_name, "health care|healthcare|healthcare") & stringr::str_detect(form_name, "Utilization|utilization") ~ "Healthcare utilization",
                         stringr::str_detect(form_name, "Health Care|Healthcare") ~ "Healthcare",
                         stringr::str_detect(form_name, "Drugs|drugs") ~ "Drugs",
                         stringr::str_detect(form_name, "bifs") ~ "BIF",
                         stringr::str_detect(form_name, "incarceration") ~ "Incarceration",
                         stringr::str_detect(form_name, "health_hx") ~ "Health history",
      stringr::str_detect(form_name, "social") ~ "Social support",
      stringr::str_detect(form_name, "income") ~ "Income",
      stringr::str_detect(form_name, "lone") & stringr::str_starts(raw_var, "lr") ~ "Loneliness",
      stringr::str_detect(form_name, "lone") & stringr::str_starts(raw_var, "pr") ~ "PROMIS",
      stringr::str_detect(form_name, "sex") ~ "Sexual relationships",
      stringr::str_detect(form_name, "veteran") ~ "Veteran",
      stringr::str_detect(form_name, "cigarette") ~ "Cigarette smoking",
      stringr::str_detect(form_name, "intro") ~ "Intro",
      stringr::str_detect(form_name, "employ") ~ "Employment",
      stringr::str_detect(form_name, "educ") ~ "Education",
      stringr::str_detect(form_name, "abuse") ~ "neglect_abuse",
      stringr::str_detect(form_name, "family") ~ "family",
      stringr::str_detect(form_name, "insurance") ~ "health_insurance",
      stringr::str_detect(form_name, "trajectory") ~ "housing_trajectory",
      stringr::str_detect(form_name, "housing") ~ "housing",
      stringr::str_detect(form_name, "medication") ~ "medication",
      stringr::str_detect(form_name, "weight") ~ "heightweightvision",
      stringr::str_detect(form_name, "child") ~ "children",
      stringr::str_detect(form_name, "data_preload|screener_info") ~ "demographics",
      stringr::str_detect(form_name, "trails_scor") ~ "trails",
      stringr::str_detect(form_name, "ms_scor") ~ "ms_scoring",
      stringr::str_detect(form_name, "sppb") ~ "sppb",
      stringr::str_detect(form_name, "hearth") ~ "hearth"
      ))

  redcap_book <- full_dict |>
    dplyr::filter(!is.na(survey)) |>
    dplyr::select(raw_var,
           question,
           question_type,
           cohort,
           base_fu,
           survey,
           file_name,
           branching_logic,
           choices_calculations_labels_raw = choices_calculations_or_slider_labels,
           text_type = text_validation_type_or_show_slider_number)

  redcap_book <- redcap_book |>
    mutate(
      branching_logic = stringr::str_replace(branching_logic, " ='", "=='"),
      branching_logic = stringr::str_replace(branching_logic, "1= '|1='", "1=='"),
      branching_logic = stringr::str_replace(branching_logic, "d= '|d='", "d=='"),
      branching_logic = stringr::str_replace(branching_logic, "4= '|4='", "4=='"),
      branching_logic = stringr::str_replace(branching_logic, "7= '|7='", "7=='"),
      branching_logic = stringr::str_replace(branching_logic, "b= '|b='", "b=='"),
      branching_logic = stringr::str_replace(branching_logic, "8= '|8='", "8=='"),
      branching_logic = stringr::str_replace(branching_logic, "fourteendays='2'", "fourteendays=='2'"),
      branching_logic = stringr::str_replace(branching_logic, "fourteendays_hospital= '1'", "fourteendays_hospital== '1'"),
      branching_logic = str_replace_all(branching_logic, "(?<=^|[^!><=])=(?=[^=])", "=="),

    )


  # dropped <- redcap_book |>
  #   dplyr::filter(question == "") |>
  #   dplyr::select(dropped_from_redcap = raw_var)
  #
  # redcap_book <- redcap_book |>
  #   dplyr::filter(question != "")
  #
  # print(dropped)

  return(redcap_book)


}
