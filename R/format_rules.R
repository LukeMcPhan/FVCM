#' Format Transition Rules
#'
#' Organizes and augments transition rules to produce spell condition metadata and state transition encoding.
#'
#' @param transition_rules Data frame of raw transition rules.
#' @param ordered_states Factor of ordered state names.
#'
#' @return A list of processed transition rule components including sequences and spell condition info.
#' @export
format_rules <- function(transition_rules, ordered_states) {
  unique_spell_conditions <- transition_rules %>%
    dplyr::select(spell_duration, spell_type) %>%
    dplyr::distinct() %>%
    dplyr::mutate(spell_duration_type = dplyr::row_number())

  unique_durations <- unique_spell_conditions %>%
    dplyr::select(spell_duration) %>%
    dplyr::distinct() %>%
    dplyr::arrange(spell_duration)

  unique_types <- unique_spell_conditions %>%
    dplyr::select(spell_type) %>%
    dplyr::distinct() %>%
    dplyr::arrange(spell_type)

  unique_spell_rules <- transition_rules %>%
    dplyr::left_join(unique_spell_conditions, by = c("spell_duration", "spell_type"))

  rule_sequences <- unique_spell_rules %>%
    dplyr::group_split(rule_number) %>%
    purrr::map(~ .x %>% dplyr::select(spell_duration_type))

  state_transitions <- unique_spell_rules %>%
    dplyr::group_split(rule_number) %>%
    purrr::map_dfr(~ .x %>%
                     dplyr::select(-c(rule_number, spell_duration_type)) %>%
                     dplyr::mutate(name = stringr::str_c(dplyr::first(initial_state_t0), dplyr::last(future_state_t1), sep = "__")) %>%
                     dplyr::select(name) %>%
                     dplyr::summarise(transiton_names = unique(name))) %>%
    unlist() %>% unname()

  named_rule_sequences <- rule_sequences %>%
    purrr::map(~ unlist(.x) %>% unname()) %>%
    rlang::set_names(nm = state_transitions)

  transition_rules_list <- list(
    spell_conditions = unique_spell_conditions,
    spell_durations = unique_durations,
    spell_type = unique_types,
    spell_rules = unique_spell_rules,
    states_ordered = ordered_states,
    named_rule_sequences = named_rule_sequences
  )

  return(transition_rules_list)
}
