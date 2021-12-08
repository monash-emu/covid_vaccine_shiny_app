
source("master_utils.R")


c1 <- get_params("Australia")

p1 <- calc_targets(c1, target_coverages = c(0, 0.3, 0.5))
plot_targets(p1)
# ok its the coverage information

# we need the barplot
# and the age breakdown

p2 <- calc_targets_ages(c1, target_coverages = c(0, 0.3, 0.5))

p2 %>%
  filter(interval == "mean") %>%
  ggplot(aes(x = strategy, y = infections, fill = factor(age_ind))) +
  geom_col() +
  coord_flip() +
  facet_grid(coverage + vaccine ~ vacc_status)
# the one challenge is dealing with the vacc vs unvacc

p2 %>%
  pivot_longer(
    cols = 3:5,
    names_to = "event",
    values_to = "value"
  ) %>%
  group_by(age_ind, interval, coverage, strategy, event, vaccine) %>%
  summarise(
    all_val = sum(value),
    all_pop = sum(pop_size)
  ) %>%
  ungroup() %>%
  group_by(interval, coverage, strategy, event, vaccine) %>%
  mutate(rate = all_val * 1e5 / sum(all_pop)) %>%
  filter(interval == "mean") %>%
  filter(!event == "hospitalisations") %>%
  left_join(ages) %>%
  ggplot(aes(x = strategy, y = rate, fill = forcats::fct_reorder(age_group_p, age_group_ind2, min))) +
  geom_col(col = "black", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  facet_grid(vaccine + coverage ~ event, scales = "free_x") +
  theme(legend.position = "bottom")

p2 %>%
  pivot_longer(
    cols = 3:5,
    names_to = "event",
    values_to = "value"
  ) %>%
  group_by(age_ind, interval, coverage, strategy, event, vaccine) %>%
  summarise(
    all_val = sum(value),
    all_pop = sum(pop_size)
  ) %>%
  ungroup() %>%
  group_by(interval, coverage, strategy, event, vaccine) %>%
  mutate(rate = all_val * 1e5 / sum(all_pop)) %>%
  filter(interval == "mean") %>%
  filter(!event == "hospitalizations") %>%
  left_join(ages) %>%
  ggplot(aes(x = forcats::fct_reorder(age, age_ind, min), y = rate, col = strategy, group = strategy)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 18) +
  labs(
    y = "rate per 100,000 population",
    x = "Age group"
  ) +
  facet_grid(vaccine + event ~ coverage, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = -90),
    legend.position = "top"
  )



p2 %>%
  pivot_longer(
    cols = 3:5,
    names_to = "event",
    values_to = "value"
  ) %>%
  group_by(age_ind, interval, coverage, strategy, event, vaccine) %>%
  summarise(
    all_val = sum(value),
    all_pop = sum(pop_size)
  ) %>%
  ungroup() %>%
  group_by(interval, coverage, strategy, event, vaccine) %>%
  mutate(rate = all_val * 1e5 / sum(all_pop)) %>%
  filter(interval == "mean") %>%
  filter(!event == "hospitalisations") %>%
  filter(
    strategy == "Transmitters",
    coverage == 0,
    vaccine == "Pfizer",
    event == "infections"
  ) %>%
  left_join(ages) %>%
  View()
# #ok so they are counts
#
# p2 %>% pivot_longer(cols = 3:5,
#                     names_to = 'event',
#                     values_to = 'value') %>%
#   group_by(age,interval,coverage,strategy,event) %>%
#   summarise(all_val = sum(value),
#             all_pop = sum(pop_size)) %>%
#   ggplot(aes(x=all_pop,y=all_val))+
#   geom_point()+geom_abline(intercept = 0,slope=1)
# #ok


p3 <- calc_targets(c1, target_coverages = seq(0, 1, 0.05))


p3 %>%
  pivot_longer(
    cols = 3:5,
    names_to = "event",
    values_to = "value"
  ) %>%
  group_by(age_ind, interval, coverage, strategy, event, vaccine) %>%
  summarise(
    all_val = sum(value),
    all_pop = sum(pop_size)
  ) %>%
  ungroup() %>%
  group_by(interval, coverage, strategy, event, vaccine) %>%
  mutate(rate = all_val * 1e5 / sum(all_pop)) %>%
  ungroup() %>%
  group_by(interval, coverage, strategy, event, vaccine) %>%
  summarise(tot_rate = sum(rate)) %>%
  filter(interval == "mean") %>%
  filter(!event == "hospitalizations") %>%
  ggplot(aes(x = coverage, y = tot_rate, col = strategy, group = strategy)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 18) +
  labs(
    y = "rate per 100,000 population",
    x = "Target vaccine coverage"
  ) +
  facet_grid(event ~ vaccine, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = -90),
    legend.position = "top"
  )



a <- c(1, 2, 3)
b <- c(3, 43, 45)

setNames(list(a, b), c("a1", "b2"))

asdf <- if (FALSE) {
  setNames(list(a, b), c("a1", "b2"))
} else {
  setNames(list(b, a), c("a1", "b2"))
}

fs <- function(x) {
  str(x)
}

fs(x = if (FALSE) {
  setNames(list(a, b), c("a1", "b2"))
} else {
  setNames(list(b, a), c("a1", "b2"))
})