# Utility functions to calculate the final size of an epidemic, the total number of hospitalizations and deaths

library(nleqslv)

source("./NGM_utils.R")

# Calculate the final size using the final size equation
calc_final_size <- function(params, vacc_prop = rep(0, 16)) {
  vacc_prop <- vacc_prop + rep(0.000001, 16)
  NGM <- calc_NGM_demergered_R(params, vacc_prop)
  with(params, {
    pop_size <- unlist(c((1 - vacc_prop) * population_size, vacc_prop * population_size))
    # print(pop_size)
    f <- function(x) {
      x - exp(-(diag(1 / pop_size) %*% NGM %*% diag(pop_size)) %*% (1 - x))
    }
    J <- function(x) {
      diag(32) - diag(as.vector(exp(-NGM %*% (1 - x)))) %*% NGM
    }
    sp <- rep(0.5, 32)
    final_size <- 1 - nleqslv(sp, f, jac = J, global = "cline")$x
    # print(sum(final_size*pop_size/(sum(pop_size))))

    if (min(final_size) < 0) {
      sp <- sp / 1000
      final_size <- 1 - nleqslv(sp, f, jac = J, global = "cline")$x
      final_size[final_size < 0] <- 0
    }

    return(final_size)
  })
}



# Calculate the number of infections, hospitalizations and deaths
calc_final_burden <- function(params,
                              vacc_prop = rep(0, 16)) {
  final_size <- calc_final_size(
    params,
    vacc_prop
  )

  with(params, {
    pop_size <- unlist(c(
      (1 - vacc_prop) * population_size,
      vacc_prop * population_size
    ))

    infections <- final_size * pop_size
    hospitalizations <- epi$hospitalization_rate * (1 - seropositivity) *
      c(
        final_size[1:16] * pop_size[1:16],
        (1 - efficacy$Vsev) * final_size[17:32] * pop_size[17:32]
      )
    deaths <- epi$infection_fatality_rate * (1 - seropositivity) *
      c(
        final_size[1:16] * pop_size[1:16],
        (1 - efficacy$Vsev) * (1 - efficacy$Vmor) * final_size[17:32] * pop_size[17:32]
      )

    return(
      list(
        "infections" = infections,
        "hospitalizations" = hospitalizations,
        "deaths" = deaths
      )
    )
  })
}




# Calculate the number of infections, hospitalizations and deaths
calc_final_burden_tibble <- function(params,
                                     vacc_prop = rep(0, 16)) {
  final_size <- calc_final_size(
    params,
    vacc_prop
  )

  with(params, {
    pop_size <- unlist(c(
      (1 - vacc_prop) * population_size,
      vacc_prop * population_size
    ))

    infections <- final_size * pop_size
    hospitalizations <- epi$hospitalization_rate * (1 - seropositivity) *
      c(
        final_size[1:16] * pop_size[1:16],
        (1 - efficacy$Vsev) * final_size[17:32] * pop_size[17:32]
      )
    deaths <- epi$infection_fatality_rate * (1 - seropositivity) *
      c(
        final_size[1:16] * pop_size[1:16],
        (1 - efficacy$Vsev) * (1 - efficacy$Vmor) * final_size[17:32] * pop_size[17:32]
      )

    return(
      tibble(
        age_ind = rep(1:16, 2),
        vacc_status = rep(c("unvaccinated", "vaccinated"), each = 16),
        infections = infections,
        hospitalizations = hospitalizations,
        deaths = deaths,
        pop_size = pop_size
      )
    )
  })
}