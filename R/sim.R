calc_yield_loss <- function(pest_density, percent_refuge, recessive_genotype_count, plant_density, acres, bpa) {
  refuge_loss = min(((.765*pest_density)/100), 1)
  bt_res_pest_density = 0
  if(percent_refuge==1) {
    bt_res_pest_density=recessive_genotype_count/(acres*plant_density)
  } else {
    bt_res_pest_density=recessive_genotype_count/((1-percent_refuge)*acres*plant_density)
  }
  bt_loss = min(((.765*bt_res_pest_density)/100), 1)
  loss = refuge_loss*bpa*percent_refuge + bt_loss*bpa*(1-percent_refuge)
  return(loss)
}

calc_crop_loss <- function(yield_loss, percent_refuge, bpa) {
  return(bpa*yield_loss)
}

calc_crop_yield <- function(bpa, crop_loss) {
  return(max(0, bpa - crop_loss))
}

calc_offspring_count <- function(n_insects) {
  n_female = 0.5 * n_insects
  eggs_per_female = (n_female/200)*rnorm(200,mean=1000, sd=200)
  success_rate_of_egg = 0.2
  offspring = round(sum(eggs_per_female * success_rate_of_egg),0)
  return(offspring)
}

gen_survivors <- function(n_insects, res_allele_freq, percent_refuge, pest_density, bpa, acreage, plant_density, h, s, selection_mode, df) {
  # calculate percent that are completely resistant
  q_sq = res_allele_freq^2
  p_sq = (1-res_allele_freq)^2
  h_sq = 2 * res_allele_freq * (1-res_allele_freq)
  total_plants = acreage * plant_density
  #####################
  # calculate fitness values based on selection type
  w11 = 0
  w12 = 0
  w22 = 0
  if(selection_mode == "darD") {
    w11 = 1 + s
    w12= 1 + h * s
    w22 = 1
  } else if(selection_mode == "darR") {
    w11 = 1
    w12= 1 + h * s
    w22 = 1 + s
  } else if(selection_mode == "puri") {
    w11 = 1
    w12= 1 - h * s
    w22 = 1 - s
  } else if(selection_mode == "recl") {
    w11= 1
    w12 = 1
    w22 = 0
  } else if(selection_mode == "detr") {
    w11 = 1
    w12 = 1
    w22 = 1 - s
  } else if(selection_mode == "deta") {
    w11 = 1
    w12 = 1 - s/2
    w22 = 1 - s
  } else if(selection_mode == "detd") {
    w11 = 1
    w12 = 1 - s
    w22 = 1 - s
  }
  ####################
  percent_bt = 1 - percent_refuge
  recessive_genotype_count = round(n_insects * q_sq,0)
  bt_sus_insects = n_insects - recessive_genotype_count

  # P(not on refuge plant)
  death_causing_plants = rbinom(1, total_plants, percent_bt)
  deaths = (p_sq * pest_density) * death_causing_plants
  new_n_insects = n_insects - deaths
  new_n_insects = round(calc_offspring_count(new_n_insects),0)
  new_pest_density = new_n_insects / (acreage * plant_density)
  yield_loss = calc_yield_loss(pest_density, percent_refuge, recessive_genotype_count, plant_density, acreage, bpa)
  new_raf = 0
  if(1-res_allele_freq != 1 && new_n_insects > 0) {
    new_raf = (((h_sq / 2) * w12)+(q_sq*w22)) / (p_sq*w11 + h_sq*w12 + q_sq*w22)
  }
  r = c(new_n_insects, new_raf, percent_refuge, new_pest_density, bpa-yield_loss)
  return(r)
}

calc_generations <- function(num_gens, num_insects, raf, rp, pest_density, bpa, acreage, plant_density, h, s, selection_mode, df) {
  df[1, ] <- c(1,num_insects,raf,rp,pest_density,bpa - calc_yield_loss(pest_density, rp, (raf^2*num_insects), plant_density, acreage, bpa))
  for(i in 2:num_gens) {
    result = gen_survivors(df[i-1,2], df[i-1, 3], rp, df[i-1, 5], bpa, acreage, plant_density, h, s, selection_mode, df)
    df[i, ] = c(i,result)
  }
  return(df)
}


simulate <- function(generations=10,n_insects=100000,resistant_allele_freq=0,refuge_crop_percentage=0.2,bushels_per_acre=150, acreage=10, plant_density=25000, h=0.5, s=0.5, selection_mode="darR") {
  pest_density = n_insects / (acreage * plant_density)

  df <- data.frame(generation=integer(),
                   n_pests=integer(),
                   res_allele_freq=double(),
                   percent_refuge=double(),
                   pest_density=integer(),
                   bushels_per_acre=double())


  decline = calc_generations(generations,
                             n_insects,
                             resistant_allele_freq,
                             refuge_crop_percentage,
                             pest_density,
                             bushels_per_acre,
                             acreage,
                             plant_density,
                             h,
                             s,
                             selection_mode,
                             df)
  return(decline)

}
