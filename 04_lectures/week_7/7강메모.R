

koica_1_2024xlsx |> 
  separate_wider_delim(
    지역, 
    delim = " > ", 
    names = c("대륙", "지역1", "지역2"), 
    too_few = "align_start"
    #too_many = "merge"
  ) |> 
  select(사업번호, 대륙, 지역1, 지역2)