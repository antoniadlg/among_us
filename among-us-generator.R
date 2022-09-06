
tasks <- read.csv("~/Downloads/amongus_tasks.csv")

task_selector <- function(n, 
                          n_crewmembers,
                          n_imposters, 
                          n_tasks) 
  #' @Description: Takes in a number of people and 
  #' @param n: integer, total players
  #' @param n_crewmembers: integer, number of people playing 
  #' @param n_imposters: integer, number of killers in the game 
  #' @Returns: A dataframe, with each number mapped to a member type and their assigned tasks 
{
  numbers <- seq(1:n)
  member_type <- c("killer", "crewmember")
  
  task_tibble <- tibble(
    number = numeric(),
    killer = logical(),
    tasks = list())
  
  killers <- sample(numbers, n_imposters, replace = FALSE)
  non_imposters <- numbers[!(numbers %in% killers)]
  
  for (i in 1:length(killers)) {
    task_tibble <- 
      task_tibble %>% add_row(number = killers[i], killer = TRUE, tasks = NULL)
  }
  
  for (i in 1:length(non_imposters)) {
    
    member_tasks <- tasks %>% sample_n(size = n_tasks)
    
    all_tasks <- member_tasks %>% pull(Task.number) %>% sort() %>% list()
    task_tibble <- 
      task_tibble %>% 
      add_row(number = non_imposters[i],
              killer = FALSE, 
              tasks = all_tasks)
  }
  
  task_tibble <- 
    task_tibble %>% mutate(tasks = as.character(tasks))

    return(task_tibble)
}


players <- c("Ant", "Bean", "Vlada", 
             "Steve", "Rika", "Claudia", 
             "Reuben", "Alay", "Savera", 
             "Geunho", "David")

assign_player_numbers <- 
  function(players) {
    #' @Description: Takes in all players and assigns them a random number
    #' @param players: A vector of all the players that will be playing in a round
    #' @Returns: Dataframe with each name having an assigned number

  player_nums <- seq(1:length(players))
  
    player <- sample(players, size = length(players), replace = F)
    player_number <- sample(player_nums, size = length(player_nums), replace = F)
  
    assigned_numbers <- 
      tibble(
        number = player_number, 
        players = player
      )
  return(assigned_numbers)
    
}

# Run to play! 

player_tasks <- task_selector(n = 11, n_crewmembers = 8, n_imposters = 3, n_tasks = 4)
numbers <- assign_player_numbers(players = players)

task_assignment_umasked <- player_tasks %>% left_join(numbers) 
filepath <- "~/Desktop/amongus"

for(player in task_assignment_umasked$players) {
  task <-
    task_assignment_umasked %>% 
    filter(players == player) %>% 
    select(player_name = players, killer, tasks)
  
  write.csv(task, paste0(filepath, "/", player, ".csv"))
}
