# This program serves an intro to R

# Define a function RandomMenu
RandomMenu <- function()
{
  # Display a menu that allows user to select a distribution
  cat("Select a distribution:\n")
  cat("1. Normal Distribution\n")
  cat("2. Uniform Distribution\n")
  cat("3. Exponential Distribution\n")
  cat("4. F Distribution\n")
  cat("5. Chi-Squared Distribution\n")
  cat("6. T distribution\n")
}

# Define a function GetChoice 
GetDistr_Choice <- function()
{
  # Prompt the user to enter their desired distribution choice
  choice <- as.integer(readline(prompt = 'Enter your desired choice(1 - 6): '))
  
  # Return the user's choice
  return(choice)
}

# define a function GenerateRandom 
GenerateRandom <- function(choice)
{
  # generate random numbers from a chosen distribution
  if (choice == 1)
  {
    num <- as.integer(readline(prompt = "Enter number of values to generate: "))
    mean <- as.numeric(readline(prompt = "Enter mean: "))
    std_deviation <- as.numeric(readline(prompt = "Enter standard deviation: "))
    random_numbers <- rnorm(num, mean, std_deviation)
    return(random_numbers)
  }
  
  else if (choice == 2)
  {
    num <- as.integer(readline(prompt = "Enter number of values to generate: "))
    min <- as.numeric(readline(prompt = "Enter the minimum value: "))
    max <- as.numeric(readline(prompt = "Enter the maximum value: "))
    random_numbers <- runif(num, min, max)
    return(random_numbers)
  }
  
  else if (choice == 3)
  {
    num <- as.integer(readline(prompt = "Enter number of values to generate: "))
    lambda <- as.numeric(readline(prompt = "Enter the lambda value: "))
    random_numbers <- rexp(num, lambda)
    return(random_numbers)
  }
  
  else if (choice == 4)
  {
    num <- as.integer(readline(prompt = "Enter number of values to generate: "))
    df1 <- as.integer(readline(prompt = "Enter degrees of freedom 1: "))
    df2 <- as.integer(readline(prompt = "Enter degrees of freedom 2: "))
    random_numbers <- rf(num, df1, df2)
    return(random_numbers)
  }
  
  else if (choice == 5)
  {
    num <- as.integer(readline(prompt = "Enter number of values to generate: "))
    df <- as.integer(readline(prompt = "Enter degrees of freedom: "))
    random_numbers <- rchisq(num, df)
    return(random_numbers)
  }
  
  else if (choice == 6)
  {
    num <- as.integer(readline(prompt = "Enter number of values to generate: "))
    df <- as.integer(readline(prompt = "Enter degrees of freedom: "))
    random_numbers <- rt(num, df)
    return(random_numbers)
  }
  
  else
  {
    cat("Invalid choice. Please select a number between 1 and 6.\n")
    return(NULL)
  }
}

# Define a function PlotHistogram
PlotHistogram <- function(data)
{
  # Plot a histogram of the generated random numbers
  hist(data,
       main = "Random numbers Histogram",
       xlab = "Random numbers",
       col = "blue",
       border = "black")
}

# Define Get_MeasuresOfSpread
Get_MeasuresOfSpread <- function(random_numbers) 
{
  # Display a menu that allows user to select a measure of spread to calculate
  cat("Select a measure of spread to calculate:\n")
  cat("1. Mean\n")
  cat("2. Standard Deviation\n")
  cat("3. Variance\n")
  
  # Prompt the user to enter their desired measure of spread choice
  choice <- as.integer(readline(prompt = 'Enter your desired choice(1 - 3): '))
  
  # Calculate and return the selected measure of spread
  if (choice == 1)
  {
    result <- mean(random_numbers)
    cat("Mean:", result, "\n")
    return(result)
  }
  
  else if (choice == 2)
  {
    result <- sd(random_numbers)
    cat("Standard Deviation:", result, "\n")
    return(result)
  }
  
  else if (choice == 3)
  {
    result <- var(random_numbers)
    cat("Variance:", result, "\n")
    return(result)
  }
  
  else
  {
    cat("Invalid choice. Please select a number between 1 and 3.\n")
    return(NULL)
  }
}

# Define a function DistributionMenu
DistributionMenu <- function()
{
  # Display a menu that allows user to select a distribution
  cat("Select a distribution:\n")
  cat("1. Normal Distribution\n")
  cat("2. Uniform Distribution\n")
  cat("3. Exponential Distribution\n")
  cat("4. F Distribution\n")
  cat("5. Chi-Squared Distribution\n")
  cat("6. T distribution\n")
}

# Define a function GetProbabilities
GetProbabilities <- function() 
{
  # Display a submenu to determine the type of probability the user wishes to find
  cat("Select the type of probability to calculate:\n")
  cat("1. P(X <= x)\n")
  cat("2. P(X >= x)\n")
  cat("3. P(a < X < b)\n")
  
  # Get user choice
  prob_choice <- as.integer(readline(prompt = "Enter desired choice(1 - 3): "))
  
  if (prob_choice == 1)
  {
    cat("You selected P(X <= x)\n")
    
    DistributionMenu()
    choice <- as.integer(readline(prompt = "Select desired distribution(1 - 6): "))
    
    if (choice == 1)
    {
      mean <- as.numeric(readline(prompt = "Enter the mean: "))
      std_deviation <- as.numeric(readline(prompt = "Enter the standard deviation: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- pnorm(x, mean, std_deviation)
    }
    
    else if (choice == 2)
    {
      min <- as.numeric(readline(prompt = "Enter min: "))
      max <- as.numeric(readline(prompt = "Enter max: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- punif(x, min, max)
    }
    
    else if (choice == 3)
    {
      lambda <- as.numeric(readline(prompt = "Enter lambda value: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- pexp(x, lambda)
    }
    
    else if (choice == 4)
    {
      df1 <- as.numeric(readline(prompt = "Enter degrees of freedom 1: "))
      df2 <- as.numeric(readline(prompt = "Enter degrees of freedom 2: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- pf(x, df1, df2)
    }
    
    else if (choice == 5)
    {
      df <- as.numeric(readline(prompt = "Enter the degrees of freedom: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- pchisq(x, df)
    }
    
    else if (choice == 6)
    {
      df <- as.numeric(readline(prompt = "Enter degrees of freedom: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- pt(x, df)
    }
    
    else
    {
      cat("Invalid choice.\n")
      return(NULL)
    }
    
    cat("Probability:", probability, "\n")
    return(probability)
  }
  
  else if (prob_choice == 2)
  {
    cat("You selected P(X >= x)\n")
    cat("This can be calculated as 1 - P(X <= x)\n")
    
    DistributionMenu()
    choice <- as.integer(readline(prompt = "Select desired distribution(1 - 6): "))
    
    if (choice == 1)
    {
      mean_val <- as.numeric(readline(prompt = "Enter mean: "))
      sd_val <- as.numeric(readline(prompt = "Enter sd: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- 1 - pnorm(x, mean = mean_val, sd = sd_val)
    }
    
    else if (choice == 2)
    {
      min_val <- as.numeric(readline(prompt = "Enter min: "))
      max_val <- as.numeric(readline(prompt = "Enter max: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- 1 - punif(x, min = min_val, max = max_val)
    }
    
    else if (choice == 3)
    {
      rate <- as.numeric(readline(prompt = "Enter rate: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- 1 - pexp(x, rate = rate)
    }
    
    else if (choice == 4)
    {
      df1 <- as.numeric(readline(prompt = "Enter df1: "))
      df2 <- as.numeric(readline(prompt = "Enter df2: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- 1 - pf(x, df1 = df1, df2 = df2)
    }
    
    else if (choice == 5)
    {
      df <- as.numeric(readline(prompt = "Enter df: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- 1 - pchisq(x, df = df)
    }
    
    else if (choice == 6)
    {
      df <- as.numeric(readline(prompt = "Enter df: "))
      x <- as.numeric(readline(prompt = "Enter x: "))
      probability <- 1 - pt(x, df = df)
    }
    
    else
    {
      cat("Invalid choice.\n")
      return(NULL)
    }
    
    cat("Probability:", probability, "\n")
    return(probability)
  }
  
  else if (prob_choice == 3)
  {
    cat("You selected P(a < X < b)\n")
    
    DistributionMenu()
    choice <- as.integer(readline(prompt = "Select desired distribution(1 - 6): "))
    
    if (choice == 1)
    {
      mean_val <- as.numeric(readline(prompt = "Enter mean: "))
      sd_val <- as.numeric(readline(prompt = "Enter sd: "))
      a <- as.numeric(readline(prompt = "Enter a: "))
      b <- as.numeric(readline(prompt = "Enter b: "))
      probability <- pnorm(b, mean = mean_val, sd = sd_val) - pnorm(a, mean = mean_val, sd = sd_val)
    }
    
    else if (choice == 2)
    {
      min_val <- as.numeric(readline(prompt = "Enter min: "))
      max_val <- as.numeric(readline(prompt = "Enter max: "))
      a <- as.numeric(readline(prompt = "Enter a: "))
      b <- as.numeric(readline(prompt = "Enter b: "))
      probability <- punif(b, min = min_val, max = max_val) - punif(a, min = min_val, max = max_val)
    }
    
    else if (choice == 3)
    {
      rate <- as.numeric(readline(prompt = "Enter rate: "))
      a <- as.numeric(readline(prompt = "Enter a: "))
      b <- as.numeric(readline(prompt = "Enter b: "))
      probability <- pexp(b, rate = rate) - pexp(a, rate = rate)
    }
    
    else if (choice == 4)
    {
      df1 <- as.numeric(readline(prompt = "Enter df1: "))
      df2 <- as.numeric(readline(prompt = "Enter df2: "))
      a <- as.numeric(readline(prompt = "Enter a: "))
      b <- as.numeric(readline(prompt = "Enter b: "))
      probability <- pf(b, df1 = df1, df2 = df2) - pf(a, df1 = df1, df2 = df2)
    }
    
    else if (choice == 5)
    {
      df <- as.numeric(readline(prompt = "Enter df: "))
      a <- as.numeric(readline(prompt = "Enter a: "))
      b <- as.numeric(readline(prompt = "Enter b: "))
      probability <- pchisq(b, df = df) - pchisq(a, df = df)
    }
    
    else if (choice == 6)
    {
      df <- as.numeric(readline(prompt = "Enter df: "))
      a <- as.numeric(readline(prompt = "Enter a: "))
      b <- as.numeric(readline(prompt = "Enter b: "))
      probability <- pt(b, df = df) - pt(a, df = df)
    }
    
    else
    {
      cat("Invalid choice.\n")
      return(NULL)
    }
    
    cat("Probability:", probability, "\n")
    return(probability)
  }
  
  else
  {
    cat("Invalid choice.\n")
    return(NULL)
  }
}

# Define a function ApproximateIntegrals
ApproximateIntegrals <- function()
{
  # Display a menu that allows user to select a function to integrate
  cat("Select a function to integrate:\n")
  cat("1. Normal Distribution (dnorm)\n")
  cat("2. Uniform Distribution (dunif)\n")
  cat("3. Exponential Distribution (dexp)\n")
  
  # Get user choice
  choice <- as.integer(readline(prompt = "Enter desired choice(1 - 3): "))
  
  # Get integration bounds and number of samples
  a <- as.numeric(readline(prompt = "Enter lower bound: "))
  b <- as.numeric(readline(prompt = "Enter upper bound: "))
  n <- as.integer(readline(prompt = "Enter number of samples: "))
  
  # Select the function based on user choice
  if (choice == 1)
  {
    g <- dnorm
  }
  
  else if (choice == 2)
  {
    g <- dunif
  }
  
  else if (choice == 3)
  {
    g <- dexp
  }
  
  else
  {
    cat("Invalid choice.\n")
    return(NULL)
  }
  
  # Generate uniform random numbers and approximate the integral
  x <- runif(n, min = a, max = b)
  result <- (b - a) * mean(g(x))
  
  cat("Approximate integral:", result, "\n")
  return(result)
}

# Define a function DetermineQuantiles
DetermineQuantiles <- function()
{
  # Display a menu that allows user to select a distribution
  DistributionMenu()
  choice <- as.integer(readline(prompt = "Select desired distribution(1 - 6): "))
  
  # Get the probability value from the user
  p <- as.numeric(readline(prompt = "Enter probability (0 - 1): "))
  
  # Calculate and return the quantile for the selected distribution
  if (choice == 1)
  {
    mean_val <- as.numeric(readline(prompt = "Enter mean: "))
    sd_val <- as.numeric(readline(prompt = "Enter standard deviation: "))
    quantile <- qnorm(p, mean = mean_val, sd = sd_val)
  }
  
  else if (choice == 2)
  {
    min_val <- as.numeric(readline(prompt = "Enter min: "))
    max_val <- as.numeric(readline(prompt = "Enter max: "))
    quantile <- qunif(p, min = min_val, max = max_val)
  }
  
  else if (choice == 3)
  {
    rate <- as.numeric(readline(prompt = "Enter rate: "))
    quantile <- qexp(p, rate = rate)
  }
  
  else if (choice == 4)
  {
    df1 <- as.numeric(readline(prompt = "Enter degrees of freedom 1: "))
    df2 <- as.numeric(readline(prompt = "Enter degrees of freedom 2: "))
    quantile <- qf(p, df1 = df1, df2 = df2)
  }
  
  else if (choice == 5)
  {
    df <- as.numeric(readline(prompt = "Enter degrees of freedom: "))
    quantile <- qchisq(p, df = df)
  }
  
  else if (choice == 6)
  {
    df <- as.numeric(readline(prompt = "Enter degrees of freedom: "))
    quantile <- qt(p, df = df)
  }
  
  else
  {
    cat("Invalid choice.\n")
    return(NULL)
  }
  
  cat("Quantile:", quantile, "\n")
  return(quantile)
}

# Define a function MainMenu
MainMenu <- function()
{
  cat("Main Menu:\n")
  cat("1. Generate random numbers\n")
  cat("2. Plot histogram\n")
  cat("3. Calculate measures of spread\n")
  cat("4. Calculate probabilities\n")
  cat("5. Approximate integrals\n")
  cat("6. Determine quantiles\n")
  cat("7. Exit\n")
}

# Define the main function to run the program
main <- function()
{
  running <- TRUE
  random_numbers <- NULL
  
  # Loop to display the menu and execute user-selected options until the user chooses to exit
  while (running)
  {
    MainMenu()
    choice <- as.integer(readline(prompt = "Enter your choice (1 - 7): "))
    
    if (choice == 1)
    {
      RandomMenu()
      distr_choice <- GetDistr_Choice()
      random_numbers <- GenerateRandom(distr_choice)
      cat("Random numbers generated successfully.\n")
    }
    
    else if (choice == 2)
    {
      # Check if random numbers have been generated before plotting
      if (!is.null(random_numbers))
      {
        PlotHistogram(random_numbers)
      }
      else
      {
        cat("Please generate random numbers first.\n")
      }
    }
    
    else if (choice == 3)
    {
      # Check if random numbers have been generated before calculating measures
      if (!is.null(random_numbers))
      {
        Get_MeasuresOfSpread(random_numbers)
      }
      else
      {
        cat("Please generate random numbers first.\n")
      }
    }
    
    else if (choice == 4)
    {
      GetProbabilities()
    }
    
    else if (choice == 5)
    {
      ApproximateIntegrals()
    }
    
    else if (choice == 6)
    {
      DetermineQuantiles()
    }
    
    else if (choice == 7)
    {
      cat("Exiting the program. Goodbye!\n")
      running <- FALSE
    }
    
    else
    {
      cat("Invalid choice.\n")
    }
  }
}

# Run the main function to start the program
main()
