test_that("Python environment is accessible", {
  # 1. Check if the environment exists first
  env_name <- "icp_conda"
  all_envs <- reticulate::conda_list()$name

  skip_if_not(env_name %in% all_envs, message = "icp_conda environment not found")

  # 2. Force use of the environment
  reticulate::use_condaenv(env_name, required = TRUE)

  # 3. Check availability
  expect_true(reticulate::py_available(initialize = TRUE))
})
