library(rhub)

rhub_setup()
rhub_doctor()

envs <- rhub_platforms()
envs$name
str(envs)
unlist(envs$aliases)

rhub_check(platforms = "clang-asan")
rhub_check(platforms = "linux", branch = "dev")
rhub_check(platforms = "clang22", branch = "dev")

?rhubv2
?local_check_linux

envs
