<details><summary>Reproducibility receipt</summary>
  
  ```{r}
## datetime
Sys.time()

## repository
if(requireNamespace('git2r', quietly = TRUE)) {
  git2r::repository()
} else {
  c(
    system2("git", args = c("log", "--name-status", "-1"), stdout = TRUE),
    system2("git", args = c("remote", "-v"), stdout = TRUE)
  )
}

## session info
sessionInfo()
```

</details>