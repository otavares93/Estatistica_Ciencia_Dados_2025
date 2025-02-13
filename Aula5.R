#Primeiro acesso ao git? configura globalmente
#Abra o terminal na pasta do projeto e digite os códigos a seguir
#git config --global user.name 'Otto Tavares'
#git config --global user.email 'otavares93@gmail.com'

library(usethis)
usethis::use_git_config(user.name = "Otto Tavares", user.email = "otavares93@gmail.com")
usethis::create_github_token()
#Primeira vez, selecione o token criado e o salve no seu rstudio
gitcreds::gitcreds_set()