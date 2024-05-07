Rails.application.routes.draw do
  root "main#main"
  get "/summary", to: "main#summary"
  resources :main
end
