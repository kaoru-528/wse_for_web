Rails.application.routes.draw do
  root "main#main"
  post "/post_data", to: "main#post_data"
  get "/summary", to: "main#summary"
  resources :main
end
