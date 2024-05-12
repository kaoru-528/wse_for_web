Rails.application.routes.draw do
<<<<<<< HEAD
  # Define your application routes per the DSL in https://guides.rubyonrails.org/routing.html

  # Defines the root path route ("/")
  # root "articles#index"
=======
  root "main#main"
  post "/post_data", to: "main#post_data"
  get "/summary", to: "main#summary"
  resources :main
>>>>>>> main-page
end
