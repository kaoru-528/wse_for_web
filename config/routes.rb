Rails.application.routes.draw do
  root "main#main"
  post "/calculate", to: "main#calculate"
  get '/result', to: 'main#result'
  resources :main
end
