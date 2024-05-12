class MainController < ApplicationController
  def main
  end
  def post_data
    response = HTTParty.post('http://localhost:8000', body: { dt: params[:dt]})
  end
  def summary
    response = HTTParty.get('http://localhost:8000')
    @summary_data = response.parsed_response
  end

end
