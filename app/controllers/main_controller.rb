class MainController < ApplicationController
  def main
  end
  def summary
    response = HTTParty.get('http://localhost:8000')
    @summary_data = response.parsed_response
  end

end
