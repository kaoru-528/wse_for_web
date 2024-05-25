require 'open3'
class MainController < ApplicationController

  def main
  end
  def calculate
    if params[:func][:Data].present?
      r_script_path = Rails.root.join('lib', 'scripts', 'WaveletShrinkageEstimation.R').to_s

      # Rスクリプトを実行
      stdout, stderr, status = Open3.capture3("Rscript", r_script_path, "#{params[:func][:Data]}", "#{params[:func][:DataTransform]}", "#{params[:func][:ThresholdName]}", "#{params[:func][:ThresholdMode]}")
      if status.success?
        result = stdout.strip.split(" ")
        result_with_index = result.map.with_index(1) { |value, index| [index, value] }
        session[:result] = result_with_index
        redirect_to result_path
      else
        render plain: "R script failed with error: #{stderr}", status: :internal_server_error
      end
    else
      render plain: "No file uploaded", status: :bad_request
    end
  end
  def result
    @results =  session[:result]
  end

end
