require 'open3'
class MainController < ApplicationController

  def main
  end
  def calculate
    if params[:data].present?
      uploaded_file = params[:data]
      file_path = Rails.root.join('tmp', uploaded_file.original_filename)

      r_script_path = Rails.root.join('lib', 'scripts', 'WaveletShrinkageEstimation.R').to_s

      # Rスクリプトを実行
      stdout, stderr, status = Open3.capture3("Rscript", r_script_path," #{params[:DataTransform]}", "#{params[:ThresholdName]}", "#{params[:ThresholdMode]}")
      if status.success?
        result = stdout.strip
        session[:result] = result
        redirect_to result_path
      else
        render plain: "R script failed with error: #{stderr}", status: :internal_server_error
      end
    else
      render plain: "No file uploaded", status: :bad_request
    end
  end
  def result
    @mean =  session[:result]
  end

end
