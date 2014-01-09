class MainController < ApplicationController
  def home
    @result = params[:result]
  end
  
  def apply_model
    result = tm_r_apply_model params[:text]
    redirect_to action: :home, result: result, text: params[:text]
  end
  
  protected
  def tm_r_apply_model text

    if text
      begin
        @rserve ||= Rserve::Connection.new
        text.gsub!(/"\n|'/, '')
        result = @rserve.eval("setwd('"+Rails.root.to_s+"/r'); source('apply_model.r'); apply_model('"+text+"')").to_ruby
        result = result["female"] || "male"
      rescue => e
        logger.error e.message
        logger.error e.backtrace
        result = "error"
      end
    end
  end
end
