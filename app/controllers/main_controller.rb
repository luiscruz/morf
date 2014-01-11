class MainController < ApplicationController
  def home
    @result = flash[:result]
    @text = flash[:text]
  end
  
  def apply_model
    text = params[:text]
    result = tm_r_apply_model text
    flash[:result] = result
    flash[:text] = text
    redirect_to action: :home
  end
  
  protected
  def tm_r_apply_model text

    if text
      begin
        @rserve ||= Rserve::Connection.new
        text.gsub!(/"\n|'/, '')
        result = @rserve.eval("setwd('"+Rails.root.to_s+"/r'); source('apply_model.r'); apply_model('"+text+"')").to_ruby
        result = result["1"]["female"] || "male"
      rescue => e
        logger.error e.message
        logger.error e.backtrace
        result = "error"
      end
    end
  end
end
