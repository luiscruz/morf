class MainController < ApplicationController
  def index
    @result = params[:result]
  end
  
  def apply_model
    result = tm_r_apply_model params[:text]
    redirect_to action: :index, result: result, text: params[:text]
  end
  
  protected
  def tm_r_apply_model text
    if text
      @rserve ||= Rserve::Connection.new
      text.gsub!(/"\n|'/, '')
      result = @rserve.eval("setwd('"+Rails.root.to_s+"/r'); source('apply_model.r'); apply_model('"+text+"')").to_ruby
      result = result["female"] || "male"
    end
  end
  
end
