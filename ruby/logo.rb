require 'RMagick'

def create_logo(company_name) 
  clown = Magick::ImageList.new("logo_background.jpg")
  text = Magick::Draw.new
  text.annotate(clown, 0, 0, 0, 60, company_name) {
    self.gravity = Magick::SouthGravity
    self.pointsize = 48
    self.stroke = 'transparent'
    self.fill = '#0000A9'
    self.font_weight = Magick::BoldWeight
  }
  clown.write('annotate.jpg')
end
