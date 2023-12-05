from PIL import Image, ImageEnhance
import argparse
import matplotlib.pyplot as plt
import matplotlib.image as mpimg

parser = argparse.ArgumentParser(description='Change Brightness of the image')
parser.add_argument('-i', action="store", dest="input_file")
parser.add_argument('-factor', action="store", dest="factor", default=1)
parser.add_argument('-contrast', action="store", dest="constrast", default=1)
parser.add_argument('-o', action="store", dest="output_file", default="output.png")
args = parser.parse_args()

im = Image.open(args.input_file)

# Image brightness enhancer
enhancer = ImageEnhance.Brightness(im)
im_output = enhancer.enhance(float(args.factor))

# Change contrast
enhancer = ImageEnhance.Contrast(im_output)
im_output = enhancer.enhance(float(args.constrast))
im_output.save(args.output_file)

# Open the image
image = mpimg.imread(args.output_file)
plt.imshow(image)
plt.show()