from argparse import ArgumentParser
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.image as mpimg
from skimage import transform
from math import log,exp

# Get the path of the image
parser = ArgumentParser(description = "Perform SVD to compress an image (application of chapter 7)")
parser.add_argument(
        "-file",
        type = str,
        required = True,
        help = "Path of the image to perform SVD on"
    )
Arguments = parser.parse_args()

# Read the image and crop it to 1000x1000 square image
Image = mpimg.imread(Arguments.file)
Image = np.dot(Image[:, :, :3], [.33, .33, .33])
Hauteur, Largeur = Image.shape
if Hauteur > Largeur:
    MilieuH = Hauteur / 2
    Image = Image[int(MilieuH - Largeur / 2):int(MilieuH + Largeur / 2), 0:Largeur]
elif Largeur > Hauteur:
    MilieuL = Largeur / 2
    Image = Image[0:Hauteur, int(MilieuL - Hauteur / 2):int(MilieuL + Hauteur / 2)]
Image = transform.resize(Image, (1000, 1000), anti_aliasing = True)

# Perform SVD
U, D, Vt = np.linalg.svd(Image)
Rangs = [int(exp(x)) for x in np.linspace(0, log(1000), num = 9)]

# Display result
fig, axes = plt.subplots(3, 3, figsize=(12, 12))
for i in range(3):
    for j in range(3):
        RangCurrent = Rangs[int(i * 3 + j)]
        Reconstruction = np.dot(U[:, 0:RangCurrent], np.dot(np.diag(D[0:RangCurrent]), Vt[0:RangCurrent, :]))
        axes[i, j].imshow(Reconstruction, cmap = "gray")
        axes[i, j].set_title("Rank = " + str(RangCurrent))
plt.tight_layout()  
plt.show()





