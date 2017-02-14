import cv2
import sys
import os

# Set wd to this file's location
os.chdir(os.path.dirname(os.path.abspath(sys.argv[0])))

# parameters
#gamma = 0.9 #contrastThreshold
delta = 8 #sampling step

# Loop through 2 datasets
for i in range(1,3):
    img_path = "VOC2005_" + str(i) + "/PNGImages/"
    desc_path = "VOC2005_" + str(i) + "/DescSIFTs/"
    if not os.path.exists(desc_path):
        os.makedirs(desc_path)
    kp_path = "VOC2005_" + str(i) + "/kpSIFTs/"
    if not os.path.exists(kp_path):
        os.makedirs(kp_path)

    # Get subdir list
    subdir = os.listdir(img_path)

    # Loop through subdir
    for j in range(0, len(subdir)):
        img_dir = img_path + subdir[j]
        desc_dir = desc_path + subdir[j]
        if not os.path.exists(desc_dir):
            os.makedirs(desc_dir)
        kp_dir = kp_path + subdir[j]
        if not os.path.exists(kp_dir):
            os.makedirs(kp_dir)

        # Get image list
        imgs = os.listdir(img_dir)

        # Loop through images
        for k in range(0, len(imgs)):
            name = imgs[k].split('.')[0]

            # Extract descriptors
            img = cv2.imread(img_dir + '/' + imgs[k])
            gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

            dense = cv2.FeatureDetector_create("Dense")
            dense.setInt('initXyStep', delta)
            kp = dense.detect(gray)

            sift = cv2.SIFT()
            #sift = cv2.SIFT(contrastThreshold=gamma)
            kp, desc = sift.compute(gray, kp)

            remove = []
            for i in range(0, len(desc)):
                if sum(desc[i]) == 0:
                    remove.append(i)

            for i in sorted(remove, reverse=True):
                del kp[i]

            keypoints = cv2.drawKeypoints(img, kp, img.copy())
            cv2.imwrite(kp_dir + '/' + imgs[k], keypoints)

            desc_out = open(desc_dir + '/' + name + '.txt', 'w')
            for l in range(0, len(desc)):
                for m in range(0, 128):
                    desc_out.write(str(desc[l][m]).ljust(10))
                desc_out.write('\n')
            desc_out.close()
