#!/usr/bin/env python
# -*- coding: utf-8 -*- #

import cv2
import numpy as np
import matplotlib.pyplot as plt

import sys
import os


def to_gray(color_img):
    gray = cv2.cvtColor(color_img, cv2.COLOR_BGR2GRAY)
    cv2.imwrite('test_gray.jpg', gray)
    return gray

def gen_sift_features(gray_img):
    sift = cv2.SIFT()
    # kp is the keypoints
    #
    # desc is the SIFT descriptors, they're 128-dimensional vectors
    # that we can use for our final features
    kp, desc = sift.detectAndCompute(gray_img, None)
    return kp, desc

def img_sift_features(gray_img, color_img, kp):
    keypoints = cv2.drawKeypoints(gray_img, kp, color_img.copy())
    cv2.imwrite('test_keypoints.png', keypoints)

os.chdir(os.path.dirname(os.path.abspath(sys.argv[0])))
# I cropped out each stereo image into its own file.
# You'll have to download the images to run this for yourself
img = cv2.imread('VOC2005_1/PNGImages/Caltech_motorbikes_side/0046.png')

img_gray = to_gray(img)

kp, desc = gen_sift_features(img_gray)

img_sift_features(img_gray, img, kp)

desc_out = open('test_desc.txt', 'w')
for i in range(0, len(desc)):
    for j in range(0, 128):
        desc_out.write(str(desc[i][j]).ljust(10))
    desc_out.write('\n')
desc_out.close()