---
# Documentation: https://sourcethemes.com/academic/docs/managing-content/

title: "B.Sc. Thesis"
summary: "High frequency airborne temperature measurements analyzed with AI techniques"
authors: []
tags: []
categories: []
date: 2020-08-02T23:45:00+03:00

# Optional external URL for project (replaces project detail page).
external_link: ""

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Focal points: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight.
image:
  caption: ""
  focal_point: ""
  preview_only: false

# Custom links (optional).
#   Uncomment and edit lines below to show custom links.
# links:
# - name: Follow
#   url: https://twitter.com
#   icon_pack: fab
#   icon: twitter

url_code: ""
url_pdf: ""
url_slides: ""
url_video: ""

# Slides (optional).
#   Associate this project with Markdown slides.
#   Simply enter your slide deck's filename without extension.
#   E.g. `slides = "example-slides"` references `content/slides/example-slides.md`.
#   Otherwise, set `slides = ""`.
slides: ""
---

As part of the ACORES project (short for ‘Azores stratoCumulus measurements Of Radiation, turbulEnce and aeroSols’ - catchy, right?), supervised by prof. Szymon Malinowski, I wrote my Bachelor's Thesis at University of Warsaw.

They were testing a new ultrafast thermometer and noticed a certain type of anomalies that occurred frequently in the collected data. They were somewhat hard to define in a programmatic way, but as they say, you know it when you see it.

![A graph with several sudden jumps marked with arrows](jump.png "Weeeeee!")
<!-- <sub><sup>An example of one of the types of jumps in the dataset.</sup></sub> -->

At the time, I was looking for a thesis project where I could use Machine Learning in some way, so -- let's try! I spent a full day rewatching Netflix shows and clicking through a self-written web app to label the dataset (the alternative was printing it out on a couple hundred pages... yea, let's not), and thus I obtained my dataset.

From that point, the Machine Learning began. For my thesis, I rigorously defined the classification problem and the fundamentals of Machine Learning (it was still a Physics degree, after all), and performed experiments with several models, both "classical" ML, and NN-based ones. Unsurprisingly, recurrent models like GRU and LSTM exhibited the best performance, confirming the applicability of ML for this task - and quite possibly pointed out a few errors I had made while preparing the dataset.