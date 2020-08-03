---
# Documentation: https://sourcethemes.com/academic/docs/managing-content/

title: "M.Sc. Thesis @ Aalto University"
summary: "Improving Ad-Hoc Cooperation in Multiagent Reinforcement Learning via Skill Modeling"
authors: []
tags: []
categories: []
date: 2020-08-03T00:38:19+03:00

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
My Master's Thesis was done at Aalto University, supervised by Alexander Ilin and advised by Antti Keurulainen from Bitville Oy. 

The inspiration of this research was the concept of Theory of Mind, taken from developmental psychology. The main experiment used for showcasing ToM capabilities is the so-called Sally-Anne experiment:

![A comic describing the Sally-Anne test from the original paper](Sally-Anne_test.jpg "Professional academic illustrations")

There are three characters involved: Sally, Anne, and the Observer whose ToM capabilities we wish to test. Sally and Anne are in a room with a box, a basket and a marble. Sally puts the marble in the basket, and then goes out of the room for a brief walk. In her absence, Anne moves the marble to the b ox. Now, Sally comes back, and the question to the Observer arises - where will Sally look for the marble?

As (presumably) an adult human, the dear reader should know the answer is the basket -- she has no knowledge that would indicate it being elsewhere. However, a naive observer (like a very young child or some animals) could answer with the box instead -- the marble is there, after all. The difference between these two answers lies in the ability to "put yourself in someone else's shoes", to reason about what someone else knows rather than using only our (perhaps superior) knowledge. In their paper [Machine Theory of Mind](https://arxiv.org/abs/1802.07740), Rabinowitz et al. show that this is, in fact, possible to create in RL-based AI systems. 

Now let's focus our attention on one specific type of ToM -- skill modeling. Can we build an agent that will accurately predict the skill level of another agent acting in the same environment, and adjust its actions accordingly? This is the question I explored in my thesis, and the answer is a resounding "maybe".

I coded up an environment using [pycolab](https://github.com/deepmind/pycolab) with a gym-like multiagent API and began the experiments.

The basic idea of the environment is as follows: on a 7x7 gridworld, there are 2 agents, 4 subgoals and a final goal. Agents can move up/down/right/left at each step, and whenever they move onto a subgoal, it's marked as collected. Once all subgoals are collected, the final goal becomes available, and once it's collected, the episode ends. The environment is fully cooperative, which means that all the rewards (obtained by collecting subgoals and the final goal) are shared between the agents.

![The interface of a manual mode of the environment, with the gridworld visualized and several other displays alongside it](manual.png "At some point I'll try to embed it here... but not today")

That is the base setting, at least. In the meantime, I added some simplified variants, like the Final Action environment in which there is an [Assurance Game](https://wiki.p2pfoundation.net/Assurance_Game) embedded at the end of an episode to put a larger emphasis on the cooperation aspect of the environment.

For further details -- feel free to read the full pdf version of my thesis available at the top of this page.