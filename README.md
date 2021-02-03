# eche-MIDI

## GOAL 1

Create a structure/flow where you can record a MIDI excerpt into pure data, parse it into a format that is manipulatable by ML or other means, do some shit to it (in R or something similar), and then convert it back to MIDI to be played by PD.

### What works so far:

* A way to send data between PD and R (and by extension C++)
* A way to convert MIDI to csv

### What needs improvement/exploration:
* Parsing function that converts MIDICSV to an array of words
* Parsing function that converts array of words to MIDICSV
* Generative text ML algorithms

### What we need to figure out:
* How to record MIDI excerpts into PD
* How to play MIDI excerpts in PD

## Other Ideas

* ML as a compositional aid
  * An ableton plug-in that takes a single MIDI sequence as input and outputs variations on that sequence
    * maybe ML could allow it to tailor the variations to the content of the MIDI sequence?
    * could also have it output a "response" sequence that is based on a training data set of call and responses
* ML directly on spectrogram
  * Saw this thing about how ML classifies audio (like speech) by doing image classification on the spectrogram of the sound wave.
    * could apply a generative ML algorithm that is set up to generate images to generating a spectrogram.
* Sending non-midi data between PD and R/C+
* Creating a language to create MIDICSV files
