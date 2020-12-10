# light-tracker
This is primarily a development project to get a working pan-tilt
head driven by stepper motors that can be used for a variety of
applications (the one in the back of my mind is pointing a radio
telescope).  This project uses a square 2x2 array of
photo-transistors separated by a X shaped shade and a
microcontroller to drive the steppers to equalize the light to the
phototransistors.

## Dependencies
### 3D Models
* The 3D models use items from my "things" repository at https://github.com/BrentSeidel/Things.git
* The gears come from sadr0b0t's "pd-gears" repository at https://github.com/sadr0b0t/pd-gears.git
### Software
* The software is written in a tiny-lisp from https://github.com/BrentSeidel/Ada-Lisp.git
* The hardware interfaces are from https://github.com/BrentSeidel/BBS-BBB-Ada.git
* I'm using an Arduino-Due as the microcontroller using https://github.com/BrentSeidel/Ada-Arduino-Due.git
* The low-level runtime should eventually wind up in AdaCore's "bb-runtimes" repostory at https://github.com/AdaCore/bb-runtimes.git
* I have a runtime version in my for at https://github.com/BrentSeidel/bb-runtimes.git, but
due to changes upstream, it no longer builds and I haven't had the time
to figure out why.
* It should be possible to adapt this to another microcontroller, either
bare metal or Linux based (such as a Raspberry PI).
