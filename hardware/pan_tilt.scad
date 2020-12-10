//
//  Models for pan and tilt mounting
//
use <../../Things/bbs_constants.scad>
use <../../Things/bbs_shapes.scad>
use <../../Things/bbs_stepper.scad>
use <../../pd-gears/pd-gears.scad>

//
//  This is the base that everything rests on.  it contains a hole for the vertical
//  shaft and a mount for a stepper motor.
//
module base()
{
  difference()
  {
    union()
    {
      cube([100, 100, 4]);
    }
    union()
    {
      for(x = [10:20:90])
      {
        for(y = [10:20:90])
        {
          translate([x, y, -0.5]) cylinder(r=screw_8_size()/2, h=5);
        }
      }
    }
  }
}
//
//  This part sits on the base.  It has an integrated gear that interfaces with with a gear
//  on the stepper motor.
//
module pan_panel(tilt_shaft)
{
  d1 = pitch_radius(5, 50);
  d2 = pitch_radius(5, 20);
  difference()
  {
    union()
    {
      cube([50, 4, 25]);
      hull()
      {
        translate([25, 4, 25]) rotate([90, 0, 0]) cylinder(r=25, h=4);
        translate([25, 4, 100]) rotate([90, 0, 0]) cylinder(r=15, h=4);
      }
      translate([50, 4, 0]) rotate([0, -90, 0]) bbs_fillet(10, 50);
      translate([0, 0, 0]) rotate([0, -90, 180]) bbs_fillet(10, 50);
    }
    union()
    {
      translate([25, 4.5, 100]) rotate([90, 0, 0]) cylinder(r=tilt_shaft, h=5);
      translate([25, 4.5, 20]) rotate([90, 0, 0]) cylinder(r=7, h=5);
      translate([25, 4.5, 80]) rotate([90, 0, 0]) cylinder(r=10, h=5);
      translate([25, 4.5, 100-d1-d2]) rotate([90, 0, 0]) bbs_NEMA17_holes(5, true);
    }
  }
}

module pan(pan_shaft, tilt_shaft)
{
  difference()
  {
    gear(5, 100, 4, 1);
    union()
    {
      translate([0, 0, -0.5]) cylinder(r=pan_shaft, h=5);
      for(t=[0:60:360])
      {
        rotate([0, 0, t]) translate([30, 0, -0.5]) cylinder(r=10, h=5);
      }
      for(t=[-45:30:45])
      {
        rotate([0, 0, t]) translate([60, 0, -0.5]) cylinder(r=10, h=5);
        rotate([0, 0, t]) translate([-60, 0, -0.5]) cylinder(r=10, h=5);
      }
    }
  }
  translate([-25, -59, 4]) pan_panel(tilt_shaft);
  translate([-25, 55, 4]) pan_panel(tilt_shaft);
}
//
//  This part attaches to the pan part and is used to tilt up and down.
//
module tilt(tilt_shaft)
{
  difference()
  {
    union()
    {
      gear(5, 50, 4, 1);
      translate([20, -16, 4]) rotate([0, -90, 0]) bbs_fillet(10, 40);
      translate([-20, -20, 4]) rotate([0, -90, 180]) bbs_fillet(10, 40);
    }
    union()
    {
      translate([0, 0, -0.5]) cylinder(r=tilt_shaft, h=5);
      for(t=[0:60:180])
      {
        rotate([0, 0, t]) translate([23, 0, -0.5]) cylinder(r=8, h=5);
      }
    }
  }
  difference()
  {
    translate([-20, -20, 4]) cube([20, 4, 100]);
    union()
    {
      for(z=[25:20:100])
      {
        translate([-10, -20.5, z]) rotate([-90, 0, 0]) cylinder(r=screw_8_size()/2, h=5);
      }
    }
  }
}
//
//  Gear for the pan stepper motor
//
module pan_gear()
{
  gear(5, 20, 4, 1);
}
//
//  Gear for the tilt stepper motor
//
//  This gear has 20 teath, the tilt gear has 50 teeth.  This is a 2:5 gear ratio.  The stepper motor
//  is 400 steps per revolution, so with the gearing it should take 1000 steps to completely rotate
//  the tilt frame.  This is 0.36 degrees per step or 2.78 steps per degree.
//
module tilt_gear()
{
  difference()
  {
    gear(5, 20, 4, 1);
    translate([0, 0, -0.5]) bbs_NEMA17_dshaft(5);
  }
}

//base();
pan(screw_8_size()/2, screw_8_size()/2);
//translate([0, -54, 104]) rotate([-90, 0, 0]) color("green") tilt(screw_8_size()/2);
//translate([0, 54, 104]) rotate([-90, 0, 180]) color("red") tilt(screw_8_size()/2);
//translate([0, -50, 104-pitch_radius(5, 50)-pitch_radius(5, 20)]) rotate([90, 7, 0])tilt_gear();
//translate([0, -54, 110]) cube([10, 108, 10]);

//tilt(screw_8_size()/2);
//translate([60, 0, 0]) tilt_gear();