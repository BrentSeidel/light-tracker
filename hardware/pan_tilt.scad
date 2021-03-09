//
//  Models for pan and tilt mounting
//
use <../../Things/bbs_constants.scad>
use <../../Things/bbs_shapes.scad>
use <../../Things/bbs_stepper.scad>
use <../../pd-gears/pd-gears.scad>

module LOCAL_base_arm()
{
  d1 = pitch_radius(5, 100);
  d2 = pitch_radius(5, 20);
  width=20;
  length=d1+d2+0;
  difference()
  {
    union()
    {
      translate([-width/2, 0, 0]) cube([width, length, 4]);
      translate([0, length, 0]) cylinder(r=10, h=4);
      translate([-width/2, d1+5, 0]) cube([width, 4, 20]);
    }
    union()
    {
      translate([0, length, -0.5]) cylinder(r=screw_8_size()/2, h=5);
      translate([0, length-20, -0.5]) cylinder(r=screw_8_size()/2, h=5);
      translate([0, length, 2]) cylinder(r=screw_8_size()/2+2.5, h=5);
      translate([0, length-20, 2]) cylinder(r=screw_8_size()/2+2.5, h=5);
      translate([0, d1+4.5, 15]) rotate([-90, 0, 0]) cylinder(r=2, h=5);
      translate([6, d1+4.5, 15]) rotate([-90, 0, 0]) cylinder(r=2, h=5);
      translate([-6, d1+4.5, 15]) rotate([-90, 0, 0]) cylinder(r=2, h=5);
    }
  }
}
module LOCAL_base_motor()
{
  d1 = pitch_radius(5, 100);
  d2 = pitch_radius(5, 20);
  difference()
  {
    rotate([0, 0, 0]) translate([-20, -15, 0]) cube([40, 135, 4]);
    union()
    {
      translate([0, (d1+d2), -0.5]) bbs_NEMA17_holes(5, true);
      translate([-0.5, 116, -0.5]) cube([1, 5, 1.5]);
    }
  }
}
//
//  This is the base that everything rests on.  it contains a hole for the vertical
//  shaft and a mount for a stepper motor.
//
module base(pan_shaft)
{
  difference()
  {
    union()
    {
      LOCAL_base_arm();
      rotate([0, 0, 90]) LOCAL_base_arm();
      rotate([0, 0, 180]) LOCAL_base_arm();
      rotate([0, 0, 270]) LOCAL_base_arm();
      rotate([0, 0, 45]) LOCAL_base_motor();
    }
    translate([0, 0, -0.5]) cylinder(r=pan_shaft, h=5);
  }
}
//
//  Feet to support the base.  These are bolted to the base and have holes where legs could
//  be attached to them.  They can also be used without legs to support the base on a table
//  top, or similar.
//
module foot()
{
  union()
  {
    difference()
    {
      cube([20, 40, 4]);
      union()
      {
        translate([10, 10, -0.5]) cylinder(r=screw_8_size()/2, h=5, $fn=12);
        translate([10, 30, -0.5]) cylinder(r=screw_8_size()/2, h=5, $fn=12);
      }
    }
    rotate([10, 0, 0]) difference()
    {
      cube([20, 4, 40]);
      union()
      {
        translate([10, -0.5, 10]) rotate([-90, 0, 0]) cylinder(r=screw_8_size()/2, h=5, $fn=12);
        translate([10, -0.5, 30]) rotate([-90, 0, 0]) cylinder(r=screw_8_size()/2, h=5, $fn=12);
      }
    }
    difference()
    {
      cube([4, 30, 20]);
      translate([-0.5, 0, 20]) rotate([-30, 0, 0]) cube([5, 40, 20]);
    }
  }
}
//
//  This part sits on the base.  It has an integrated gear that interfaces with with a gear
//  on the stepper motor.
//
module LOCAL_pan_panel(tilt_shaft)
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
      translate([4, 4.5, 12]) rotate([90, 0, 0]) cylinder(r=2, h=5);
      translate([4, 4.5, 18]) rotate([90, 0, 0]) cylinder(r=2, h=5);
      translate([46, 4.5, 12]) rotate([90, 0, 0]) cylinder(r=2, h=5);
      translate([46, 4.5, 18]) rotate([90, 0, 0]) cylinder(r=2, h=5);
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
  translate([-25, -59, 4]) LOCAL_pan_panel(tilt_shaft);
  translate([-25, 55, 4]) LOCAL_pan_panel(tilt_shaft);
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
//  Drive gears for the stepper motors
//
module drive_gear()
{
  difference()
  {
    gear(5, 20, 4, 1);
    translate([0, 0, -0.5]) bbs_NEMA17_dshaft(5);
  }
}

//rotate([0, -90, 0]) foot();
//base(screw_8_size()/2);
translate([0, 0, 5]) pan(screw_8_size()/2, screw_8_size()/2);
//translate([0, -54, 104]) rotate([-90, 0, 0]) color("green") tilt(screw_8_size()/2);
//translate([0, 54, 104]) rotate([-90, 0, 180]) color("red") tilt(screw_8_size()/2);
//translate([0, -50, 104-pitch_radius(5, 50)-pitch_radius(5, 20)]) rotate([90, 7, 0])tilt_gear();
//translate([0, -54, 110]) cube([10, 108, 10]);

//tilt(screw_8_size()/2);
//translate([60, 0, 0]) drive_gear();
