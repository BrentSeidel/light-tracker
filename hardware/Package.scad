use <../../Things/bbs_arduino.scad>
use <../../Things/bbs_boards.scad>
use <../../Things/bbs_breadboard.scad>
use <../../Things/bbs_connectors.scad>
use <../../Things/bbs_constants.scad>
use <../../Things/bbs_panel.scad>
use <../../Things/bbs_rack.scad>
use <../../Things/bbs_switches.scad>
use <../../Things/bbs_tray.scad>
//
//  This file contains items for mounting and housing the electronics to control the light-tracker.
//
module tray()
{
  screw_size = 3*screw_6_size()/4;
  screw_hole = screw_6_size()/2;
  difference()
  {
    union()
    {
      bbs_tray(5, 7, false);
      translate([150, 70, 0]) rotate([0, 0, 90]) bbs_tb6612_standoffs(5, screw_size, 12);
      translate([115, 70, 0]) rotate([0, 0, 90]) bbs_tb6612_standoffs(5, screw_size, 12);
      translate([0, 10, 0]) bbs_arduino_mega2560_standoffs(5, screw_size, 12);
      translate([110, 10, 0]) bbs_quarter_permaprotoboard_standoffs(5, screw_size, 12);
    }
    union()
    {
      translate([150, 70, -1]) rotate([0, 0, 90]) bbs_tb6612_standoffs(7, screw_hole, 12);
      translate([115, 70, -1]) rotate([0, 0, 90]) bbs_tb6612_standoffs(7, screw_hole, 12);
      translate([0, 10, -1]) bbs_arduino_mega2560_standoffs(7, screw_hole, 12);
      translate([110, 10, -1]) bbs_quarter_permaprotoboard_standoffs(7, screw_hole, 12);
      translate([25, 15, -1]) minkowski()
      {
        cube([30, 80, 7]);
        cylinder(r=5, h=7);
      }
      translate([125, 15, -1]) minkowski()
      {
        cube([10, 80, 7]);
        cylinder(r=5, h=7);
      }
      translate([75, 15, -1]) minkowski()
      {
        cube([5, 80, 7]);
        cylinder(r=5, h=7);
      }
    }
  }
}

module rack()
{
  bbs_rack(5, 7, 3);
}

module connector_panel()
{
  difference()
  {
    bbs_panel(5, 3);
    union()
    {
      translate([30, 40, -0.5]) rotate([0, 0, 90]) bbs_d_cutout(1, 3);
      translate([30, 80, -0.5]) rotate([0, 0, 90]) bbs_d_cutout(1, 3);
      translate([18, 40, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Stepper", halign="center", valign="center", size=6, font="Liberation Sans:Bold");
      translate([18, 80, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Sensors", halign="center", valign="center", size=6, font="Liberation Sans:Bold");
    }
  }
}

module switch_panel()
{
  difference()
  {
    bbs_panel(5, 2);
    union()
    {
      translate([20, 90, -0.1]) bbs_spdt_switch_cutout(2.2);
      translate([20, 70, -0.1]) bbs_spdt_switch_cutout(2.2);
      translate([20, 50, -0.1]) bbs_spdt_switch_cutout(2.2);
      translate([20, 30, -0.1]) bbs_spdt_switch_cutout(2.2);
      translate([7, 90, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Stop", halign="center", valign="center", size=6, font="Liberation Sans:Bold");
      translate([30, 90, 1.5]) linear_extrude(height = 0.6) rotate([0, 0, -90])
              text("Run", halign="center", valign="center", size=6, font="Liberation Sans:Bold");
    }
  }
}

//tray();
//translate([150, 70, 10]) rotate([0, 0, 90]) color("red") bbs_tb6612();
//translate([115, 70, 10]) rotate([0, 0, 90]) color("red") bbs_tb6612();
//translate([0, 10, 10]) color("green") bbs_arduino_mega2560();
//translate([110, 10, 10]) color("red") bbs_quarter_permaprotoboard();
//rack();
//connector_panel();
switch_panel();
