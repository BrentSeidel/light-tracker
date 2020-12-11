use <../../Things/bbs_constants.scad>

module LOCAL_led(height)
{
  lead_space = 2.54;
  translate([0, lead_space/2, -0.5]) cube([1, 1, height]);
  translate([0, -lead_space/2, -0.5]) cube([1, 1, height]);
  translate([-20, lead_space/2, -0.5]) cube([41, 1, height/2]);
  translate([-20, -lead_space/2, -0.5]) cube([41, 1, height/2]);
}
module phototransitor_mount()
{
  difference()
  {
    union()
    {
      cube([40, 80, 4]);
      translate([0, 39, 0]) cube([40, 2, 40]);
      translate([19, 20, 0]) cube([2, 40, 40]);
    }
    union()
    {
      translate([10, 10, -0.5]) cylinder(r=screw_8_size()/2, h=5);
      translate([30, 10, -0.5]) cylinder(r=screw_8_size()/2, h=5);
      translate([10, 70, -0.5]) cylinder(r=screw_8_size()/2, h=5);
      translate([30, 70, -0.5]) cylinder(r=screw_8_size()/2, h=5);
      translate([14, 34, 0]) LOCAL_led(5);
      translate([14, 46, 0]) LOCAL_led(5);
      translate([26, 34, 0]) LOCAL_led(5);
      translate([26, 46, 0]) LOCAL_led(5);
    }
  }
}

rotate([0, 0, 90]) phototransitor_mount();
