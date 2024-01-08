union() {
    sphere(r = 1.5e1);
    cube([1.0e1, 1.5e1, 2.0e1]);
    translate([0.0e0, 0.0e0, 2.5e1]) union() {
        cylinder(r1 = 0.0e0, r2 = 2.0e1, 2.0e1);
        cube([1.0e1, 1.0e1, 1.0e1]);
    }
    linear_extrude(height = 7.0e0) translate([2.0e1, 0.0e0]) difference() {
        circle([1.0e1]);
        circle([1.0e1]);
    }
}