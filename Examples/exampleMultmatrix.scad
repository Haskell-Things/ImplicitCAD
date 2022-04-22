// Examples from
// https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Transformations#multmatrix
// CC-BY-SA 3.0 https://creativecommons.org/licenses/by-sa/3.0/

angle=PI/4;
multmatrix(m = [ [cos(angle), -sin(angle), 0,  0],
                 [sin(angle),  cos(angle), 0, 30],
                 [         0,           0, 1,  0],
                 [         0,           0, 0,  1]
               ] )
union() {
   cylinder(r=10.0,h=10,center=false);
   cube(size=[10,10,10],center=false);
}

// skew
M = [ [ 1  , 0  , 0  , 0   ],
      [ 0  , 1  , 0.7, 0   ],  // The "0.7" is the skew value; pushed along the y axis as z changes.
      [ 0  , 0  , 1  , 0   ],
      [ 0  , 0  , 0  , 1   ] ] ;
translate([-20,0,0])
multmatrix(M) {  union() {
    cylinder(r=10.0,h=10,center=false);
    cube(size=[10,10,10],center=false);
} }

// same as previous example but using 3x4 matrix
N = [ [ 1  , 0  , 0  , 0   ],
      [ 0  , 1  , 0.7, 0   ],  // The "0.7" is the skew value; pushed along the y axis as z changes.
      [ 0  , 0  , 1  , 0   ] ] ;
translate([20,0,0])
multmatrix(N) {  union() {
    cylinder(r=10.0,h=10,center=false);
    cube(size=[10,10,10],center=false);
} }

