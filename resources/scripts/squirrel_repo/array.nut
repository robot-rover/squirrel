/*
*
* Original Javascript version by David Hedbor(http://www.bagley.org/~doug/shootout/)
*
*/
local n, i, k;

function print_array(name, a) {
  ::print(name + " = [")
  for (i = 0; i < a.len(); i+=1) {
    ::print(a[i].tostring());
    if (i < a.len()-1) {
      ::print(", ");
    }
  }
  ::print("]\n");
}

if(vargv.len()!=0) {
   n = vargv[0].tointeger();
  if(n < 1) n = 1;
} else {
  n = 1;
}

::print("n = "+n.tostring()+"\n");
local x = []; x.resize(n);
local y = []; y.resize(n);

for (i = 0; i < n; i+=1) {
  x[i] = i + 1;
  y[i] = 0;
}

print_array("x", x)
print_array("y", y)

::print((0 <= 1) + "\n")

for (k = 0 ; k < n; k+=1) {
  for (i = n-1; i >= 0; i-=1) {
    y[i] = y[i]+ x[i];
  }
}

print_array("x", x)
print_array("y", y)

print(y[0].tostring()+" "+y[n-1]);

