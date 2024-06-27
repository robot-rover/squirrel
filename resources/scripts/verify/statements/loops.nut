::print("2.4.3 Loops\n")
::print("2.4.3.1 For Loop\n")
::print(" - For loop with inline local index\n")
for(local a=0;a<5;a+=1)
    print(a+" ");
::print("\n")
::print(" - For loop with env index\n")
glob <- null
for(glob=0;glob<5;glob+=1){
    print(glob+" ");
}
::print("\n")
//or
::print(" - Infinite For loop with break\n")
local counter = 0
for(;;){
    if (counter > 5) break
    print(counter+" ")
    counter++
}
::print("\n")

::print("2.4.3.2 Foreach Loop\n")
::print(" - Foreach loop over array with index\n")
local a=[10,23,33,41,589,56]
foreach(idx,val in a)
    print("index="+idx+" value="+val+"\n");
//or
::print(" - Foreach loop over array\n")
foreach(val in a)
    print("value="+val+"\n");

// Foreach over object has no definite order
// To make the outputs match, need to sort after iteration
::print(" - Foreach loop over object\n")
local obj = {a=10, b="b", c=4.001e-1}
local obj_list = []
foreach(val in obj)
    obj_list.push("value="+val+"\n");
obj_list.sort()
foreach(val in obj_list)
    print(val);
obj_list.clear()

::print(" - Foreach loop over object with key\n")
this.obj <- {}
for (local i=10;i>=0;i-=1)
    this.obj[i + " KEY"] <- i.tostring() + " VAL"

foreach(key,val in this.obj)
    obj_list.push("key="+key+" value="+val+"\n");
obj_list.sort()

foreach(val in obj_list)
    print(val);