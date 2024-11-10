# FP-2024 | Vehicle Garage

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

### To Execute a Lecture Example
1. `stack ghci`
2. `:l src/Lessons/Lesson01.hs`

### Lib2.hs BNF Changes
Added View command to BNF for a more extensive display of inventory, clearly defined what is a digit and what is a number.

### Batch Queries
From Lib3 it is possible to provide queries as a batch.
After loading the program, you can run it using `stack run fp2024-three`, then write the following commands:
```
>>> :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| add_vehicle(Car, "Mini", 2009, 240000 km);
| perform_maintenance(Car, OilChange, 2 days);
| inventory(Car);
| sell_vehicle(Car, "Mini", 2009, 5555.55);
| END
| 
Added Car Mini (2009)
Performed OilChange on Car for Days 2
Inventory for Car:
Mini (2009)

Sold Car Mini (2009) for $5555.55
```

This is used for application state saving and loading to/from file. They can be used like so:
```
>>> add_vehicle(Car, "Mini", 2009, 240000 km)
Added Car Mini (2009)
>>> add_vehicle(Car, "Audi", 2008, 320000 km)
Added Car Audi (2008)
>>> save
State saved successfully
```
Then we can inspect the 'state.txt' file, where we will see
```
BEGIN
add_vehicle(Car, "Audi", 2008, 320000 km);
add_vehicle(Car, "Mini", 2009, 240000 km);
END
```
these commands are similar to what we have using batch proccessing, we can modify the file and later load it in our program.
```
>>> view()
Vehicles:

Inventory:

>>> load
Added Car Audi (2008)
Added Car Mini (2009)
>>> view()
Vehicles:
Car Mini (2009)
Car Audi (2008)

Inventory:
2 Car
```