# Introduction

EPANET is an open source cross-platform software package for modeling water distribution systems. It simulates the hydraulic and water quality behavior of a pressurized pipe network over an extended period of time.

<imgl images/DistributionSystem.png>

A network can consist of pipes, nodes (pipe junctions), pumps, valves and storage tanks or reservoirs. EPANET tracks the flow rate of water in each pipe, the pressure at each node, the height of water in each tank, and the concentration of chemical species throughout the network during a multi-time period simulation. In addition to chemical species, water age and source tracing can also be simulated.  

Version 2.3 introduces a completely new user interface that: 
- offers a modern and simple design
- runs on Windows, Linux and MacOS
- has full support for EPANET's multi-species water quality (MSX) extension
- can import data from GIS shapefiles and DXF CAD files
- can use web mapping services to provide background basemaps
- includes additional reporting options for simulation results. 

# Program Overview

## EPANET's Data Model

EPANET models a pipe network as a collection of links connected to nodes. The links represent pipes, pumps, and control valves. The nodes represent junctions, tanks, and reservoirs. The figure below illustrates how these objects can be connected to one another to form a network.

<imgc images/network.png>

Junctions have a user-supplied water withdrawal rate (i.e., consumer demand) associated with them. Tanks are storage units whose water level changes over time. Reservoirs are boundary points where a fixed hydraulic head applies.

Pipes have a length, diameter, roughness coefficient, and possible leakage area. Pumps have either a constant power rating or a pump curve that determines the head they add as a function of flow rate. Valves are used to regulate either flow or pressure. Controls can be applied to completely open or close a link or to adjust its setting (pump speed or valve setting).

In addition to these physical objects a network model can also contain the following data objects:
- time patterns that allow demands, quality source strength and pump speed settings to vary at fixed intervals of time
- data curves that describe relationships between two quantities, such as head versus flow for pumps and volume versus water level for tanks
- simple controls that adjust a link's setting (such as a pump's status) based on node pressure, tank level, elapsed time, ot time of day
- rule-based controls that consist of one or more premises that if true result in one set of actions being taken and if false result in a different set of actions being taken
- water quality sources that introduce a chemical constituent into the network at specified nodes.

The model also contains a number of analysis options that specify:
- the project's flow units which in turn determines its unit system (US or SI)
- the formula used to compute head loss
- whether to use a demand driven or a pressure driven analysis
- hydraulic convergence criteria
- time steps used for hydraulic, water quality and reporting
- the type of water quality analysis to perform (chemical reaction, source tracing, water age, or multiple species)
- global values for chemical reaction coefficients that can be overridden for individual pipes
- global values for energy usage parameters that can be overridden for individual pumps.

## EPANET's Workflow

One typically carries out the following steps when using EPANET to analyze a water distribution system:
1. Create a new project or open an existing one (see [Working with Projects]).
2. For a new project set its default properties (see [Setting Project Defaults]).
3. Select an optional basemap layer to display your pipe network on (see [Using a Basemap]).
4. If available, import network information residing in GIS or CAD files (see [Importing Data]).
5. Add network objects to the map and edit their properties (see [Working with Objects]).
6. Specify a set of analysis options and run a simulation (see [Running a Simulation]).
7. View the simulation results (see [Viewing Simulation Results]).
