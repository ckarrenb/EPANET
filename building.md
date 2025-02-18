# Building the EPANET User Interface
This document contains instructions for building the EPANET user interface for both Windows and Linux on the x86_64 family of CPUs. Instructions for MacOS will be added at a later date.

## Build Tools
To build the EPANET UI you need the Lazarus Integrated Development Environment (version 3.2 or higher) and the Free Pascal Compiler (version 3.2.2). If these are not already installed on your machine we recommend using [FPCUPdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases) to install them.

## Required Packages
The following packages must be installed into your Lazarus setup to build the EPANET UI. They can be installed by selecting either the **Install/Uninstall Package** command or the **Online Package Manager** command from Lazarus' **Package** menu as indicated in the table below.

| Package | Version | Install Command |
| ------ | ------ | ------ |
| HtmlViewer | FrameViewer09 | Online Package Manager |
| LazMapViewer | 0.2.7.0 | Online Package Manager |
| lazmrumenu | 1.0 | Install/Uninstall Package |
| TAChartBgra | 1.0 | Install/Uninstall Package |
| TAChartLazarusPkg | 1.0 | Install/Uninstall Package |

## Required Libraries
The following third-party libraries are required to build the EPANET UI:
| Name | Version | Source Code Location | Windows Name | Linux Name |
| ---- | ------- | -------------------- | ---------------- | ---------------- |
| epanet | 2.3 (dev) | https://github.com/openwateranalytics/epanet | epanet2.dll | libepanet2.so |
| epanet-MSX | 2.0 | https://github.com/USEPA/EPANETMSX | epanetmsx.dll | libepanetmsx.so |
| ShapeLib | 1.5.0 | http://shapelib.maptools.org | shplib.dll | libshp.so |
| Proj4 | 4.3 | https://github.com/OrdnanceSurvey/proj.4 | proj.dll | libproj.so |

Binaries for each of these libraries have been placed in the *bin* folder of this repository.

## Build Instructions
1. Before opening the project in Lazarus, copy the *epanet-win64.lpi* file (for a Windows build) or the *epanet-linux.lpi* file (for a Linux build) to a file named ***epanet.lpi***.
2. For a Linux build, copy the four .so library files in the bin/linux folder to /user/local/lib.
3. Launch Lazarus and from the **File** menu open ***epanet.lpi***.
4. From the **Run** menu select the **Build** command.
5. For a Windows build, the newly created executable file ***epanet.exe*** will appear in the *bin/windows* folder while for a Linux build the executable ***epanet*** will appear in the *bin/linux* folder.
6. For a Linux build issue the following command in a terminal window so that the third-party libraries can be found:
`sudo ldconfig /usr/local/lib`
