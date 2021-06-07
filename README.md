                                                      

This project is a [Lazarus](https://www.lazarus-ide.org/) program for view [geopackage](https://www.geopackage.org/) files and drawing maps.

uses [BGRABITMAP package]( https://wiki.freepascal.org/BGRABitmap)



Tested on windows only.

in windows
requires sqlite3.dll in the program folder and in the lazarus.exe folder.
by default maps are drawed using the web mercator projection. EPSG:3857
requires library [proj8](https://proj.org/install.html) installed in the computer.
allow display maps in other projections.

may require changing the path in the
procedure ProjInit; in the geopackagedrawerproj unit.
and the dll name in proj.pas unit
 
if you comment in frmMain.pas then only depends on sqlite3.dll

{$DEFINE USE_PROJ7}


Homepage: [www.github.com/DomingoGP/geopackageViewer](https://github.com/DomingoGP/geopackageViewer)


screenshoots

![screenshoot1 docked](https://github.com/DomingoGP/lazIdeMiniMap/blob/master/screenshots/scs1.png)

![screenshoot2 undocked](https://github.com/DomingoGP/lazIdeMiniMap/blob/master/screenshots/scs2.png)

sites for download free geopackages

[www.naturalearthdata.com](https://www.naturalearthdata.com/downloads/)

[BlueMarble tiles](https://geoint.nrlssc.navy.mil/static/geopackages/NRL_BlueMarble-3395_GLOBAL_0-6_v1-0_18OCT2017.gpkg)

[gadm.org](https://gadm.org/index.html)

