                                                      

This project is a [Lazarus](https://www.lazarus-ide.org/) program for view [geopackage](https://www.geopackage.org/) files and drawing maps.

uses [BGRABITMAP package]( https://wiki.freepascal.org/BGRABitmap)

Tested on windows only.

In windows
requires [sqlite3.dll](https://www.sqlite.org/index.html) in the program folder and in the lazarus.exe folder.
By default maps are drawed using the web mercator projection. EPSG:3857.

Requires library [proj8](https://proj.org/install.html) (versions 6 and 7 also works) installed in the computer and you can draw maps in other projections.

May require changing the path in the call of 
procedure ProjInit; in the FormCreate event of frmMain.pas unit.
and the dll name in proj.pas unit
 
if you comment {$DEFINE USE_PROJ7} in frmMain.pas then only depends on sqlite3.dll.


Homepage: [www.github.com/DomingoGP/geopackageViewer](https://github.com/DomingoGP/geopackageViewer)


Sites for download free geopackages

[www.naturalearthdata.com](https://www.naturalearthdata.com/downloads/)

[BlueMarble tiles](https://geoint.nrlssc.navy.mil/static/geopackages/NRL_BlueMarble-3395_GLOBAL_0-6_v1-0_18OCT2017.gpkg)

[gadm.org](https://gadm.org/index.html)


Sample code for drawing a map.
```
uses
   
   SQLiteWrap, geopackagewkb, geopackagedrawer, BGRABitmap, BGRABitmapTypes;

{
Data used in this test downloaded from.
https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_ESP_gpkg.zip
}

procedure TForm1.btnHighLevelClick(Sender: TObject);
var
  wGeoPackage: TGeoPackage;
  wDrawer: TGeoPackageDrawer;
  //BitmapMapa: TBGRABitmap;
begin
  //BitmapMapa:=TBGRABitmap.Create(1000,1000);
  BitmapMapa.Fill(clWhite);
  wDrawer := nil;
  wGeoPackage := nil;
  try
//wGeoPackage := TGeoPackage.Create('gadm36_ESP.gpkg');
wGeoPackage := TGeoPackage.Create(edGeoPackage.Text);
wDrawer := TGeoPackageDrawer.Create(BitmapMapa, 6, 43.80, -10.00);
wDrawer.DrawFeatures(wGeoPackage, 'gadm36_ESP_1', '', '');  // draw all records in the table.
wDrawer.FillColor := BGRA($255, 200, 200);
wDrawer.DrawFeatures(wGeoPackage, 'gadm36_ESP_1', 'NAME_1', 'fid=11 or fid=13'); //draw items 11 and 13 with label.
wDrawer.DrawLabels;//draw labels at end.
BitmapMapa.Draw(Canvas, 380, 10);
  finally
wGeoPackage.Free;
wDrawer.Free;
	//BitmapMapa.Free;
  end;
end;
```

screenshoots
[![Test-Project1.png](https://i.postimg.cc/Df1Wywvr/Test-Project1.png)](https://postimg.cc/t7CRByVJ)

[![Geo-Package-Viewer1.png](https://i.postimg.cc/7YYx8HfY/Geo-Package-Viewer1.png)](https://postimg.cc/qgSf2ds9)

[![Geo-Package-Viewer3.png](https://i.postimg.cc/50xrY0cX/Geo-Package-Viewer3.png)](https://postimg.cc/d7x6gJVF)

[![Geo-Package-Viewer2.png](https://i.postimg.cc/YStR8sdV/Geo-Package-Viewer2.png)](https://postimg.cc/v4S5Z3FL)

[![Geo-Package-Viewer.png](https://i.postimg.cc/y6Rf1c3L/Geo-Package-Viewer.png)](https://postimg.cc/z3JwpLjh)