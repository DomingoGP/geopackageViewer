{
ISC License
Copyright (c) 2021 Domingo Galmés

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND ISC DISCLAIMS ALL WARRANTIES WITH REGARD TO
THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL ISC BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
}
unit geopackagedrawerproj;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, geopackagedrawer, proj, BGRABitmap, BGRABitmapTypes;

type

  { TGeoPackageDrawerProj }

  TGeoPackageDrawerProj = class(TGeoPackageDrawer)
  protected
    P: PPJ;
  public
    Scale:double;
    Origin_Shift:double;
    constructor Create(ABitmap: TBGRABitmap; AZoomLevel: integer;
      aLatLeftTop: double; aLonLeftTop: double;aProjectTo:string='EPSG:3857');
    function CoordToPixelXY(latitude, longitude: double; var pixelX: integer;
      var pixelY: integer): boolean; override;
    function PixelXYToCoord(pixelX, pixelY: integer;
      var latitude: double; var longitude: double):boolean;override;
    procedure SetupProjection; virtual; overload;
    procedure SetupProjection(const aTo: string); virtual; overload;
    destructor Destroy; override;
    procedure DrawTiles(aGeoPackage: TGeoPackage; const aTableName: string;
      aOpacity: byte = 255); override;
    function CoordToMeters(latitude, longitude: double;
      out metersX: double; out metersY: double): boolean;
    procedure SetScale;
    function IsProjValid:boolean;override;
  end;


procedure ProjInit(const aProjPath:string);
procedure ProjFinalize;

implementation

uses
  Math, SQLiteWrap;

const
  EARTH_EQUATORIAL_RADIUS:double = 6378137;

constructor TGeoPackageDrawerProj.Create(ABitmap: TBGRABitmap;
  AZoomLevel: integer; aLatLeftTop: double; aLonLeftTop: double;aProjectTo:string='EPSG:3857');
begin
  inherited Create(ABitmap, AZoomLevel, aLatLeftTop, aLonLeftTop);
  FGridTileSize := 16;
  SetLatBounds(-90,90);
  SetLonBounds(-180,180);
  SetupProjection(aProjectTo);
end;

function TGeoPackageDrawerProj.CoordToMeters(latitude, longitude: double;
  out metersX: double; out metersY: double): boolean;
var
  c_in, c_out: PJ_COORD;
begin
  Result := True;
  if P = nil then
    Exit(false)
  else
  begin
    c_in.lpzt.lam := Clip(longitude, MinLon, MaxLon);
    c_in.lpzt.phi := Clip(-latitude, MinLat, MaxLat);   // we want Y origen at top not at bottom.
    c_in.lpzt.z := 0.0;
    c_in.lpzt.t := HUGE_VAL;
    c_out := proj_trans(P, PJ_FWD, c_in);
    if IsInfinite(c_out.xy.x) or IsInfinite(c_out.xy.y) or IsNan(c_out.xy.x) or IsNan(c_out.xy.y) then
      Exit(False);
    metersX := c_out.xy.x;
    metersY := c_out.xy.y;
  end;
end;

procedure TGeoPackageDrawerProj.SetScale;
var
  wLenMeters:double;
begin
  wLenMeters:= 2 * PI * EARTH_EQUATORIAL_RADIUS;
  Scale:=MapSizeInPixels(FZoomLevel) / wLenMeters;
  Origin_Shift:= wLenMeters * 0.5 * Scale;
end;

function TGeoPackageDrawerProj.IsProjValid: boolean;
begin
  Result:= P<>nil;
end;

function TGeoPackageDrawerProj.CoordToPixelXY(latitude, longitude: double;
  var pixelX: integer; var pixelY: integer): boolean;
var
  c_in, c_out: PJ_COORD;

  function CheckPoint(aV:double;var aOut:integer):boolean;
  begin
    result:=true;
    if IsInfinite(aV) or IsNan(aV) then
      Exit(false)
    else if aV>MaxInt then
      Exit(false)
    else if aV<-MaxInt then
      Exit(false);
    aOut := Trunc(aV);
  end;
begin
  Result := True;
  if P = nil then
    inherited
  else
  begin
    c_in.lpzt.lam := Clip(longitude, MinLon, MaxLon);
    c_in.lpzt.phi := Clip(-latitude, MinLat, MaxLat);   // we want Y origen at top not at bottom.
    c_in.lpzt.z := 0.0;
    c_in.lpzt.t := HUGE_VAL;
    c_out := proj_trans(P, PJ_FWD, c_in);
    if IsInfinite(c_out.xy.x) or IsInfinite(c_out.xy.y) or IsNan(c_out.xy.x) or IsNan(c_out.xy.y) then
      Exit(False);
    if CheckPoint(Origin_Shift + c_out.xy.x * Scale, PixelX)=false then
      Exit(false);
    if CheckPoint(Origin_Shift + c_out.xy.y * Scale, PixelY)=false then
      Exit(false);
  end;
end;

function TGeoPackageDrawerProj.PixelXYToCoord(pixelX, pixelY: integer;
  var latitude: double; var longitude: double):boolean;
var
  c_in, c_out: PJ_COORD;
  mx, my: double;
begin
  result:=false;
  latitude:=infinity;
  longitude:=infinity;
  if P = nil then
  begin
    result:=inherited PixelXYToCoord(pixelX, pixelY, latitude, longitude);
    exit;
  end;
  try
    if not IsZero(Scale) then
    begin
      mx:=(PixelX-Origin_Shift) / Scale;
      my:=(pixelY-Origin_Shift) / Scale;
      if IsNan(mx) or IsInfinite(mx) then exit;
      if IsNan(my) or IsInfinite(my) then exit;
      c_in.xy.x := mx;
      c_in.xy.y := my;
      c_in.lpzt.z := 0.0;
      c_in.lpzt.t := HUGE_VAL;
      c_out := proj_trans(P, PJ_INV, c_in);
      if (not IsNan(c_out.lpzt.phi)) and (not IsInfinite(c_out.lpzt.phi)) then
        latitude := -c_out.lpzt.phi;
      if (not IsNan(c_out.lpzt.lam)) and (not IsInfinite(c_out.lpzt.lam)) then
        longitude := c_out.lpzt.lam;
      result:=true;
    end;
  except
  end;
end;

//default projection google maps tiles.
procedure TGeoPackageDrawerProj.SetupProjection;
begin
  SetupProjection('EPSG:3857');
end;

// parse a value in the projection string  +proj=xxx +lon_0=12.5
function ParseValueProj(const aLabel:string;const aStr:string):double;
var
  wP:integer;
  wL:integer;
  wNumber:string;
begin
  result:=0;
  wP:=Pos(aLabel,aStr);
  if wP>0 then
  begin
    wL:=Length(aStr);
    wP:=wP+5;
    while (wP<=wL) and (aStr[wP]=' ') do Inc(wP);
    if (wP<=wL) and (aStr[wP]='=') then
    begin
      Inc(wP);
      while (wP<=wL) and (aStr[wP]=' ') do Inc(wP);
      wNumber:='';
      while (wP<=wL) and (Pos(aStr[wP],'+-.0123456789')>0) do
      begin
       wNumber:=wNumber + aStr[wP];
       Inc(wP);
      end;
      Val(wNumber,result,wL);
    end;
  end;
end;

procedure TGeoPackageDrawerProj.SetupProjection(const aTo: string);
var
  P_for_GIS: PPJ;
  wPX,wPY:integer;
begin
  P_for_GIS := nil;
  if P <> nil then
    proj_destroy(P);
  P := proj_create_crs_to_crs(PJ_DEFAULT_CTX, pansichar('EPSG:4326'),
    pansichar(aTo), nil);
  if P<>nil then
    P_for_GIS := proj_normalize_for_visualization(PJ_DEFAULT_CTX, P);
  if P <> nil then
    proj_destroy(P);
  P := P_for_GIS;
  SetScale;
  inherited CoordToPixelXY(FLatOrigin, FLonOrigin, FXOrigin, FYOrigin);
  inherited PixelXYToCoord(FXOrigin + FBitmap.Width, FYOrigin + FBitmap.Height, FLatEnd, FLonEnd);
  FFalseMeridian:=ParseValueProj('lon_0',aTo);
  FFalseEquator:=ParseValueProj('lat_0',aTo);
  FClipPoligons:=true;
  if Pos('ortho',LowerCase(aTo))>0 then
  begin
    CoordToPixelXY(-FFalseEquator,FFalseMeridian,wPX,wPY);
    PixelXYToPixelBitmapNoClip(wPX, wPY, FCenter);
    FRadio:=Scale * EARTH_EQUATORIAL_RADIUS;
    FFalseMeridian:=0;
    FFalseEquator:=0;
    FClipPoligons:=true; //false;
    //all globe.
    FLatEnd:=-90;
    FLonEnd:=180;
  end;
end;

destructor TGeoPackageDrawerProj.Destroy;
begin
  if P <> nil then
    proj_destroy(P);
  inherited Destroy;
end;

procedure ProjInit(const aProjPath:string);
var
  Paths: array of pansichar;
begin
  SetLength(Paths, 1);
  Paths[0] := pansichar(aProjPath);
  {
  It is IMPORTANT to put the path to the proj data, otherwise it gives a file not found error.
  You also can set the PROJ_LIB var from Operating system.
  SET PROJ_LIB=C:\OSGeo4W\share\proj
  }
  proj_context_set_search_paths(PJ_DEFAULT_CTX, 1, @Paths[0]);
end;

procedure ProjFinalize;
begin
  proj_cleanup;
end;

procedure TGeoPackageDrawerProj.DrawTiles(aGeoPackage: TGeoPackage;
  const aTableName: string; aOpacity: byte);
var
  wTable: TSQLiteTable;
  wQueryString: string;
  wBM: TBGRABitmap;
  wTexture: TBGRABitmap;
  wMemStream: TMemoryStream;
  wTileWidth, wTileHeight: integer;
  wMatrixW,wMatrixH:integer;
  wOffsetX, wOffsetY: integer;
  wTileX, wTileY: integer;
  wRow, wCol: integer;
  wNumRows, wNumCols: integer;
  wTemp: integer;
  //wPixelXSize, wPixelYSize: double;
  wGMXOrigin, wGMYOrigin: integer;
  wLat00, wLat01, wLat10, wLat11: double;
  wLon00, wLon01, wLon10, wLon11: double;
  wXP00, wXP01, wXP10, wXP11: integer;
  wYP00, wYP01, wYP10, wYP11: integer;
  wPt00, wPt01, wPt10, wPt11: TPoint;

  wStartX, wEndX, wStartY, wEndY: integer;
  wOk: boolean;

  function AdjustTo180(aLon:double):double;
  var
    wA:double;
  begin
    wA:=10-FZoomLevel;
    if FZoomLevel>4 then
      wA:=0.5;
    result:=aLon;
    if result > (180 - wA) then
      result:=180;
  end;

  function AdjustLatTo90(aLat:double):double;
  begin
    result:=aLat;
    if aLat>=85.051 {MaxLatitude} then
      result:=90;
    if aLat<=-85.051 {MinLatitude} then
      result:=-90;
  end;

begin
  if (aGeoPackage = nil) or (aGeoPackage.Database = nil) then
    Exit;

{
   idea, unir los tiles en un bitmap temporal del tamaño deseado (sin reproyectar) y
   luego hacer una malla de triangulos pequeños y proyectarlos en el bitmap final.

   NO funciona bien con proyeccion esferica si el mapa no es suficientemente grande
   para contener el mapa completo.

   Para que vaya bien el bitmap debe tener sufiente tamaño para que quepa la proyeccion
   completa (todo el mundo) basicamente el doble de ancho que el bitmap de destino

   TODO:
     usar como textura Bitmap de 2x2 tiles y cargarlos a medida que sean necesarios.

}


  // find tiles size
  wQueryString :=
    'select matrix_width,matrix_height,tile_width,tile_height,pixel_x_size,pixel_y_size from gpkg_tile_matrix where table_name='''
    + aTableName + ''' and zoom_level=:zl ';
  aGeoPackage.Database.ParamsClear;
  aGeoPackage.Database.AddParamInt(':zl', FZoomLevel);
  wTable := aGeoPackage.Database.GetTable(wQueryString);
  try
    if wTable.EOF then
      Exit;
    wTileWidth := wTable.FieldAsInteger(2);
    wTileHeight := wTable.FieldAsInteger(3);
    wMatrixW:=wTable.FieldAsInteger(0);
    wMatrixH:=wTable.FieldAsInteger(1);
    //wPixelXSize := wTable.FieldAsDouble(4);
    //wPixelYSize := wTable.FieldAsDouble(5);
  finally
    wTable.Free;
  end;
  // find top left tile and offsets
  // origin in tiles projection. (google maps)
  inherited CoordToPixelXY(FLatOrigin, FLonOrigin, wGMXOrigin, wGMYOrigin);
  wOffsetX := -(wGMXOrigin mod wTileWidth);
  wOffsetY := -(wGMYOrigin mod wTileHeight);
  wTileX := wGMXOrigin div wTileWidth;
  wTileY := {wMatrixH -} (wGMYOrigin div wTileHeight);
  // fill bitmap.
  wQueryString := 'select tile_data from ' + aTableName +
    ' where zoom_level=:zl and tile_column=:tc and tile_row=:tr ';
  wBM := nil;
  wTexture := nil;
  try
    wTexture := TBGRABitmap.Create(FBitmap.Width * 2, FBitmap.Height + wTileHeight, BGRA(0, 0, 0, 0));
    wNumRows := ((wTexture.Height - wOffsetY) div wTileHeight);
    wNumCols := ((wTexture.Width - wOffsetX) div wTileWidth);
    wBM := TBGRABitmap.Create;
    try
      for wRow := 0 to wNumRows do
      begin
        for wCol := 0 to wNumCols do
        begin
          aGeoPackage.Database.ParamsClear;
          aGeoPackage.Database.AddParamInt(':zl', FZoomLevel);
          wTemp := wTileX + wCol; // remove compiler hint
          aGeoPackage.Database.AddParamInt(':tc', wTemp);
          wTemp := wTileY + wRow;   // remove compiler hint
          aGeoPackage.Database.AddParamInt(':tr', wTemp);
          wTable := aGeoPackage.Database.GetTable(wQueryString);
          wMemStream := nil;
          try
            if not wTable.EOF then
            begin
              wMemStream := wTable.FieldAsBlob(0);
              wBM.LoadFromStream(wMemStream);
              wTexture.PutImage(wOffsetX + wCol * wTileHeight, wOffsetY +
                wRow * wTileWidth, wBM, dmSet, aOpacity);

              //            FBitmap.FillPolyLinearMapping( [PointF(110,10), PointF(250,10), PointF(350,160), PointF(10,160)], wBM,
              //             [PointF(0,0), PointF(wBM.width-1,0), PointF(wBM.Width-1,wBM.Height-1), PointF(0,wBM.Height-1)], true);
            end;
          finally
            wMemStream.Free;
            wTable.Free;
          end;
        end;
      end;
    finally
      wBM.Free;
    end;
    // map texture to map using grid.
    wStartY := -wOffsetY;
    wMatrixH:=wMatrixH * wTileHeight;
    wMatrixW:=wMatrixW * wTileWidth;
    while (wStartY <= (wTexture.Height - 1)) and (wStartY<=wMatrixH) do
    begin
      wEndY := wStartY + FGridTileSize;
      if wEndY > (wTexture.Height - 1) then
        wEndY := wTexture.Height - 1;

      wStartX := -wOffsetX;
      while (wStartX <= (wTexture.Width - 1)) and (wStartX<=wMatrixW) do
      begin
        wEndX := wStartX + FGridTileSize;
        if wEndX > (wTexture.Width - 1) then
          wEndX := wTexture.Width - 1;
        //proyectamos los 4 puntos.
        inherited PixelXYToCoord(wGMXOrigin + wStartX, wGMYOrigin + wStartY, wLat00, wLon00);
        inherited PixelXYToCoord(wGMXOrigin + wEndX, wGMYOrigin + wStartY, wLat01, wLon01);
        inherited PixelXYToCoord(wGMXOrigin + wStartX, wGMYOrigin + wEndY, wLat10, wLon10);
        inherited PixelXYToCoord(wGMXOrigin + wEndX, wGMYOrigin + wEndY, wLat11, wLon11);

        //HACK evita dejar linea abierta al final en proyeccion esferica.
        wLon00:=AdjustTo180(wLon00);
        wLon01:=AdjustTo180(wLon01);
        wLon10:=AdjustTo180(wLon10);
        wLon11:=AdjustTo180(wLon11);

        if FRadio>0 then     // ortho
        begin
          wLat00:=AdjustLatTo90(wLat00);
          wLat01:=AdjustLatTo90(wLat01);
          wLat10:=AdjustLatTo90(wLat10);
          wLat11:=AdjustLatTo90(wLat11);
        end;

        //los reproyectamos con la nueva proyeccion
        wOk := True;
        wOk := wOk and CoordToPixelXY(wLat00, wLon00, wXP00, wYP00);
        wOk := wOk and CoordToPixelXY(wLat01, wLon01, wXP01, wYP01);
        wOk := wOk and CoordToPixelXY(wLat10, wLon10, wXP10, wYP10);
        wOk := wOk and CoordToPixelXY(wLat11, wLon11, wXP11, wYP11);
        if wOk then
        begin
          PixelXYToPixelBitmapNoClip(wXP00, wYP00, wPt00);
          PixelXYToPixelBitmapNoClip(wXP01, wYP01, wPt01);
          PixelXYToPixelBitmapNoClip(wXP10, wYP10, wPt10);
          PixelXYToPixelBitmapNoClip(wXP11, wYP11, wPt11);
          //          FBitmap.FillPolyLinearMapping( [PointF(wStartX,wStartY), PointF(wEndX,wStartY), PointF(wEndX,wEndY), PointF(wStartX,wEndY)], wTexture,
          //             [PointF(wStartX,wStartY), PointF(wEndX,wStartY), PointF(wEndX,wEndY), PointF(wStartX,wEndY)], true);
          FBitmap.FillPolyLinearMapping(
            [PointF(wPt00.X, wPt00.Y), PointF(wPt01.X, wPt01.Y), PointF(wPt11.X, wPt11.Y),
            PointF(wPt10.X, wPt10.Y)], wTexture,
            [PointF(wStartX, wStartY), PointF(wEndX, wStartY),
            PointF(wEndX, wEndY), PointF(wStartX, wEndY)], True);
        end;
        Inc(wStartX, FGridTileSize);
      end;

      Inc(wStartY, FGridTileSize);
    end;
    //    FBitmap.PutImage(wOffsetX, wOffsetY, wTexture, dmLinearBlend, aOpacity);
  finally
    wTexture.Free;
  end;
end;

end.
