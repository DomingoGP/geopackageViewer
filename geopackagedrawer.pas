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
unit geopackagedrawer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, contnrs, SysUtils, BGRABitmap, BGRABitmapTypes, geopackagewkb, Graphics
  , SQLiteWrap, polylabelu;

const
  MinLatitude = -85.05112878;
  MaxLatitude = 85.05112878;
  MinLongitude = -180;
  MaxLongitude = 180;


type
  GeoPackageException=Exception;
  { TPointList }

  TDrawInterval = (di0 = 0, di5 = 5, di10 = 10, di15 = 15, di30 = 30,
    di45 = 45, di90 = 90);

  TPointList = class
    Points: array of TPointF;
    Capacity: integer;
    Count: integer;
    procedure AddPointF(aX: integer; aY: integer); overload;
    procedure AddPointF(aPointF: TPointF); overload;
    procedure AddEmptyPoint;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

  TGPLabel = class
    Caption: string;
    Priority: integer;
    FontColor: TColor;
    FontHeight: integer;
    X: integer;
    Y: integer;
  end;

  RFeatureProperties=record
    GeometryFieldName:string;
    SrsId:integer;
    SrsName:string;
  end;


  { TGeoPackage }

  TGeoPackage = class
  protected
    FDataBase: TSQLiteDatabase;
  public
    constructor Create; overload;
    constructor Create(const aFileName: string); overload;
    destructor Destroy; override;
    function Open(const aFileName: string): boolean;
    procedure Close;
    procedure GetFeatureTables(aTables: TStrings);
    procedure GetFeatureTableFields(const aTableName: string; aFields: TStrings);
    property Database: TSQLiteDatabase read FDatabase;
    function GetFeatureProperties(const aTableName: string; out aFeatureProperties: RFeatureProperties): boolean;
    function GetSrsName(aId: integer): string;
  end;

  { TGeoPackageDrawer }

  TGeoPackageDrawer = class
  protected
    FDrawShapes: boolean;
    FOnlyDrawPoints: boolean;
    FLabelsSorted: boolean;
    FXOrigin: integer;
    FYOrigin: integer;
    FLatOrigin: double;
    FLonOrigin: double;
    FLatEnd: double;
    FLonEnd: double;
    FZoomLevel: integer;
    FBitmap: TBGRABitmap;
    FPointsList: TPointList;
    FLabelsList: TFPObjectList;
    wkbReader: TWkbReader;
    FGH: wkbGeometryHeader;
    FFillColor: TBGRAPixel;
    FBorderColor: TBGRAPixel;
    FLineWidth: single;
    FPointRadius: single;
    FFontHeigth: integer;
    FNumPointsFirstPoligon: integer;
    FLabelPos: TPoleOfInaccessibility;
    FHasLabel: boolean;
    FFalseMeridian: double;
    FFalseEquator:double;
    FClipPoligons:boolean;
    //<size of rectangles used for reprject. Smaller is more precise but slower.
    FGridTileSize: integer;
    procedure ReadPoint(const aGH: wkbGeometryHeader);
    procedure ReadMultiPoint(const aGH: wkbGeometryHeader);
    procedure ReadMultiPoligono(const aGH: wkbGeometryHeader);
    procedure ReadPoligono(const aGH: wkbGeometryHeader);
    procedure ReadLineString(const aGH: wkbGeometryHeader);
    procedure ReadMultiLineString(const aGH: wkbGeometryHeader);
    procedure PixelXYToPixelBitmapNoClip(pixelX, pixelY: integer;
      out punto: TPoint);
    function GetMinEnvelope: wkPointZM;
    function GetMaxEnvelope: wkPointZM;
    procedure DrawPoints(aPoints:array of TPointF; aColor:TBGRAPixel);
  public
    FRadio:double;
    FCenter:TPoint;
    MinLat,MaxLat:double;
    MinLon,MaxLon:double;
    ErrorString:string;
    //HIGH LEVEL procedures.
    constructor Create(ABitmap: TBGRABitmap; AZoomLevel: integer;
      aLatLeftTop: double; aLonLeftTop: double); overload;
    destructor Destroy; override;
    procedure DrawFeatures(aGeoPackage: TGeoPackage; const aTableName: string;
      const aFieldLabel: string = ''; const aWhereFilter: string = '');
    procedure DrawTiles(aGeoPackage: TGeoPackage; const aTableName: string;
      aOpacity: byte = 255); virtual;
    procedure DrawLabels; overload;
    procedure DrawGrid(aColor: TBGRAPixel; aInterval: TDrawInterval = di15;
      aLineWidth: single = 1);
    //drawing blue background for oceans
    procedure DrawFilledGrid(aColor: TBGRAPixel;aFillColor:TBGRAPixel;
      aInterval: TDrawInterval = di15; aLineWidth: single = 1);
    //LOW LEVEL procedures.
    procedure Draw(APtrWellKnowBinary: pbyte; ASize: integer; aClip: boolean = True);
    //AddLabel??? should be called after Draw.
    procedure AddLabel(const aCaption: string; aPriority: integer = 0;
      aFontHeigth: integer = 14; aFontColor: TColor = clBlack); overload;
    procedure AddLabel(aLat: double; aLon: double; const aCaption: string;
      aPriority: integer = 0; aFontHeigth: integer = 14;
      aFontColor: TColor = clBlack); overload;
    procedure AddLabelXY(aX: single; aY: single; const aCaption: string;
      aPriority: integer = 0; aFontHeigth: integer = 14; aFontColor: TColor = clBlack);
    procedure SortLabels;
    procedure ClearLabels;
    procedure DrawLabel(aLabel: TGPLabel);
    procedure DrawLabels(aFromPriority: integer; aToPriority: integer); overload;
    function CoordToPixelXY(latitude, longitude: double; var pixelX: integer;
      var pixelY: integer): boolean; virtual;
    function PixelXYToCoord(pixelX, pixelY: integer;
      var latitude: double; var longitude: double):boolean;virtual;
    procedure SetLatBounds(aMinLat:double;aMaxLat:double);
    procedure SetLonBounds(aMinLon:double;aMaxLon:double);
    function IsProjValid:boolean;virtual;
    property FillColor: TBGRAPixel read FFillColor write FFillColor;
    property BorderColor: TBGRAPixel read FBorderColor write FBorderColor;
    property MinEnvelope: wkPointZM read GetMinEnvelope;
    property MaxEnvelope: wkPointZM read GetMaxEnvelope;
    property LatEnd: double read FLatEnd;
    property LonEnd: double read FLonEnd;
  published
    property ZoomLevel: integer read FZoomLevel;
    property LatOrigin: double read FLatOrigin;
    property LonOrigin: double read FLonOrigin;

    property XOrigin: integer read FXOrigin;
    property YOrigin: integer read FYOrigin;

    property LineWidth: single read FLineWidth write FLineWidth;
    property PointRadius: single read FPointRadius write FPointRadius;
    property FontHeigth: integer read FFontHeigth write FFontHeigth;

    property DrawShapes: boolean read FDrawShapes write FDrawShapes;
    property FalseMeridian: double read FFalseMeridian write FFalseMeridian;
    property FalseEquator:double read FFalseEquator write FFalseEquator;
    property OnlyDrawPoints:boolean read FOnlyDrawPoints write FOnlyDrawPoints;
    property GridTileSize: integer read FGridTileSize write FGridTileSize;
  end;


procedure LatLongToPixelXY(latitude, longitude: double; ZoomLevel: integer;
  out pixelX: integer; out pixelY: integer);
procedure PixelXYToLatLong(pixelX, pixelY, ZoomLevel: integer;
  out latitude: double; out longitude: double);
function Clip(n, MinValue, MaxValue: double): double;
function MapSizeInPixels(ZoomLevel: integer): longword;

implementation

uses
  Math;


function Clip(n, MinValue, MaxValue: double): double;
begin
  Result := Min(Max(n, MinValue), MaxValue);
end;

{
Returns the map width and height (in pixels) at a specified zoom level.
@param(ZoomLevel, from 1 to 23.)
@return(The map width and height in pixels.)
}
function MapSizeInPixels(ZoomLevel: integer): longword;
begin
  Result := 256 shl ZoomLevel;
end;

{
Converts a point from latitude/longitude WGS-84 coordinates (in degrees)
into pixel XY coordinates at a specified zoom level.
The latitude and longitude on the WGS 84 datum.
The longitude range from -180 to +180 degrees
Latitude range from -85.05112878 to 85.05112878.
ZoomLevel range from 1 to 23
}
procedure LatLongToPixelXY(latitude, longitude: double; ZoomLevel: integer;
  out pixelX: integer; out pixelY: integer);
var
  wX, sinLatitude, wY: double;
  wMapSize: longword;
begin
  longitude := Clip(longitude, MinLongitude, MaxLongitude);
  latitude := Clip(latitude, MinLatitude, MaxLatitude);
  wX := (longitude + 180) / 360;
  sinLatitude := Sin(latitude * PI / 180);
  wY := 0.5 - Ln((1 + sinLatitude) / (1 - sinLatitude)) / (4 * PI);

  wMapSize := MapSizeInPixels(ZoomLevel);
  pixelX := trunc(Clip(wX * wMapSize + 0.5, 0, wMapSize - 1));
  pixelY := trunc(Clip(wY * wMapSize + 0.5, 0, wMapSize - 1));
end;


procedure PixelXYToLatLong(pixelX, pixelY, ZoomLevel: integer;
  out latitude: double; out longitude: double);
var
  wMapSize, wX, wY: double;
begin
  wMapSize := MapSizeInPixels(ZoomLevel);
  wX := (Clip(pixelX, 0, wMapSize - 1) / wMapSize) - 0.5;
  wY := 0.5 - (Clip(pixelY, 0, wMapSize - 1) / wMapSize);

  latitude := 90 - 360 * ArcTan(Exp(-wY * 2 * PI)) / PI;
  longitude := 360 * wX;
end;


{ TGeoPackage }

constructor TGeoPackage.Create;
begin
  inherited;
  FDatabase := nil;
end;

constructor TGeoPackage.Create(const aFileName: string);
begin
  inherited Create;
  FDatabase := nil;
  Open(aFileName);
end;

destructor TGeoPackage.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TGeoPackage.Open(const aFileName: string): boolean;
begin
  Close;
  if not FileExists(aFileName) then
  begin
    //ShowMessage('File ' + aFileName + ' not exists');
    Exit(False);
  end;
  FDataBase := TSQLiteDatabase.Create(aFileName);
  Result := True;
end;

procedure TGeoPackage.Close;
begin
  if FDatabase <> nil then
    FDatabase.Free;
  FDatabase := nil;
end;

procedure TGeoPackage.GetFeatureTables(aTables: TStrings);
begin
  aTables.Clear;
  if FDatabase = nil then
    Exit;
  FDatabase.GetTableStrings('select table_name from gpkg_contents', aTables);
end;

procedure TGeoPackage.GetFeatureTableFields(const aTableName: string;
  aFields: TStrings);
var
  fSltb: TSQLiteTable;
begin
  aFields.Clear;
  if FDatabase = nil then
    Exit;
  fSltb := FDatabase.GetTable('PRAGMA table_info(''' + aTableName + ''')');
  try
    while fSltb.EOF = False do
    begin
      aFields.Add(fSltb.FieldAsString(1));
      fSltb.Next;
    end;
  finally
    fSltb.Free;
  end;
end;

function TGeoPackage.GetSrsName(aId:integer):string;
var
  wTable: TSQLiteTable;
begin
  result:='';
  if FDatabase = nil then
    Exit;
  Database.ParamsClear;
  Database.AddParamInt(':id', aId);
  wTable := Database.GetTable('select organization,organization_coordsys_id from gpkg_spatial_ref_sys where srs_id=:id');
  try
    if wTable.EOF = False then
      result:=Trim(wTable.FieldAsString(0))+':'+Trim(wTable.FieldAsString(1));
  finally
    wTable.Free;
  end;
end;

function TGeoPackage.GetFeatureProperties(const aTableName:string;out aFeatureProperties:RFeatureProperties):boolean;
var
  wTable: TSQLiteTable;
begin
  result:=false;
  aFeatureProperties.GeometryFieldName:='';
  aFeatureProperties.SrsId:=0;
  aFeatureProperties.SrsName:='';
  if FDatabase = nil then
    Exit;

  Database.ParamsClear;
  Database.AddParamText(':table_name', aTableName);
  wTable := Database.GetTable('select column_name,srs_id from gpkg_geometry_columns where table_name=:table_name');
  try
    if wTable.EOF = False then
    begin
      aFeatureProperties.GeometryFieldName := wTable.FieldAsString(0);
      aFeatureProperties.SrsId:=wTable.FieldAsInteger(1);
      aFeatureProperties.SrsName:=GetSrsName(aFeatureProperties.SrsId);
      result:=true;
    end;
  finally
    wTable.Free;
  end;
end;

procedure TPointList.AddPointF(aX: integer; aY: integer); overload;
begin
  if Count >= Capacity then
  begin
    Capacity := Capacity * 2;
    SetLength(Points, Capacity);
  end;
  Points[Count].X := aX;
  Points[Count].Y := aY;
  Inc(Count);
end;

procedure TPointList.AddPointF(aPointF: TPointF); overload;
begin
  if Count >= Capacity then
  begin
    Capacity := Capacity * 2;
    SetLength(Points, Capacity);
  end;
  Points[Count] := aPointF;
  Inc(Count);
end;

procedure TPointList.AddEmptyPoint;
begin
  if Count >= Capacity then
  begin
    Capacity := Capacity * 2;
    SetLength(Points, Capacity);
  end;
  Points[Count].X := EmptySingle;
  Points[Count].Y := EmptySingle;
  Inc(Count);
end;

constructor TPointList.Create;
begin
  SetLength(Points, 200);
  Capacity := 200;
  Count := 0;
end;

destructor TPointList.Destroy;
begin
  if Points <> nil then
    SetLength(Points, 0);
  inherited Destroy;
end;

procedure TPointList.Clear;
begin
  Count := 0;
end;


constructor TGeoPackageDrawer.Create(ABitmap: TBGRABitmap; AZoomLevel: integer;
  aLatLeftTop: double; aLonLeftTop: double);
begin
  inherited Create;
  FClipPoligons:=true;
  FPointsList := TPointList.Create;
  FLabelsList := TFPObjectList.Create(True);
  FBitmap := ABitmap;
  FLineWidth := 1;
  FFillColor := BGRA(0, $FF, 0);
  FBorderColor := BGRA(0, 0, $FF);
  FFontHeigth := 16;
  FDrawShapes := True;
  FOnlyDrawPoints := False;
  FZoomLevel := aZoomLevel;
  FLatOrigin := aLatLeftTop;
  FLonOrigin := aLonLeftTop;
  CoordToPixelXY(aLatLeftTop, aLonLeftTop, FXOrigin, FYOrigin);
  PixelXYToCoord(FXOrigin + FBitmap.Width, FYOrigin + FBitmap.Height, FLatEnd, FLonEnd);
end;

destructor TGeoPackageDrawer.Destroy;
begin
  FPointsList.Free;
  FLabelsList.Free;
  inherited;
end;

function IsReal(const d: single): boolean; overload;
begin
  result:=(Uint32(d) and $7f800000)<>$7f800000;
end;

function IsReal(const d: double): boolean; overload;
begin
  result:=(UInt64(d) and $7f80000000000000)<>$7f80000000000000;
end;


// signaling nan
//  7f800001 to  7fbfffff    or  ff800001   ffbfffff
// quiet nan
//  7fc00001 to  7fffffff    or  ffc00001   ffffffff

function IsNanDGP(const d : Single): Boolean; overload;
begin
  result:= not (d=d);
//    result:=(Uint32(d) and $7fffffff)>$7f000000;
end;

function IsNanDGP(const d : Double): Boolean;overload;
begin
  result:= not (d=d);
  //result:=(Uint64(d) and $7fffffffffffffff)>$7f00000000000000;
end;

procedure TGeoPackageDrawer.DrawPoints(aPoints:array of TPointF; aColor:TBGRAPixel);
var
  wI:integer;

  function PP(aV:single):integer;
  begin
    result:=MaxInt;
    if IsNan(aV) or IsInfinite(aV)  then
      exit;
    if aV>MaxInt then
      exit;
    if aV<-MaxInt then
      Exit(-MaxInt);
    result:=Trunc(aV);
  end;

begin
  for wI := 0 to High(aPoints) do
  begin
    FBitmap.SetPixel(PP(aPoints[wI].x),PP(aPoints[wI].y),aColor);
  end;
end;

procedure TGeoPackageDrawer.ReadMultiPoligono(const aGH: wkbGeometryHeader);
var
  wP: integer;
  wGH: wkbGeometryHeader;
begin
  for wP := 1 to aGH.number do
  begin
    wGH := wkbReader.ReadGeometryHeader;
    ReadPoligono(wGH);
  end;
end;

procedure TGeoPackageDrawer.ReadPoligono(const aGH: wkbGeometryHeader);
var
  wPuntos: integer;
  wPunto: wkPoint;
  wPX, wPY: integer;
  wPoint: TPoint;
  wkbReaderClone: TWkbReader;
  wClipLon0, wClipLon1: boolean;
  wClipLat0, wClipLat1: boolean;
  wAntiMeridian0: double;
  wAntiMeridian1: double;
  wAntiMeridian: double;

  wAntiEquator0, wAntiEquator1, wAntiEquator: double;

  procedure DoPolygon(aClipMinLon: double; aClipMaxLon: double;
    aClipMinLat: double; aClipMaxLat: double);
  var
    wRings: uint32;
    wZ: integer;
    wPosStart:integer;
  begin
    FPointsList.Clear;
    for wRings := 1 to aGH.number do
    begin
      wPuntos := wkbReader.ReadUInt32(aGH.isBigEndian);
      wPosStart:=FPointsList.Count;
      for wZ := 1 to wPuntos do
      begin
        wkbReader.ReadPoint(wPunto);
        wPunto.X := Clip(wPunto.X, aClipMinLon, aClipMaxLon);
        wPunto.Y := Clip(wPunto.Y, aClipMinLat, aClipMaxLat);
        if CoordToPixelXY(wPunto.Y, wPunto.X, wPX, wPY) then
        begin
          PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
          FPointsList.AddPointF(wPoint.x, wPoint.Y);
        end;
      end;
      // Force Closed Poly
      if FPointsList.Count>wPosStart then
      begin
        if (wPoint.x<>FPointsList.Points[wPosStart].x) or (wPoint.y<>FPointsList.Points[wPosStart].y) then
        begin
          FPointsList.AddPointF(FPointsList.Points[wPosStart]);
        end;
      end;


      // only for geometry of one polygon.
      if (FHasLabel = True) and (FNumPointsFirstPoligon = -2) then
      begin
        FNumPointsFirstPoligon := FPointsList.Count;
        //calc label pos
        FLabelPos := polylabel(slice(FPointsList.Points, FNumPointsFirstPoligon), 1);
      end;
      if wRings < aGH.number then
        FPointsList.AddEmptyPoint;
    end;
  end;

  procedure DrawPoly;
  begin
    if OnlyDrawPoints then
    begin
      DrawPoints(slice(FPointsList.Points, FPointsList.Count),FBorderColor);
      Exit;
    end;
    if FDrawShapes then
    begin
      FBitmap.DrawPolygonAntialias(slice(FPointsList.Points, FPointsList.Count),
        FBorderColor, FLineWidth, {BGRA(0,255,0)} FFillColor);
    end;
  end;

begin

  wAntiEquator0 := FFalseEquator - 90;
  wAntiEquator1 := FFalseEquator + 90;
  wkbReaderClone := wkbReader;
  wAntiMeridian0 := FFalseMeridian - 180;
  wAntiMeridian1 := FFalseMeridian + 180;
  DoPolygon(-9999999, 9999999, -9999999, 9999999);
  //DEBUG: without clipping.
  if FClipPoligons=false then
  begin
    DrawPoly;
    Exit;
  end;
  // clipping.
  wClipLon0 := (wkbReader.MinEnvelope.x < wAntiMeridian0) and
    (wkbReader.MaxEnvelope.x >= wAntiMeridian0);
  if wClipLon0 then
    wAntiMeridian := wAntimeridian0;
  wClipLon1 := (wkbReader.MinEnvelope.x < wAntiMeridian1) and
    (wkbReader.MaxEnvelope.x >= wAntiMeridian1);
  if wClipLon1 then
    wAntiMeridian := wAntimeridian1;

  wClipLat0 := (wkbReader.MinEnvelope.y < wAntiEquator0) and
    (wkbReader.MaxEnvelope.y >= wAntiEquator0);
  if wClipLat0 then
    wAntiEquator := wAntiEquator0;
  wClipLat1 := (wkbReader.MinEnvelope.y < wAntiEquator1) and
    (wkbReader.MaxEnvelope.y >= wAntiEquator1);
  if wClipLat1 then
    wAntiEquator := wAntiEquator1;

  if wClipLon0 or wClipLon1 then
  begin
    // poligon cross antimeridian
    // so we draw two times clipped.

    if wClipLat0 or wClipLat1 then
    begin
      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(wAntiMeridian + 0.001, 9999999, wAntiEquator + 0.001, 9999999);
      DrawPoly;
      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(wAntiMeridian + 0.001, 9999999, -9999999, wAntiEquator - 0.001);
      DrawPoly;

      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(-9999999, wAntiMeridian - 0.001, wAntiEquator + 0.001, 9999999);
      DrawPoly;

      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(-9999999, wAntiMeridian - 0.001, -9999999, wAntiEquator - 0.001);

    end
    else
    begin

      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(wAntiMeridian + 0.001, 9999999, -9999999, 9999999);
      DrawPoly;
      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(-9999999, wAntiMeridian - 0.001, -9999999, 9999999);
    end;
  end
  else
  begin
    if wClipLat0 or wClipLat1 then
    begin
      // poligon cross antiequator
      // so we draw two times clipped.
      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(-9999999, 9999999, wAntiEquator + 0.001, 9999999);
      DrawPoly;
      wkbReader := wkbReaderClone;   //reset data
      DoPolygon(-9999999, 9999999, -9999999, wAntiEquator - 0.001);
    end;
  end;
  DrawPoly;
end;

procedure TGeoPackageDrawer.ReadLineString(const aGH: wkbGeometryHeader);
var
  wZ: integer;
  wPuntos: integer;
  wPunto: wkPoint;
  wPX, wPY: integer;
  wPoint: TPoint;
  wFrom: TPoint;
begin
  //  changed. don't draw parts no visibles in esferical projections.
  //  FPointsList.Clear;
  wFrom.x := -99999;
  wPuntos := aGH.number;
  for wZ := 1 to wPuntos do
  begin
    wkbReader.ReadPoint(wPunto);
    if CoordToPixelXY(wPunto.Y, wPunto.X, wPX, wPY) then
    begin
      PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
      //      FPointsList.AddPointF(wPoint.x, wPoint.Y);
      if OnlyDrawPoints then
      begin
        FBitmap.SetPixel(Trunc(wPoint.X),Trunc(wPoint.Y),FBorderColor);
      end
      else
      if FDrawShapes then
      begin
        if wFrom.x <> -99999 then
          FBitmap.DrawLineAntialias(wFrom.X, wFrom.Y, wPoint.X, wPoint.Y,
            FBorderColor, FlineWidth);
      end;
      wFrom := wPoint;
    end
    else
      wFrom.x := -99999;
  end;
  //if FDrawShapes then
  //begin
  //  FBitmap.DrawPolyLineAntialias(slice(FPointsList.Points, FPointsList.Count),
  //    FBorderColor, FLineWidth);
  //end;
end;

procedure TGeoPackageDrawer.DrawGrid(aColor: TBGRAPixel;
  aInterval: TDrawInterval = di15; aLineWidth: single = 1);
var
  wPX, wPY: integer;
  wPoint: TPoint;
  wLat, wLon: integer;
  wInc: integer;
  wIncDraw: integer;
  wFrom: TPoint;
begin
  if not FDrawShapes then
    Exit;
  if aInterval = di0 then
    Exit;
  wInc := integer(aInterval);
  wIncDraw := 5;
  // draw meridians.
  wLon := -180;
  while wLon <= 180 do
  begin
    wFrom.x := -99999;
    wLat := 90;
    while wLat >= -85 do
    begin
      if CoordToPixelXY(wLat, wLon, wPX, wPY) then
      begin
        PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
        if wFrom.x <> -99999 then
          FBitmap.DrawLineAntialias(wFrom.X, wFrom.Y, wPoint.X, wPoint.Y, aColor, aLineWidth);
        wFrom := wPoint;
      end
      else
        wFrom.x := -99999;
      wLat := wLat - wIncDraw;
    end;
    wLon := wLon + wInc;
  end;
  // draw parallels.
  wLat := 90;
  while wLat >= -90 do
  begin
    wFrom.x := -99999;
    wLon := -180;
    while wLon <= 180 do
    begin
      if CoordToPixelXY(wLat, wLon, wPX, wPY) then
      begin
        PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
        if wFrom.x <> -99999 then
          FBitmap.DrawLineAntialias(wFrom.X, wFrom.Y, wPoint.X, wPoint.Y, aColor, aLineWidth);
        wFrom := wPoint;
      end
      else
        wFrom.x := -99999;
      wLon := wLon + wIncDraw;
    end;
    wLat := wLat - wInc;
  end;
end;


procedure TGeoPackageDrawer.DrawFilledGrid(aColor: TBGRAPixel;aFillColor:TBGRAPixel;
  aInterval: TDrawInterval = di15; aLineWidth: single = 1);
var
  wPX, wPY: integer;
  wPoint: TPoint;
  wLat, wLon: integer;
  wInc: integer;
  wP0,wP1,wP2,wP3:TPointF;
  wP0b,wP1b,wP2b,wP3b:boolean;
  wPoints:array[0..5] of TPointF;
  wNumPoints:integer;
begin
//  if not FDrawShapes then
//    Exit;
  if aInterval = di0 then
    Exit;
  wInc := integer(aInterval);
  // draw parallels.
  wLat := 90;
  while wLat >= -(90-wInc) do
  begin
    wLon := -180;
    wP0b:=CoordToPixelXY(wLat, wLon, wPX, wPY);
    if wP0b then
    begin
      PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
      wP0.X:=wPoint.X;
      wP0.Y:=wPoint.Y;
    end;
    wP1b:=CoordToPixelXY(wLat - wInc, wLon, wPX, wPY);
    if wP1b then
    begin
      PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
      wP1.X:=wPoint.X;
      wP1.Y:=wPoint.Y;
    end;

    while wLon <= (180-wInc) do
    begin
      wNumPoints:=0;
      wP2b:=CoordToPixelXY(wLat, wLon+wInc, wPX, wPY);
      if wP2b then
      begin
        PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
        wP2.X:=wPoint.X;
        wP2.Y:=wPoint.Y;
      end;
      wP3b:=CoordToPixelXY(wLat-wInc, wLon + wInc, wPX, wPY);
      if wP3b then
      begin
        PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
        wP3.X:=wPoint.X;
        wP3.Y:=wPoint.Y;
      end;
      if wP0b then
      begin
        wPoints[wNumPoints]:=wP0;
        inc(wNumPoints);
      end;
      if wP2b then
      begin
        wPoints[wNumPoints]:=wP2;
        inc(wNumPoints);
      end;
      if wP3b then
      begin
        wPoints[wNumPoints]:=wP3;
        inc(wNumPoints);
      end;
      if wP1b then
      begin
        wPoints[wNumPoints]:=wP1;
        inc(wNumPoints);
      end;
      if wNumPoints>=2 then
      begin
        wPoints[wNumPoints]:=wPoints[0]; // close polygon.
        inc(wNumPoints);
       FBitmap.DrawPolygonAntialias(slice(wPoints, wNumPoints),
        aColor, aLineWidth, {BGRA(0,255,0)} aFillColor);
      end;
      wP0b:=wP2b;
      wP1b:=wP3b;
      wP0:=wP2;
      wP1:=wP3;
      wLon := wLon + wInc;
    end;
    wLat := wLat - wInc;
  end;
end;


procedure TGeoPackageDrawer.ReadPoint(const aGH: wkbGeometryHeader);
var
  wPunto: wkPoint;
  wPX, wPY: integer;
  wPoint: TPoint;
begin
  wkbReader.ReadPoint(wPunto);
  if CoordToPixelXY(wPunto.Y, wPunto.X, wPX, wPY) then
  begin
    PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
    if FDrawShapes then
    begin
      if FPointRadius < 1 then
        FBitmap.SetPixel(wPoint.x, wPoint.y, FBorderColor)
      else
        FBitmap.EllipseAntialias(wPoint.x, wPoint.y, FPointRadius, FPointRadius,
          FBorderColor, FLineWidth, FFillColor);
    end;
  end;
end;

procedure TGeoPackageDrawer.ReadMultiPoint(const aGH: wkbGeometryHeader);
var
  wZ: integer;
  wPuntos: integer;
  wPunto: wkPoint;
  wPX, wPY: integer;
  wPoint: TPoint;
begin
  wPuntos := aGH.number;
  for wZ := 1 to wPuntos do
  begin
    wkbReader.ReadPoint(wPunto);
    if CoordToPixelXY(wPunto.Y, wPunto.X, wPX, wPY) then
    begin
      PixelXYToPixelBitmapNoClip(wPX, wPY, wPoint);
      if FDrawShapes then
      begin
        if FPointRadius < 1 then
          FBitmap.SetPixel(wPoint.x, wPoint.y, FBorderColor)
        else
          FBitmap.EllipseAntialias(wPoint.x, wPoint.y, FPointRadius,
            FPointRadius, FBorderColor, FLineWidth, FFillColor);
      end;
    end;
  end;
end;

procedure TGeoPackageDrawer.ReadMultiLineString(const aGH: wkbGeometryHeader);
var
  wRings: integer;
  wGH: wkbGeometryHeader;
begin
  for wRings := 1 to aGH.number do
  begin
    wGH := wkbReader.ReadGeometryHeader;
    ReadLineString(wGH);
  end;
end;

procedure TGeoPackageDrawer.SetLatBounds(aMinLat: double; aMaxLat: double);
begin
  MinLat:=aMinLat;
  MaxLat:=aMaxLat;
end;

procedure TGeoPackageDrawer.SetLonBounds(aMinLon: double; aMaxLon: double);
begin
  MinLon:=aMinLon;
  MaxLon:=aMaxLon;
end;

function TGeoPackageDrawer.IsProjValid: boolean;
begin
  Result:=true;
end;

procedure TGeoPackageDrawer.PixelXYToPixelBitmapNoClip(pixelX, pixelY: integer;
  out punto: TPoint);
begin
  punto.X := pixelX - FXOrigin;
  punto.Y := pixelY - FYOrigin;
end;

function TGeoPackageDrawer.GetMinEnvelope: wkPointZM;
begin
  Result := wkbReader.MinEnvelope;
end;

function TGeoPackageDrawer.GetMaxEnvelope: wkPointZM;
begin
  Result := wkbReader.MaxEnvelope;
end;

procedure TGeoPackageDrawer.Draw(APtrWellKnowBinary: pbyte; ASize: integer;
  aClip: boolean);
begin
  FNumPointsFirstPoligon := -1;
  wkbReader.Init(APtrWellKnowBinary, ASize);
  FGH := wkbReader.ReadGeometryHeader;
  if (wkbReader.EnvelopeType <> wkbEnvelopeTypeNone) and aClip then
  begin
    if wkbReader.MinEnvelope.x > FLonEnd then Exit;
    if wkbReader.MaxEnvelope.x < FLonOrigin then Exit;
    if wkbReader.MinEnvelope.y > FLatOrigin then Exit;
    if wkbReader.MaxEnvelope.Y < FLatEnd then Exit;
  end;
  case FGH.geometryType of
    wkbMultiPolygon: ReadMultiPoligono(FGH);
    wkbPolygon:
    begin
      FNumPointsFirstPoligon := -2;
      ReadPoligono(FGH);
    end;
    wkbPoint: ReadPoint(FGH);
    wkbMultiPoint: ReadMultiPoint(FGH);
    wkbMultiLineString: ReadMultiLineString(FGH);
    wkbLineString: ReadLineString(FGH);
    else
    //not supported yet.
  end;
end;

procedure TGeoPackageDrawer.AddLabel(const aCaption: string;
  aPriority: integer; aFontHeigth: integer; aFontColor: TColor);
begin
  if FNumPointsFirstPoligon > 0 then
  begin
    AddLabelXY(FLabelPos.PlotX, FLabelPos.PlotY, aCaption, aPriority,
      FFontHeigth, aFontColor);
  end
  else
  begin
    AddLabel((MinEnvelope.y + MaxEnvelope.y) / 2,
      (MinEnvelope.x + MaxEnvelope.x) / 2,
      aCaption, aPriority, FFontHeigth, aFontColor);
  end;
end;

procedure TGeoPackageDrawer.AddLabel(aLat: double; aLon: double;
  const aCaption: string; aPriority: integer; aFontHeigth: integer; aFontColor: TColor);
var
  wL: TGPLabel;
  wX, wY: integer;
  wPoint: TPoint;
begin
  FLabelsSorted := True;
  wL := TGPLabel.Create;
  if CoordToPixelXY(ALat, ALon, wX, wY) then
  begin
    PixelXYToPixelBitmapNoClip(wX, wY, wPoint);
    wL.X := wPoint.X;
    wL.Y := wPoint.Y;
    wL.Caption := aCaption;
    wl.FontColor := aFontColor;
    wL.FontHeight := aFontHeigth;
    wL.Priority := aPriority;
    FLabelsList.Add(TObject(wL));
  end;
end;

procedure TGeoPackageDrawer.AddLabelXY(aX: single; aY: single;
  const aCaption: string; aPriority: integer; aFontHeigth: integer; aFontColor: TColor);
var
  wL: TGPLabel;
begin
  FLabelsSorted := True;
  wL := TGPLabel.Create;
  wL.X := Trunc(aX);
  wL.Y := Trunc(aY);
  wL.Caption := aCaption;
  wl.FontColor := aFontColor;
  wL.FontHeight := aFontHeigth;
  wL.Priority := aPriority;
  FLabelsList.Add(TObject(wL));
end;

function SLCompare(Item1: Pointer; Item2: Pointer): integer;
begin
  Result := TGPLabel(Item1).Priority - TGPLabel(Item2).Priority;
end;

procedure TGeoPackageDrawer.SortLabels;
begin
  FLabelsSorted := True;
  FLabelsList.Sort(@SLCompare);
end;

procedure TGeoPackageDrawer.ClearLabels;
begin
  FLabelsSorted := True;
  FLabelsList.Clear;
end;

procedure TGeoPackageDrawer.DrawLabel(aLabel: TGPLabel);
begin
  //FBitmap.FontHeight:=ALabel.FontHeight;//+1;
  //FBitmap.FontStyle:=[fsBold];
  //FBitmap.TextOut(aLabel.X,aLabel.Y,aLabel.Caption,clWhite,taCenter,True);
  //FBitmap.FontHeight:=-ALabel.FontHeight;
  FBitmap.FontFullHeight := ALabel.FontHeight;
  //FBitmap.FontStyle:=[];
  FBitmap.TextOut(aLabel.X, aLabel.Y - ALabel.FontHeight div 2, aLabel.Caption, aLabel.FontColor, taCenter, True);
end;

procedure TGeoPackageDrawer.DrawLabels(aFromPriority: integer; aToPriority: integer);
var
  wI: integer;
  wL: TGPLabel;
begin
  if not FLabelsSorted then
    SortLabels;
  wI := 0;
  while wI < FLabelsList.Count do
  begin
    wL := TGPLabel(FLabelsList[wI]);
    if wL.Priority > aToPriority then
      Exit;
    if wL.Priority >= aFromPriority then
      DrawLabel(wL);
    Inc(wI);
  end;
end;

procedure TGeoPackageDrawer.DrawLabels;
begin
  DrawLabels(Low(integer), High(integer));
end;

procedure TGeoPackageDrawer.DrawTiles(aGeoPackage: TGeoPackage;
  const aTableName: string; aOpacity: byte);
var
  wTable: TSQLiteTable;
  wQueryString: string;
  wBM: TBGRABitmap;
  wMemStream: TMemoryStream;
  wTileWidth, wTileHeight: integer;
  //wMatrixW,wMatrixH:integer;
  wOffsetX, wOffsetY: integer;
  wTileX, wTileY: integer;
  wRow, wCol: integer;
  wNumRows, wNumCols: integer;
  wTemp: integer;
begin
  if (aGeoPackage = nil) or (aGeoPackage.Database = nil) then
    Exit;
  // find tiles size
  wQueryString :=
    'select matrix_width,matrix_height,tile_width,tile_height from gpkg_tile_matrix where table_name='''
    + aTableName + ''' and zoom_level=:zl ';
  aGeoPackage.Database.ParamsClear;
  aGeoPackage.Database.AddParamInt(':zl', FZoomLevel);
  wTable := aGeoPackage.Database.GetTable(wQueryString);
  try
    if wTable.EOF then
      Exit;
    wTIleWidth := wTable.FieldAsInteger(2);
    wTIleHeight := wTable.FieldAsInteger(3);
    //wMatrixW:=wTable.FieldAsInteger(0);
    //wMatrixH:=wTable.FieldAsInteger(1);
  finally
    wTable.Free;
  end;
  // find top left tile and offsets
  wOffsetX := -(FXOrigin mod wTileWidth);
  wOffsetY := -(FYOrigin mod wTileHeight);
  wTileX := FXOrigin div wTileWidth;
  wTileY := {wMatrixH -} (FYOrigin div wTileHeight);
  // fill bitmap.
  wQueryString := 'select tile_data from ' + aTableName +
    ' where zoom_level=:zl and tile_column=:tc and tile_row=:tr ';
  wNumRows := ((FBitmap.Height - wOffsetY) div wTileHeight);// - 1;
  wNumCols := ((FBitmap.Width - wOffsetX) div wTileWidth);// - 1;
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
            FBitmap.PutImage(wOffsetX + wCol * wTileHeight, wOffsetY +
              wRow * wTileWidth, wBM, {dmSet} dmLinearBlend, aOpacity);
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
end;

procedure TGeoPackageDrawer.DrawFeatures(aGeoPackage: TGeoPackage;
  const aTableName: string; const aFieldLabel: string; const aWhereFilter: string);
var
  wTable: TSQLiteTable;
  wPtr: pbyte;
  wSize: integer;
  wQueryString: string;
  wFilter: string;
  wFP:RFeatureProperties;
begin
  if (aGeoPackage = nil) or (aGeoPackage.Database = nil) then
    Exit;
  FHasLabel := False;
  if aGeoPackage.GetFeatureProperties(aTableName,wFP) then
    wQueryString := 'select ' + wFP.GeometryFieldName
  else
    wQueryString := 'select geom';
  if wFP.SrsName<>'EPSG:4326' then
  begin
    //not implemented
    raise GeoPackageException.Create('Not supported srs: '+wFP.SrsName);
    Exit;
  end;

  if aFieldLabel <> '' then
  begin
    wQueryString := wQueryString + ',' + aFieldLabel;
    FHasLabel := True;
  end;
  wQueryString := wQueryString + ' from ' + aTableName;
  wFilter := Trim(aWhereFilter);
  if wFilter <> '' then
    wQueryString := wQueryString + ' where ' + wFilter;
  wTable := aGeoPackage.Database.GetTable(wQueryString);
  try
    while wTable.EOF = False do
    begin
      wPtr := wTable.FieldAsBlobPtr(0, wSize);
      Draw(wPtr, wSize);
      if FHasLabel then
        AddLabel(wTable.FieldAsString(1), 0, FFontHeigth);
      wTable.Next;
    end;
  finally
    wTable.Free;
  end;
end;

function TGeoPackageDrawer.CoordToPixelXY(latitude, longitude: double;
  var pixelX: integer; var pixelY: integer): boolean;
begin
  Result := True;
  LatLongToPixelXY(latitude, longitude, FZoomLevel, pixelX, pixelY);
end;

function TGeoPackageDrawer.PixelXYToCoord(pixelX, pixelY: integer;
  var latitude: double; var longitude: double):boolean;
begin
  result:=true;
  PixelXYToLatLong(pixelX, pixelY, FZoomLevel, latitude, longitude);
end;

end.
