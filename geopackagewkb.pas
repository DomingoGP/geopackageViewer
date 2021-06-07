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


{
   Low level reader of data in "well-know-binary" from a BLOB field of a  geopackage.

   Example:

   var
     gpr:TWkbReader;
     gh:wkbGeometryHeader;
     gp:wkPoint;
     gnumber:uint32;
     grings:uint32;
     wi,wp:uint32;

   gpr.Init(buffer,len);
   if gpr.IsEmptyGeometry then
     Exit;
   gh:=ReadGeometryHeader;

   if gh.geometryType=wkbPoint then
   begin
      gpr.ReadPoint(gp);

      // process point.

   end;
   if gh.geometryType=wkbPolygon then
   begin
     grings:=gpr.ReadUint32(gh.IsBigEndian);
     for wi:=1 to grings do
     begin
       gnumber:=gpr.ReadUint32(gh.IsBigEndian);
       for wp:=1 to gnumber do
       begin
         gpr.ReadPoint(gp);

         // process point.

       end;
     end;
   end;

}
unit geopackagewkb;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils;

const
 wkbEnvelopeTypeNone    = 0;
 wkbEnvelopeTypeXY      = 1;
 wkbEnvelopeTypeXYZ     = 2;
 wkbEnvelopeTypeXYM     = 3;
 wkbEnvelopeTypeXYZM    = 4;
 wkbEnvelopeTypeInvalid = 5;



type

  EGeoPackage = class(Exception);

  wkbGeometryType = (
    wkbPoint = 1,
    wkbLineString = 2,
    wkbPolygon = 3,
    wkbMultiPoint = 4,
    wkbMultiLineString = 5,
    wkbMultiPolygon = 6,
    wkbGeometryCollection = 7,
    wkbPolyhedralSurface = 15,
    wkbTIN = 16,
    wkbTriangle = 17,
    wkbPointZ = 1001,
    wkbLineStringZ = 1002,
    wkbPolygonZ = 1003,
    wkbMultiPointZ = 1004,
    wkbMultiLineStringZ = 1005,
    wkbMultiPolygonZ = 1006,
    wkbGeometryCollectionZ = 1007,
    wkbPolyhedralSurfaceZ = 1015,
    wkbTINZ = 1016,
    wkbTrianglez = 1017,
    wkbPointM = 2001,
    wkbLineStringM = 2002,
    wkbPolygonM = 2003,
    wkbMultiPointM = 2004,
    wkbMultiLineStringM = 2005,
    wkbMultiPolygonM = 2006,
    wkbGeometryCollectionM = 2007,
    wkbPolyhedralSurfaceM = 2015,
    wkbTINM = 2016,
    wkbTriangleM = 2017,
    wkbPointZM = 3001,
    wkbLineStringZM = 3002,
    wkbPolygonZM = 3003,
    wkbMultiPointZM = 3004,
    wkbMultiLineStringZM = 3005,
    wkbMultiPolygonZM = 3006,
    wkbGeometryCollectionZM = 3007,
    wkbPolyhedralSurfaceZM = 3015,
    wkbTinZM = 3016,
    wkbTriangleZM = 3017
    );

  wkbGeoPackageType = (wkbStandard := 0, wkbExtented);

  wkPoint = record
    x: double;
    y: double;
  end;

  wkPointZ = record
    x: double;
    y: double;
    z: double;
  end;

  wkPointM = record
    x: double;
    y: double;
    m: double;
  end;

  wkPointZM = record
    x: double;
    y: double;
    z: double;
    m: double;
  end;

  wkbGeometryHeader = record
    isBigEndian: boolean;
    geometryType: wkbGeometryType;
    number: uint32;
  end;

  { TWkbReader }

  TWkbReader = record
  private
    fPtr: pbyte;
    fSize: int32;
    fIsBigEndian: boolean;
    fVersion: byte;
    fEnvelopeType: byte;
    fType: wkbGeoPackageType;
    fIsEmptyGeometry: boolean;
    fSrs_id: int32;
    procedure ReadHeader;
    procedure CalcEnvelope(aPoint:wkPoint);overload;
    procedure CalcEnvelope(aPoint:wkPointZ);overload;
    procedure CalcEnvelope(aPoint:wkPointM);overload;
    procedure CalcEnvelope(aPoint:wkPointZM);overload;
  public
    MinEnvelope: wkPointZM;
    MaxEnvelope: wkPointZM;
    procedure Init(aPtr: pbyte; aSize: int32);
    function ReadByte: byte;
    function ReadUInt32: uint32; overload;
    function ReadUInt32(aIsBigEndian: boolean): uint32; overload;
    function ReadDouble: double; overload;
    function ReadDouble(aIsBigEndian: boolean): double; overload;
    function ReadGeometryHeader: wkbGeometryHeader;
    procedure ReadPoint(out aPoint: wkPoint);
    procedure ReadPointZ(out aPoint: wkPointZ);
    procedure ReadPointM(out aPoint: wkPointM);
    procedure ReadPointZM(out aPoint: wkPointZM);
    property IsBigEndian: boolean read fIsBigEndian;
    property Version: byte read fVersion;
    property IsEmptyGeometry: boolean read fIsEmptyGeometry;
    property Srs_Id: int32 read fSrs_id;
    property EnvelopeType:byte read fEnvelopeType;
  end;

implementation

uses
  Math;

const
  NOT_VALID_VALUE=99999;

{ TWkbReader }
//http://www.geopackage.org/spec/#gpb_format
procedure TWkbReader.ReadHeader;
var
  wB: byte;
const
  Error_message = 'not valid header';
begin
  if fSize < 8 then
    raise EGeoPackage.Create(Error_message);
  wB := ReadByte;
  if wB <> $47 then
    raise EGeoPackage.Create(Error_message);
  wB := ReadByte;
  if wB <> $50 then
    raise EGeoPackage.Create(Error_message);
  fVersion := ReadByte;
  wB := ReadByte; //flags
  fIsBigEndian := (wB and $1) = 0;
  fEnvelopeType := (wB and %00001110) shr 1;
  fType := wkbGeoPackageType((wB and %00100000) shr 5);
  fIsEmptyGeometry := (wB and %00010000) <> 0;
  fSrs_id := ReadUInt32;
  if fEnvelopeType = wkbEnvelopeTypeNone then
  begin
    MinEnvelope.x := NOT_VALID_VALUE;
    MinEnvelope.y := NOT_VALID_VALUE;
    MinEnvelope.z := NOT_VALID_VALUE;
    MinEnvelope.m := NOT_VALID_VALUE;

    MaxEnvelope.x := NOT_VALID_VALUE;
    MaxEnvelope.y := NOT_VALID_VALUE;
    MaxEnvelope.z := NOT_VALID_VALUE;
    MaxEnvelope.m := NOT_VALID_VALUE;
  end
  else
  begin
    MinEnvelope.x := ReadDouble;
    MaxEnvelope.x := ReadDouble;
    MinEnvelope.y := ReadDouble;
    MaxEnvelope.y := ReadDouble;
    if (fEnvelopeType = wkbEnvelopeTypeXYZ) or (fEnvelopeType = wkbEnvelopeTypeXYZM) then
    begin
      MinEnvelope.z := ReadDouble;
      MaxEnvelope.z := ReadDouble;
    end
    else
    begin
      MinEnvelope.z := NOT_VALID_VALUE;
      MaxEnvelope.z := NOT_VALID_VALUE;
    end;
  end;
  if (fEnvelopeType = wkbEnvelopeTypeXYM) or (fEnvelopeType = wkbEnvelopeTypeXYZM) then
  begin
    MinEnvelope.m := ReadDouble;
    MaxEnvelope.m := ReadDouble;
  end
  else
  begin
    MinEnvelope.m := NOT_VALID_VALUE;
    MaxEnvelope.m := NOT_VALID_VALUE;
  end;
end;

procedure TWkbReader.CalcEnvelope(aPoint: wkPoint);
begin
  if (MinEnvelope.x=NOT_VALID_VALUE) or (aPoint.x<MinEnvelope.x) then
    MinEnvelope.x:=aPoint.x;
  if (MaxEnvelope.x=NOT_VALID_VALUE) or (aPoint.x>MaxEnvelope.x) then
    MaxEnvelope.x:=aPoint.x;

  if (MinEnvelope.y=NOT_VALID_VALUE) or (aPoint.y<MinEnvelope.y) then
    MinEnvelope.y:=aPoint.y;
  if (MaxEnvelope.y=NOT_VALID_VALUE) or (aPoint.y>MaxEnvelope.y) then
    MaxEnvelope.y:=aPoint.y;

end;

procedure TWkbReader.CalcEnvelope(aPoint: wkPointZ);
begin
  if (MinEnvelope.x=NOT_VALID_VALUE) or (aPoint.x<MinEnvelope.x) then
    MinEnvelope.x:=aPoint.x;
  if (MaxEnvelope.x=NOT_VALID_VALUE) or (aPoint.x>MaxEnvelope.x) then
    MaxEnvelope.x:=aPoint.x;

  if (MinEnvelope.y=NOT_VALID_VALUE) or (aPoint.y<MinEnvelope.y) then
    MinEnvelope.y:=aPoint.y;
  if (MaxEnvelope.y=NOT_VALID_VALUE) or (aPoint.y>MaxEnvelope.y) then
    MaxEnvelope.y:=aPoint.y;

  if (MinEnvelope.z=NOT_VALID_VALUE) or (aPoint.z<MinEnvelope.z) then
    MinEnvelope.z:=aPoint.z;
  if (MaxEnvelope.z=NOT_VALID_VALUE) or (aPoint.z>MaxEnvelope.z) then
    MaxEnvelope.z:=aPoint.z;

end;

procedure TWkbReader.CalcEnvelope(aPoint: wkPointM);
begin
  if (MinEnvelope.x=NOT_VALID_VALUE) or (aPoint.x<MinEnvelope.x) then
    MinEnvelope.x:=aPoint.x;
  if (MaxEnvelope.x=NOT_VALID_VALUE) or (aPoint.x>MaxEnvelope.x) then
    MaxEnvelope.x:=aPoint.x;

  if (MinEnvelope.y=NOT_VALID_VALUE) or (aPoint.y<MinEnvelope.y) then
    MinEnvelope.y:=aPoint.y;
  if (MaxEnvelope.y=NOT_VALID_VALUE) or (aPoint.y>MaxEnvelope.y) then
    MaxEnvelope.y:=aPoint.y;

  if (MinEnvelope.m=NOT_VALID_VALUE) or (aPoint.m<MinEnvelope.m) then
    MinEnvelope.m:=aPoint.m;
  if (MaxEnvelope.m=NOT_VALID_VALUE) or (aPoint.m>MaxEnvelope.m) then
    MaxEnvelope.m:=aPoint.m;

end;

procedure TWkbReader.CalcEnvelope(aPoint: wkPointZM);
begin
  if (MinEnvelope.x=NOT_VALID_VALUE) or (aPoint.x<MinEnvelope.x) then
    MinEnvelope.x:=aPoint.x;
  if (MaxEnvelope.x=NOT_VALID_VALUE) or (aPoint.x>MaxEnvelope.x) then
    MaxEnvelope.x:=aPoint.x;

  if (MinEnvelope.y=NOT_VALID_VALUE) or (aPoint.y<MinEnvelope.y) then
    MinEnvelope.y:=aPoint.y;
  if (MaxEnvelope.y=NOT_VALID_VALUE) or (aPoint.y>MaxEnvelope.y) then
    MaxEnvelope.y:=aPoint.y;

  if (MinEnvelope.z=NOT_VALID_VALUE) or (aPoint.z<MinEnvelope.z) then
    MinEnvelope.z:=aPoint.z;
  if (MaxEnvelope.z=NOT_VALID_VALUE) or (aPoint.z>MaxEnvelope.z) then
    MaxEnvelope.z:=aPoint.z;

  if (MinEnvelope.m=NOT_VALID_VALUE) or (aPoint.m<MinEnvelope.m) then
    MinEnvelope.m:=aPoint.m;
  if (MaxEnvelope.m=NOT_VALID_VALUE) or (aPoint.m>MaxEnvelope.m) then
    MaxEnvelope.m:=aPoint.m;
end;

procedure TWkbReader.Init(aPtr: pbyte; aSize: int32);
begin
  fPtr := aPtr;
  fSize := aSize;
  if fPtr = nil then
    fSize := 0;
  ReadHeader;
end;

function TWkbReader.ReadByte: byte;
begin
  Result := 0;
  if fSize <= 0 then
    Exit(0);
  Result := fPtr^;
  Inc(fPtr);
  Dec(fSize);
end;

function TWkbReader.ReadUInt32: uint32;
begin
  Result := ReadUInt32(fIsBigEndian);
end;

function TWkbReader.ReadUInt32(aIsBigEndian: boolean): uint32;
begin
  Result := 0;
  if fSize < sizeof(uint32) then
    Exit(0);
  Result := PUInt32(fPtr)^;
  Inc(fPtr, sizeof(uint32));
  Dec(fSize, sizeof(uint32));
  {$IFDEF ENDIAN_LITTLE}
    if aIsBigEndian then
      Result:=SwapEndian(Result);
  {$ELSE}
  if not aIsBigEndian then
    Result := SwapEndian(Result);
  {$ENDIF}
end;

function TWkbReader.ReadDouble: double;
begin
  Result := ReadDouble(fIsBigEndian);
end;

function TWkbReader.ReadDouble(aIsBigEndian: boolean): double;
var
  wR: double;
  wI: int64 absolute wR;
begin
  wR := 0.0;
  if fSize < sizeof(double) then
    Exit(0.0);
  wR := PDouble(fPtr)^;
  Inc(fPtr, sizeof(double));
  Dec(fSize, sizeof(double));
  {$IFDEF ENDIAN_LITTLE}
    if aIsBigEndian then
      wI:=SwapEndian(wI);
  {$ELSE}
  if not aIsBigEndian then
    wI := SwapEndian(wI);
  {$ENDIF}
  Result := wR;
end;

function TWkbReader.ReadGeometryHeader: wkbGeometryHeader;
begin
  Result.isBigEndian := ReadByte = 0;
  fIsBigEndian := Result.isBigEndian;
  Result.geometrytype := wkbGeometryType(ReadUInt32);
  if (Result.geometrytype <> wkbPoint) and (Result.geometrytype <> wkbPointZ)
    and (Result.geometrytype <> wkbPointM) and
    (Result.geometrytype <> wkbPointZM) then
    Result.number := ReadUInt32
  else
    Result.number := 1;
end;

procedure TWkbReader.ReadPoint(out aPoint: wkPoint);
begin
  aPoint.x := ReadDouble;
  aPoint.y := ReadDouble;
  if fEnvelopeType=wkbEnvelopeTypeNone then
    CalcEnvelope(aPoint);
end;

procedure TWkbReader.ReadPointZ(out aPoint: wkPointZ);
begin
  aPoint.x := ReadDouble;
  aPoint.y := ReadDouble;
  aPoint.z := ReadDouble;
  if fEnvelopeType=wkbEnvelopeTypeNone then
    CalcEnvelope(aPoint);
end;

procedure TWkbReader.ReadPointM(out aPoint: wkPointM);
begin
  aPoint.x := ReadDouble;
  aPoint.y := ReadDouble;
  aPoint.m := ReadDouble;
  if fEnvelopeType=wkbEnvelopeTypeNone then
    CalcEnvelope(aPoint);
end;

procedure TWkbReader.ReadPointZM(out aPoint: wkPointZM);
begin
  aPoint.x := ReadDouble;
  aPoint.y := ReadDouble;
  aPoint.z := ReadDouble;
  aPoint.m := ReadDouble;
  if fEnvelopeType=wkbEnvelopeTypeNone then
    CalcEnvelope(aPoint);
end;

end.
