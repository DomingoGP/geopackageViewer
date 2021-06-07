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

{ Convert from well know binary format from geopackage to GeoJson.}

unit wkb2geojsonu;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, geopackagewkb;

type
  wkb2GeoJson = record
  private
    FwkbReader: TWkbReader;
    FGH: wkbGeometryHeader;
    FS: TFormatSettings;
    FFmtStr: string;
    procedure ReadPoint(const aGH: wkbGeometryHeader);
    procedure ReadLineString(const aGH: wkbGeometryHeader; aWithHeader: boolean);
    procedure ReadPoligono(const aGH: wkbGeometryHeader;aWithHeader: boolean);
    procedure ReadMultiPoligono(const aGH: wkbGeometryHeader;aWithHeader: boolean);
    procedure ReadMultiLineString(const aGH: wkbGeometryHeader;aWithHeader: boolean);
    procedure ReadMultiPoint(const aGH: wkbGeometryHeader;aWithHeader: boolean);
    function ReadBarePoint: string;
    function FF(aNumber: double): string;
  public
    FGJString: string;
    procedure GeometryToGeoJson(APtrWellKnowBinary: pbyte; ASize: integer; aNumDecimals: integer);
  end;

implementation


function wkb2GeoJson.FF(aNumber: double): string;
begin
  Result := Format(FFmtStr, [aNumber], FS);
end;

function wkb2GeoJson.ReadBarePoint: string;
var
  wPunto: wkPoint;
begin
  FwkbReader.ReadPoint(wPunto);
  Result := '[' + FF(wPunto.X) + ',' + FF(wPunto.Y) + ']';
end;

procedure wkb2GeoJson.ReadPoint(const aGH: wkbGeometryHeader);
begin
  FGJString := FGJString + '"type": "Point",' + LineEnding;
  FGJString := FGJString + '"coordinates": ' + ReadBarePoint + LineEnding;
end;

procedure wkb2GeoJson.ReadLineString(const aGH: wkbGeometryHeader; aWithHeader: boolean);
var
  wZ: integer;
  wPuntos: integer;
begin
  if aWithHeader then
  begin
    FGJString := FGJString + '"type": "LineString",' + LineEnding;
    FGJString := FGJString + '"coordinates": ' + LineEnding;
  end;
  FGJString := FGJString + '[';
  wPuntos := aGH.number;
  for wZ := 1 to wPuntos do
  begin
    FGJString := FGJString + ReadBarePoint;
    if wZ < wPuntos then
      FGJString := FGJString + ',';
  end;
  FGJString := FGJString + ']' + LineEnding;
end;

procedure wkb2GeoJson.ReadPoligono(const aGH: wkbGeometryHeader;aWithHeader: boolean);
var
  wPuntos: integer;
  wRings: uint32;
  wZ: integer;
begin
  if aWithHeader then
  begin
    FGJString := FGJString + '"type": "Polygon",' + LineEnding;
    FGJString := FGJString + '"coordinates": ' + LineEnding;
  end;
  FGJString := FGJString + '[';
  for wRings := 1 to aGH.number do
  begin
    FGJString := FGJString + '[';
    wPuntos := FwkbReader.ReadUInt32(aGH.isBigEndian);
    for wZ := 1 to wPuntos do
    begin
      FGJString := FGJString + ReadBarePoint;
      if wZ < wPuntos then
        FGJString := FGJString + ',';
    end;
    FGJString := FGJString + ']';
    if wRings < aGH.number then
      FGJString := FGJString + ','+LineEnding;
  end;
  FGJString := FGJString + ']' + LineEnding;
end;

procedure wkb2GeoJson.ReadMultiPoligono(const aGH: wkbGeometryHeader;aWithHeader: boolean);
var
  wP: integer;
  wGH: wkbGeometryHeader;
  wPuntos:integer;
begin
  if aWithHeader then
  begin
    FGJString := FGJString + '"type": "MultiPolygon",' + LineEnding;
    FGJString := FGJString + '"coordinates": ' + LineEnding;
  end;
  FGJString := FGJString + '[';
  wPuntos:=aGH.number;
  for wP := 1 to wPuntos do
  begin
    wGH := FwkbReader.ReadGeometryHeader;
    ReadPoligono(wGH,False);
    if wP < wPuntos then
      FGJString := FGJString + ','+LineEnding;
  end;
  FGJString := FGJString + ']' + LineEnding;
end;


procedure wkb2GeoJson.ReadMultiLineString(const aGH: wkbGeometryHeader;aWithHeader: boolean);
var
  wRings: integer;
  wGH: wkbGeometryHeader;
begin
  if aWithHeader then
  begin
    FGJString := FGJString + '"type": "MultiLineString",' + LineEnding;
    FGJString := FGJString + '"coordinates": ' + LineEnding;
  end;
  FGJString := FGJString + '[';
  for wRings := 1 to aGH.number do
  begin
    wGH := FwkbReader.ReadGeometryHeader;
    ReadLineString(wGH,false);
    if wRings < aGH.number then
      FGJString := FGJString + ','+LineEnding;
  end;
  FGJString := FGJString + ']' + LineEnding;
end;

procedure wkb2GeoJson.ReadMultiPoint(const aGH: wkbGeometryHeader;aWithHeader: boolean);
var
  wZ: integer;
  wPuntos: integer;
begin
  if aWithHeader then
  begin
    FGJString := FGJString + '"type": "MultiPoint",' + LineEnding;
    FGJString := FGJString + '"coordinates": ' + LineEnding;
  end;
  FGJString := FGJString + '[';

  wPuntos := aGH.number;
  for wZ := 1 to wPuntos do
  begin
    FGJString := FGJString + ReadBarePoint;
    if wZ<wPuntos then
      FGJString := FGJString + ',';
  end;
  FGJString := FGJString + ']' + LineEnding;
end;

procedure wkb2GeoJson.GeometryToGeoJson(APtrWellKnowBinary: pbyte;
  ASize: integer; aNumDecimals: integer);
begin
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := ',';
  FFmtStr := '%.' + IntToStr(aNumDecimals) + 'f';
  FwkbReader.Init(APtrWellKnowBinary, ASize);
  FGH := FwkbReader.ReadGeometryHeader;
  if FwkbReader.EnvelopeType <> wkbEnvelopeTypeNone then
  begin
    FGJString := FGJString + '"bbox": ['
      +FF(FwkbReader.MinEnvelope.x)+',' +FF(FwkbReader.MinEnvelope.y)+','
      +FF(FwkbReader.MaxEnvelope.x)+','+FF(FwkbReader.MaxEnvelope.y)+'],'+LineEnding;
  end;
  FGJString := FGJString + '"geometry": {' + LineEnding;
  case FGH.geometryType of
    wkbMultiPolygon: ReadMultiPoligono(FGH, True);
    wkbPolygon: ReadPoligono(FGH, True);
    wkbPoint: ReadPoint(FGH);
    wkbMultiPoint: ReadMultiPoint(FGH, True);
    wkbMultiLineString: ReadMultiLineString(FGH,True);
    wkbLineString: ReadLineString(FGH, True);
    else
    //not supported yet.
  end;
  FGJString := FGJString + '}' + LineEnding;
end;

end.
