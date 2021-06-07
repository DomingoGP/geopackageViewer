{
Port to freepascal
ISC License
Copyright (c) 2021 Jamie Philbrook, Domingo Galmés

Original project license
ISC License
Copyright (c) 2016 Mapbox

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
unit polylabelu;

{
 https://blog.mapbox.com/a-new-algorithm-for-finding-a-visual-center-of-a-polygon-7c77e6492fbc
 https://github.com/mapbox/polylabel
 https://forum.lazarus.freepascal.org/index.php/topic,54795.0.html
}

{$mode objfpc}{$H+}
{$define IMPLEMENT_TPOINT}
{$define IMPLEMENT_TPOINTF}

interface

uses
  Classes, Graphics, SysUtils, Types, Math
  {$ifdef IMPLEMENT_TPOINTF}, BGRABitmapTypes{$endif}
  ;

type

  TPoleOfInaccessibility = record
    PlotX, PlotY: single;
    Distance: single;
    CentroidX, CentroidY: single;
  end;

{$ifdef IMPLEMENT_TPOINTF}
function polylabel(const aPolygon: array of TPointF;
  aPrecision: single = 1.0): TPoleOfInaccessibility; overload;
{$endif}
{$ifdef IMPLEMENT_TPOINT}
function polylabel(const aPolygon: array of TPoint;
  aPrecision: single = 1.0): TPoleOfInaccessibility; overload;
{$endif}

implementation

uses
  GPriorityQueue;

type
  PCell = ^TCell;

  TCell = record
    CenterX: single; // cell center x
    CenterY: single; // cell center y
    H: single;  // half the cell size
    D: single;  // distance from cell center to polygon
    Max: single;   // max distance to polygon within a cell
  end;

  lesspcell = class
  public
    class function c(a, b: PCell): boolean; inline;
  end;

class function lesspcell.c(a, b: PCell): boolean; inline;
begin
  Result := a^.Max < b^.max;
end;

type
  TPQueuePCell = specialize TPriorityQueue<PCell, lesspcell>;

const
  SQRT_OF_2 = 1.41421356237;

{$ifdef IMPLEMENT_TPOINTF}
// get squared distance from a point to a segment
function GetSegDistSq(aX, aY: single; aA, aB: TPointF): single; overload;
var
  x, y, dx, dy: double;
  t: double;
begin
  x := aA.x;
  y := aA.y;
  dx := aB.x - x;
  dY := aB.y - y;
  if (dx <> 0) or (dy <> 0) then
  begin
    T := ((aX - x) * dx + (aY - y) * dy) / (dx * dx + dy * dy);

    if (t > 1) then
    begin
      x := aB.x;
      y := aB.y;
    end
    else if (t > 0) then
    begin
      x := x + (dx * t);
      y := y + (dy * t);
    end;
  end;
  dx := aX - x;
  dy := aY - y;
  Result := dx * dx + dy * dy;
end;

function PointToPolygonDist(aX, aY: single;
  const polygon: array of TPointF): single; overload;
var
  Inside: boolean = False;
  minDistSq: single = infinity;
  I, J: integer;
  A, B: TPointF;
begin
  I := 0;
  J := Length(PolyGon) - 1;
  while I < Length(PolyGon) do
  begin
    A := PolyGon[I];
    B := PolyGon[J];
    if ((A.Y > aY) <> (B.Y > aY)) and (aX < (B.X - A.x) * (aY - A.Y) /
      (B.Y - A.Y) + A.X) then
      Inside := not Inside;
    J := I;
    Inc(I);
    minDistSq := min(minDistSq, GetSegDistSq(aX, aY, A, B));
  end;
  if MinDistSq = 0 then
    Result := 0
  else
  begin
    Result := Sqrt(MinDistSq);
    if not Inside then
      Result := -Result;
  end;
end;

function NewCell(aX, aY, aH: single; const aPolyGon: array of TPointF): PCell; overload;
begin
  GetMem(Result, sizeof(TCell));
  Result^.CenterX := aX;
  Result^.CenterY := aY;
  Result^.H := aH;
  Result^.D := PointToPolyGonDist(Result^.CenterX, Result^.CenterY, APolyGon);
  Result^.Max := Result^.d + Round(Result^.h * SQRT_OF_2);
end;

// get polygon centroid
function getCentroidCell(const aPolygon: array of TPointF): Pcell; overload;
var
  area: single = 0;
  x: single = 0;
  y: single = 0;
  A, B: TPointF;
  F: single;
  I, J: integer;
begin
  I := 0;
  J := Length(aPolygon) - 1;
  while I < Length(aPolygon) do
  begin
    a := aPolygon[i];
    b := aPolygon[j];
    f := a.x * b.y - b.x * a.y;
    x += (a.x + b.x) * f;
    y += (a.y + b.y) * f;
    area += f * 3;
    J := I;
    Inc(I);
  end;
  if Area = 0 then
    Result := NewCell(aPolygon[0].x,aPolygon[0].y, 0, aPolygon)
  else
    Result := NewCell(X / area, y / area, 0, aPolygon);
end;

function polylabel(const aPolygon: array of TPointF;
  aPrecision: single = 1.0): TPoleOfInaccessibility;
var
  MinX: single;
  MinY: single;
  MaxX: single;
  MaxY: single;
  H, Width, Height, CellSize: single;
  I: integer;
  X, Y: single;
  P: TPointF;
  CellQueue: TPQueuePCell;
  BBoxCell, BestCell, Cell: PCell;
  NumProbes: integer;
const
  MIN_POLYGON_SIZE=10;
begin
  // find the bounding box of the outer ring
  for i := 0 to Length(aPolygon) - 1 do
  begin
    p := aPolygon[i];
    if (I = 0) or (P.x < minX) then
      MinX := P.x;
    if (I = 0) or (p.y < minY) then
      MinY := P.y;
    if (I = 0) or (P.x > MaxX) then
      MaxX := P.x;
    if (I = 0) or (P.y > MaxY) then
      MaxY := P.y;
  end;
  Width := maxX - minX;
  Height := maxY - minY;
  CellSize := Min(Width, Height);
  H := CellSize / 2;
  //if CellSize = 0 then      // error out of memory
  //begin
  //  Result.PlotX := MinX;
  //  Result.PlotY := MinY;
  //  Result.Distance := 0;
  //  Exit;
  //end;
  if CellSize < MIN_POLYGON_SIZE then  // poligon to small, return center.
  begin
    Result.PlotX := (MinX+MaxX)/2;
    Result.PlotY := (MinY+MaxY)/2;
    Result.Distance := 0;
    Exit;
  end;

  // a priority queue of cells in order of their "potential" (max distance to aPolygon)
  CellQueue := TPQueuePCell.Create;
  X := MinX;
  while X < MaxX do
  begin
    Y := MinY;
    while Y < MaxY do
    begin
      CellQueue.Push(NewCell(x + h, y + h, h, aPolygon));
      Y := Y + CellSize;
    end;
    X := X + CellSize;
  end;
  bestCell := getCentroidCell(aPolygon);
  Result.CentroidX := BestCell^.CenterX;
  Result.CentroidY := BestCell^.CenterY;
  BBoxcell := NewCell(minX + Width / 2, minY + Height / 2, 0, aPolygon);

  if (bboxCell^.d > bestCell^.d) then
    bestCell^ := bboxCell^;
  NumProbes := CellQueue.Size;
  while (CellQueue.Size <> 0) do
  begin
    Cell := CellQueue.Top;
    CellQueue.Pop;
    if Cell^.D > BestCell^.D then
      BestCell^ := Cell^;
    if (cell^.max - bestCell^.d <= aPrecision) then
    begin
      FreeMem(Cell);
      continue;
    end;
    H := Cell^.H / 2;
    CellQueue.Push(NewCell(cell^.CenterX - h, cell^.CenterY - h, h, aPolygon));
    CellQueue.Push(NewCell(cell^.CenterX + h, cell^.CenterY - h, h, aPolygon));
    CellQueue.Push(NewCell(cell^.CenterX - h, cell^.CenterY + h, h, aPolygon));
    CellQueue.Push(NewCell(cell^.CenterX + h, cell^.CenterY + h, h, aPolygon));
    Inc(NumProbes,4);
    FreeMem(Cell);
  end;
  Result.PlotX := BestCell^.CenterX;
  Result.PlotY := BestCell^.CenterY;
  Result.Distance := BestCell^.D;
  FreeMem(BBOxcell);
  FreeMem(BestCell);
  CellQueue.Free;
end;
{$endif}

//------------------------------------------------------------------------------
{$ifdef IMPLEMENT_TPOINT}

// get squared distance from a point to a segment
function GetSegDistSq(aX, aY: single; aA, aB: TPoint): single; overload;
var
  X, Y, dx, dy: double;
  T: double;
begin
  X := aA.x;
  Y := aA.Y;
  dx := aB.X - X;
  dY := aB.Y - Y;
  if (dx <> 0) or (dy <> 0) then
  begin
    T := ((aX - x) * dx + (aY - y) * dy) / (dx * dx + dy * dy);

    if (t > 1) then
    begin
      x := aB.x;
      y := aB.y;
    end
    else if (t > 0) then
    begin
      x += (dx * t);
      y += (dy * t);
    end;
  end;
  dx := aX - x;
  dy := aY - y;
  Result := dx * dx + dy * dy;
end;

function PointToPolygonDist(aX, aY: single;
  const aPolygon: array of TPoint): single; overload;
var
  Inside: boolean = False;
  minDistSq: single = infinity;
  I, J: integer;
  A, B: TPoint;
begin
  I := 0;
  J := Length(aPolygon) - 1;
  while I < Length(aPolygon) do
  begin
    A := aPolygon[I];
    B := aPolygon[J];
    if ((A.Y > aY) <> (B.Y > aY)) and (aX < (B.X - A.x) * (aY - A.Y) /
      (B.Y - A.Y) + A.X) then
      Inside := not Inside;
    J := I;
    Inc(I);
    minDistSq := min(minDistSq, GetSegDistSq(aX, aY, A, B));
  end;

  if MinDistSq = 0 then
    Result := 0
  else
  begin
    Result := Sqrt(MinDistSq);
    if not Inside then
      Result := -Result;
  end;
end;

function NewCell(aX, aY, aH: single; const aPolyGon: array of TPoint): PCell; overload;
begin
  GetMem(Result, sizeof(TCell));
  Result^.CenterX := aX;
  Result^.CenterY := aY;
  Result^.H := aH;
  Result^.D := PointToPolyGonDist(Result^.CenterX, Result^.CenterY, APolyGon);
  Result^.Max := Result^.d + Round(Result^.h * SQRT_OF_2);
end;

// get polygon centroid
function getCentroidCell(const aPolygon: array of TPoint): Pcell; overload;
var
  area: single = 0;
  x: single = 0;
  y: single = 0;
  A, B: TPoint;
  F: single;
  I, J: integer;
begin
  I := 0;
  J := Length(aPolygon) - 1;
  while I < Length(aPolygon) do
  begin
    a := aPolygon[i];
    b := aPolygon[j];
    f := a.x * b.y - b.x * a.y;
    x += (a.x + b.x) * f;
    y += (a.y + b.y) * f;
    area += f * 3;
    J := I;
    Inc(I);
  end;
  if Area = 0 then
    Result := NewCell(aPolygon[0].x,aPolygon[0].y, 0, aPolygon)
  else
    Result := NewCell(X / area, y / area, 0, aPolygon);
end;

function polylabel(const aPolygon: array of TPoint;
  aPrecision: single = 1.0): TPoleOfInaccessibility;
var
  MinX: single;
  MinY: single;
  MaxX: single;
  MaxY: single;
  H, Width, Height, CellSize: single;
  I: integer;
  X, Y: single;
  P: TPoint;
  CellQueue: TPQueuePCell;
  BBoxCell, BestCell, Cell: PCell;
  NumProbes: integer;
const
  MIN_POLYGON_SIZE=10;
begin
  // find the bounding box of the outer ring
  for i := 0 to Length(aPolygon) - 1 do
  begin
    p := aPolygon[i];
    if (I = 0) or (P.x < minX) then
      MinX := P.x;
    if (I = 0) or (p.y < minY) then
      MinY := P.y;
    if (I = 0) or (P.x > MaxX) then
      MaxX := P.x;
    if (I = 0) or (P.y > MaxY) then
      MaxY := P.y;
  end;
  Width := maxX - minX;
  Height := maxY - minY;
  CellSize := Min(Width, Height);
  H := CellSize / 2;
  //if CellSize = 0 then      // error out of memory
  //begin
  //  Result.PlotX := MinX;
  //  Result.PlotY := MinY;
  //  Result.Distance := 0;
  //  Exit;
  //end;
  if CellSize < MIN_POLYGON_SIZE then  // poligon to small, return center.
  begin
    Result.PlotX := (MinX+MaxX)/2;
    Result.PlotY := (MinY+MaxY)/2;
    Result.Distance := 0;
    Exit;
  end;
  // a priority queue of cells in order of their "potential" (max distance to aPolygon)
  CellQueue := TPQueuePCell.Create;
  X := MinX;
  while X < MaxX do
  begin
    Y := MinY;
    while Y < MaxY do
    begin
      CellQueue.Push(NewCell(x + h, y + h, h, aPolygon));
      Y := Y + CellSize;
    end;
    X := X + CellSize;
  end;
  bestCell := getCentroidCell(aPolygon);
  Result.CentroidX := BestCell^.CenterX;
  Result.CentroidY := BestCell^.CenterY;
  BBoxcell := NewCell(minX + Width / 2, minY + Height / 2, 0, aPolygon);

  if (bboxCell^.d > bestCell^.d) then
    bestCell^ := bboxCell^;
  NumProbes := CellQueue.Size;
  while (CellQueue.Size <> 0) do
  begin
    Cell := CellQueue.Top;
    CellQueue.Pop;
    if Cell^.D > BestCell^.D then
      BestCell^ := Cell^;
    if (cell^.max - bestCell^.d <= aPrecision) then
    begin
      FreeMem(Cell);
      continue;
    end;
    H := Cell^.H / 2;
    CellQueue.Push(NewCell(cell^.CenterX - h, cell^.CenterY - h, h, aPolygon));
    CellQueue.Push(NewCell(cell^.CenterX + h, cell^.CenterY - h, h, aPolygon));
    CellQueue.Push(NewCell(cell^.CenterX - h, cell^.CenterY + h, h, aPolygon));
    CellQueue.Push(NewCell(cell^.CenterX + h, cell^.CenterY + h, h, aPolygon));
    Inc(NumProbes, 4);
    FreeMem(Cell);
  end;
  Result.PlotX := BestCell^.CenterX;
  Result.PlotY := BestCell^.CenterY;
  Result.Distance := BestCell^.D;
  FreeMem(BBOxcell);
  FreeMem(BestCell);
  CellQueue.Free;
end;
{$endif}


end.
