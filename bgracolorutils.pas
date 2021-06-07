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
unit bgracolorutils;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, BGRABitmapTypes, graphics;

function IsCharHex(ch: char): boolean;
function H2I(c:char):integer;
function Hex2CharToInt(C1,C2:char):integer;
function HtmlToBGRAPixel(const AColor:string;var aC:TBGRAPixel ):boolean;
function BGRAPixelToHtml(aC:TBGRAPixel):string;
function HTmlColorToTColor(const AColor:string;ADefault:TColor=clBlack):TColor;
function HTmlColorToTBGRAPixel(const AColor:string;ADefault:TBGRAPixel):TBGRAPixel;



implementation


{
Strings that begin with '#' will be parsed as hexadecimal in the following way:
#RGB (becomes RRGGBB)
#RRGGBB

#RGBA (becomes RRGGBBAA)
#RRGGBBAA

When not specified alpha will default to FF.
}

function IsCharHex(ch: char): boolean;
begin
  Result:= ch in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

// el caracter debe ser correcto 0..9 a..f  A..F
function H2I(c:char):integer;
begin
  result:=ord(c);
  if result>=97 then  //'a'
    result:=result -87 //97 + 10;  'a'
  else if result>=65 then //'A'
    result:=result -55 // 65 +10; 'A'
  else
    result:=result -48;  // '0'
end;

function Hex2CharToInt(C1,C2:char):integer;
begin
  result:=(h2i(c1) shl 4) + h2i(c2);
end;

function HtmlToBGRAPixel(const AColor:string;var aC:TBGRAPixel ):boolean;
var
  Len,i:integer;
begin
  result:=false;
  aC:=BGRA(0,0,0,0);
  Len:=Length(AColor);
  if (Len<>4) and (Len<>7) and (Len<>9) and (Len<>5) then
    exit;
  if AColor[1]<>'#' then
    exit;
  for i:= 2 to Len do
  begin
    if not IsCharHex(AColor[i]) then
      exit;
  end;
  aC.alpha:=$FF;
  if Len>=7 then
  begin
    aC.red:=Hex2CharToInt(AColor[2],AColor[3]); // StrToInt('$'+Copy(AColor, 2, 2));
    aC.green:=Hex2CharToInt(AColor[4],AColor[5]); // StrToInt('$'+Copy(AColor, 4, 2));
    aC.blue:=Hex2CharToInt(AColor[6],AColor[7]); // StrToInt('$'+Copy(AColor, 6, 2));
    if Len=9 then
      aC.alpha:=Hex2CharToInt(AColor[8],AColor[9]); // StrToInt('$'+Copy(AColor,8,2));
  end
  else
  begin
    aC.red:=Hex2CharToInt(AColor[2],AColor[2]); // StrToInt('$'+AColor[2]+AColor[2]);
    aC.green:=Hex2CharToInt(AColor[3],AColor[3]); // StrToInt('$'+AColor[3]+AColor[3]);
    aC.blue:=Hex2CharToInt(AColor[4],AColor[4]); // StrToInt('$'+AColor[4]+AColor[4]);
    if Len=5 then
      aC.alpha:=Hex2CharToInt(AColor[5],AColor[5]);
  end;
  Result:= true;
end;

function BGRAPixelToHtml(aC:TBGRAPixel):string;
begin
  result:='#'+IntToHex(aC.red,2)+IntToHex(aC.green,2)+IntToHex(aC.blue,2)+IntToHex(aC.alpha,2);
end;

//ignoramos alpha aunque lo pasen.
function HTmlColorToTColor(const AColor:string;ADefault:TColor=clBlack):TColor;
var
  wC:TBGRAPixel;
begin
  if  HtmlToBGRAPixel(aColor,wC) then
  begin
    {$IFDEF FPC}
    Result:= RGBToColor(wC.red,wC.green,wC.blue);
    {$ELSE}
    Result:=RGB(wC.red,wC.green,wC.blue);
    {$ENDIF}
  end
  else
    result:=ADefault;
end;

// #RRGGBB  y #RRGGBBAA
function HTmlColorToTBGRAPixel(const AColor:string;ADefault:TBGRAPixel):TBGRAPixel;
begin
  if  HtmlToBGRAPixel(aColor,result)=false then
    result:=ADefault;
end;

end.

