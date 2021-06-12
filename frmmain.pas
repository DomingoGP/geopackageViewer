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
unit frmMain;

{$mode objfpc}{$H+}

// in windows
// requires sqlite3.dll in the program folder and in the lazarus.exe folder.

//by default maps are drawed using the web mercator projection. EPSG:3857

// requires library proj7 installed in the computer.
// allow display maps in other projections.

// may require changing the path in the
//  procedure ProjInit; in the geopackagedrawerproj unit.
// and the dll name in proj.pas unit
{$DEFINE USE_PROJ7}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, DBGrids, ComCtrls, DBCtrls, ExtCtrls, Spin, MaskEdit,
  Menus, SQLiteWrap, geopackagewkb, geopackagedrawer, BGRABitmap,
  BGRABitmapTypes;

type

  { TForm1 }

  TGraphicProps = record
    Pos0: wkPoint;  // lat, lon
    X0, Y0: integer;
    ZoomLevel: integer;
  end;


  TForm1 = class(TForm)
    btnDraw: TButton;
    btnFillColor: TColorButton;
    btnOceanColor: TColorButton;
    btnFilter: TButton;
    btnLineColor: TColorButton;
    btnBackgroundColor: TColorButton;
    btnOpen: TButton;
    btnSave: TButton;
    btnSaveSettings: TButton;
    btnResize: TButton;
    btnDrawMapProj: TButton;
    btnSetOrigin: TButton;
    btnQueryExec: TButton;
    btnToGeoJson: TButton;
    cbClearBitmap: TCheckBox;
    cbLabelField: TComboBox;
    cbProjection: TComboBox;
    cbTables: TComboBox;
    cbGrid: TComboBox;
    cbClipLat: TCheckBox;
    cbOnlyDrawPoints: TCheckBox;
    cbPreFillOceans: TCheckBox;
    DataSource1: TDataSource;
    DataSourceSlave: TDataSource;
    dsQueryDB: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    DBNavigator1: TDBNavigator;
    edBackgroundColor: TMaskEdit;
    edOceanColor: TMaskEdit;
    edQuery: TEdit;
    edLineColor: TMaskEdit;
    edFilter: TEdit;
    FileNameEdit1: TFileNameEdit;
    edFillColor: TMaskEdit;
    Label12: TLabel;
    lbProjection: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lbBgraBitmap: TLabel;
    lbProjLib: TLabel;
    lbRepoUrl1: TLabel;
    lbSQLite: TLabel;
    miSetOrigin: TMenuItem;
    miDraw: TMenuItem;
    miCopyToGeoJson: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupMenuGraphic: TPopupMenu;
    ScrollBox1: TScrollBox;
    sePointRadius: TFloatSpinEdit;
    seLineWidth: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lbStyle: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbRepoUrl: TLabel;
    pnlStyle: TPanel;
    seFontHeight: TSpinEdit;
    seLat: TFloatSpinEdit;
    seLon: TFloatSpinEdit;
    seOpacity: TSpinEdit;
    seZoom: TSpinEdit;
    seBitmapWidth: TSpinEdit;
    seBitmapHeight: TSpinEdit;
    SQLQueryDB: TSQLQuery;
    StatusBar1: TStatusBar;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuerySlave: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    tbsDatabase: TTabSheet;
    tbsData: TTabSheet;
    tbsGeo: TPageControl;
    tbsGraphic: TTabSheet;
    procedure btnBackgroundColorColorChanged(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure btnDrawMapProjClick(Sender: TObject);
    procedure btnFillColorColorChanged(Sender: TObject);
    procedure btnLineColorColorChanged(Sender: TObject);
    procedure btnOceanColorColorChanged(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure btnQueryExecClick(Sender: TObject);
    procedure btnResizeClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnSetOriginClick(Sender: TObject);
    procedure btnToGeoJsonClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure edBackgroundColorEditingDone(Sender: TObject);
    procedure edFillColorEditingDone(Sender: TObject);
    procedure edLineColorEditingDone(Sender: TObject);
    procedure edOceanColorEditingDone(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbProjLibClick(Sender: TObject);
    procedure lbBgraBitmapClick(Sender: TObject);
    procedure lbRepoUrlClick(Sender: TObject);
    procedure lbSQLiteClick(Sender: TObject);
    procedure lbStyleClick(Sender: TObject);
    procedure miCopyToGeoJsonClick(Sender: TObject);
    procedure miDrawClick(Sender: TObject);
    procedure miSetOriginClick(Sender: TObject);
    procedure PaintBox1MouseLeave(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure ScrollBox1Paint(Sender: TObject);
    procedure SQLQuery1AfterScroll(DataSet: TDataSet);
    procedure FillTablasComboBox;
  private
    geopackagewkb: TGeoPackage;
    graphic: TGraphicProps;
    BitmapMapa: TBGRABitmap;
    projDefaultPath: string;
    Drawer: TGeoPackageDrawer;
    procedure DrawMap(aDrawer: TGeoPackageDrawer);
    procedure FillLabelFieldNames;
    procedure LoadSettings;
    procedure Open(const aFileName: string);
    procedure SaveSettings;
    procedure SetupDrawer(aDrawer: TGeoPackageDrawer);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  lclintf, laz_xmlcfg,
  {$IFDEF USE_PROJ7}
  geopackagedrawerproj,
  {$ENDIF}
  bgracolorutils, wkb2geojsonu, Clipbrd;

{$R *.lfm}

{ TForm1 }
const
  GridInterval: array[0..6] of TDrawInterval = (di0, di5, di10, di15, di30, di45, di90);

function Spaces2Zeros(const aText: string): string;
var
  wI: integer;
begin
  Result := aText;
  for wI := 1 to Length(Result) do
  begin
    if Result[wI] = ' ' then
    begin
      Result[wI] := '0';
    end;
  end;
end;

procedure TForm1.Open(const aFileName: string);
begin
  if not FileExists(aFileName) then
  begin
    ShowMessage('File ' + aFileName + ' not exists');
    Exit;
  end;
  SQLQuerySlave.Active := False;
  SQLQuery1.Active := False;
  SQLite3Connection1.Connected := False;
  SQLite3Connection1.DatabaseName := aFileName;
  SQLite3Connection1.Connected := True;
  SQLQuery1.Active := True;
  SQLQuery1AfterScroll(SQLQuery1);
  tbsGeo.Visible := True;
  geopackagewkb.Open(aFileName);
  FillTablasComboBox;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  Open(FileNameEdit1.Text);
end;

procedure TForm1.btnDrawClick(Sender: TObject);
begin
  DrawMap(TGeoPackageDrawer.Create(BitmapMapa, seZoom.Value, seLat.Value, seLon.Value));
end;

procedure TForm1.SetupDrawer(aDrawer: TGeoPackageDrawer);
begin
  if cbClipLat.Checked then
    Drawer.SetLatBounds(-85.05112878, 85.05112878)
  else
    aDrawer.SetLatBounds(-90.0, 90.0);
  aDrawer.OnlyDrawPoints := cbOnlyDrawPoints.Checked;
  if seZoom.Value < 4 then
    aDrawer.GridTileSize := 8
  else
    aDrawer.GridTileSize := 16;
  aDrawer.FillColor := HTmlColorToTBGRAPixel(Spaces2Zeros(edFillColor.Text), clGreen);
  aDrawer.BorderColor := HTmlColorToTBGRAPixel(Spaces2Zeros(edLineColor.Text), clRed);
  aDrawer.LineWidth := seLineWidth.Value;
  aDrawer.PointRadius := sePointRadius.Value;
  aDrawer.FontHeigth := seFontHeight.Value;
end;

procedure TForm1.DrawMap(aDrawer: TGeoPackageDrawer);
var
  wType: string;
  wOldCursor: TCursor;
  wColor: TBGRAPixel;
begin
  if cbClearBitmap.Checked then
    BitmapMapa.Fill(HTmlColorToTBGRAPixel(Spaces2Zeros(edBackgroundColor.Text),
      BGRA(255, 255, 255, 255)));
  wType := SQLQuery1.FieldByName('data_type').AsString;
  if (wType <> 'tiles') and (wType <> 'features') then
  begin
    ShowMessage('Draw not implemented for this table type');
    Exit;
  end;
  wOldCursor := Screen.Cursor;
  SCreen.Cursor := crHourGlass;
  Drawer.Free;
  Drawer := nil;
  SetupDrawer(aDrawer);
  Drawer := aDrawer;
  if not aDrawer.IsProjValid then
  begin
    ShowMessage('Error creating proj. Invalid string?');
    Exit;
  end;
  try
    if cbClearBitmap.Checked and cbPreFillOceans.Checked then
    begin
      wColor := HTmlColorToTBGRAPixel(edOceanColor.Text, BGRA(180, 225, 250));
      if aDrawer.FRadio > 0 then
        BitmapMapa.EllipseAntialias(aDrawer.FCenter.x, aDrawer.FCenter.y,
          aDrawer.FRadio, aDrawer.FRadio,
          BGRA(0, 0, 0), 1, wColor)
      else
        aDrawer.DrawFilledGrid(wColor, wColor, di10, 1);
    end;
    graphic.Pos0.Y := aDrawer.LatOrigin;
    graphic.Pos0.X := aDrawer.LonOrigin;
    graphic.X0 := aDrawer.XOrigin;
    graphic.Y0 := aDrawer.YOrigin;
    graphic.ZoomLevel := seZoom.Value;

    if wType = 'tiles' then
    begin
      aDrawer.DrawTiles(geopackagewkb, SQLQuery1.FieldByName(
        'table_name').AsString, seOpacity.Value);
    end
    else
    begin // wType='features'
      aDrawer.DrawFeatures(geopackagewkb, SQLQuery1.FieldByName(
        'table_name').AsString, cbLabelField.Text, edFilter.Text);
      aDrawer.DrawLabels;
    end;
    if cbGrid.ItemIndex > 0 then
      aDrawer.DrawGrid(BGRA(127, 127, 127), GridInterval[cbGrid.ItemIndex], 1.5);
  finally
    //aDrawer.Free; //On FormDestroy
    Screen.Cursor := wOldCursor;
  end;
  tbsGeo.ActivePageIndex := 1;
end;

{$IFDEF USE_PROJ7}
procedure TForm1.btnDrawMapProjClick(Sender: TObject);
var
  wProjectionString: string;
  wP: integer;
begin
  wProjectionString := cbProjection.Text;
  //{name of the projection} +proj ...    or +proj ...
  wP := Pos('}', wProjectionString);
  if wP > 0 then
  begin
    Inc(wP);
    while (wP < length(wProjectionString)) and (wProjectionString[wP] = ' ') do
      Inc(wP);
    wProjectionString := Copy(wProjectionString, wP, length(wProjectionString) - wP + 1);
  end;
  DrawMap(TGeoPackageDrawerProj.Create(BitmapMapa, seZoom.Value,
    seLat.Value, seLon.Value, wProjectionString));
end;

{$ELSE}
procedure TForm1.btnDrawMapProjClick(Sender: TObject);
begin
  //empty procedure.
end;
{$ENDIF}

procedure TForm1.btnFilterClick(Sender: TObject);
var
  wQuery: string;
  wFilter: string;
begin
  wQuery := 'select * from ' + SQLQuery1.FieldByName('table_name').AsString;
  wFilter := Trim(edFilter.Text);
  if wFilter <> '' then
    wQuery := wQuery + ' where ' + wFilter;
  SQLQuerySlave.Active := False;
  SQLQuerySlave.SQL.Clear;
  SQLQuerySlave.SQL.Add(wQuery);
  SQLQuerySlave.Active := True;
end;

procedure TForm1.btnQueryExecClick(Sender: TObject);
begin
  SQLQueryDB.Active := False;
  SQLQueryDB.SQL.Clear;
  SQLQueryDB.SQL.Add(edQuery.Text);
  SQLQueryDB.Active := True;
end;

procedure TForm1.btnResizeClick(Sender: TObject);
begin
  BitmapMapa.Free;
  BitmapMapa := TBGRABitmap.Create(seBitmapWidth.Value, seBitmapHeight.Value, clWhite);
  ScrollBox1.VertScrollBar.Range := BitmapMapa.Height - ScrollBox1.Height;
  ScrollBox1.HorzScrollBar.Range := BitmapMapa.Width - ScrollBox1.Width;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  wSaveDialog: TSaveDialog;
begin
  wSaveDialog := TSaveDialog.Create(nil);
  try
    wSaveDialog.DefaultExt := 'png';
    wSaveDialog.Filter :=
      'png file (*.png)|*.png|bmp file(*.bmp)|*.bmp|jpeg file (*.jpg)|*.jpg|All (*.*)|*.*';
    wSaveDialog.FilterIndex := 0;
    if wSaveDialog.Execute then
      BitmapMapa.SaveToFile(wSaveDialog.Filename);
  finally
    wSaveDialog.Free;
  end;
end;

procedure TForm1.btnSaveSettingsClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TForm1.btnSetOriginClick(Sender: TObject);
begin
  seLat.Value := SQLQuery1.FieldByName('max_y').AsFloat;
  seLon.Value := SQLQuery1.FieldByName('min_x').AsFloat;
end;


function EscapeString(const AValue: string): string;
const
  ESCAPE = '\';
  QUOTATION_MARK = '"';
  REVERSE_SOLIDUS = '\';
  SOLIDUS = '/';
  BACKSPACE = #8;
  FORM_FEED = #12;
  NEW_LINE = #10;
  CARRIAGE_RETURN = #13;
  HORIZONTAL_TAB = #9;
var
  AChar: char;
  wL, wP: integer;
  wNewStr: string;

  procedure AddEscapedChar(aC: char);
  begin
    wNewStr[wP] := ESCAPE;
    Inc(wP);
    wNewStr[wP] := aC;
    Inc(wP);
  end;

  procedure AddChar(aC: char);
  begin
    wNewStr[wP] := aC;
    Inc(wP);
  end;

  procedure AddString(const aStr: string);
  var
    wC: char;
  begin
    for wC in aStr do
      AddChar(wC);
  end;

begin
  Result := '';
  wL := Length(aValue);
  if wL = 0 then
    Exit;
  SetLength(wNewStr, wL * 5);  // uXXXX 5 max replaced length for char.
  wP := 1;
  for AChar in AValue do
  begin
    case AChar of
      QUOTATION_MARK: AddEscapedChar(QUOTATION_MARK);
      REVERSE_SOLIDUS: AddEscapedChar(REVERSE_SOLIDUS);
      SOLIDUS: AddEscapedChar(SOLIDUS);
      BACKSPACE: AddEscapedChar('b');
      FORM_FEED: AddEscapedChar('f');
      NEW_LINE: AddEscapedChar('n');
      CARRIAGE_RETURN: AddEscapedChar('r');
      HORIZONTAL_TAB: AddEscapedChar('t');
      else
      begin
        if (integer(AChar) < 32) {or (Integer(AChar) > 126)} then
        begin
          AddEscapedChar('u');
          AddString(IntToHex(integer(AChar), 4));
        end
        else
          AddChar(AChar);
      end;
    end;
  end;
  SetLength(wNewStr, wP - 1);
  Result := wNewStr;
end;

procedure TForm1.btnToGeoJsonClick(Sender: TObject);
var
  fstream: TStream;
  fdata: TMemoryStream;
  wToGJ: wkb2GeoJson;
  wGeoJson: string;
  wI: integer;
  wFieldName: string;
  wFieldType: TFieldType;
  wLastField: integer;
  wOldDS: char;
begin
  if SQLQuerySlave.BOF and SQLQuerySlave.EOF then
    Exit;
  if SQLQuerySlave.FindField('geom')=nil then
    Exit;
  if not (tblobfield(SQLQuerySlave.FieldByName('geom')).IsNull) then
  begin
    fstream := SQLQuerySlave.CreateBlobStream(SQLQuerySlave.FieldByName('geom'), bmread);
    fdata := TMemoryStream.Create;
    try
      fdata.LoadFromStream(fstream);
      wGeoJson := '{' + LineEnding;
      wGeoJson := wGeoJson + '  "type": "FeatureCollection",' + LineEnding;
      wGeoJson := wGeoJson + '  "features": [' + LineEnding;
      //-----
      wGeoJson := wGeoJson + '{' + LineEnding;
      wGeoJson := wGeoJson + '"type": "Feature",' + LineEnding;
      wToGJ.GeometryToGeoJson(fdata.Memory, fdata.Size, 2);
      wGeoJson := wGeoJson + wToGJ.FGJString;
      wGeoJson := wGeoJson + ',' + LineEnding;
      wGeoJson := wGeoJson + '"properties": {' + LineEnding;
      wLastField := SQLQuerySlave.Fields.Count - 1;
      wOldDS := FormatSettings.DecimalSeparator;
      FormatSettings.DecimalSeparator := '.';

      for wI := 0 to wLastField do
      begin
        wFieldName := LowerCase(SQLQuerySlave.Fields[wI].FieldName);
        wFieldType := SQLQuerySlave.Fields[wI].DataType;
        if wFieldName <> 'geom' then
        begin
          wGeoJson := wGeoJson + '  "' + wFieldName + '": ';
          if wFieldType in [ftUnknown, ftBlob, ftGraphic, ftFmtMemo,
            ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftOraBlob,
            ftOraClob, ftVariant, ftInterface, ftIDispatch] then
            continue;
          if wFieldType in [ftSmallint, ftInteger, ftWord, ftBoolean,
            ftFloat, ftCurrency, ftBCD, ftAutoInc, ftLargeInt, ftFMTBcd] then
            wGeoJson := wGeoJson + SQLQuerySlave.Fields[wI].AsString
          else
            wGeoJson := wGeoJson + '"' + EscapeString(SQLQuerySlave.Fields[wI].AsString) + '"';
          if wI < wLastField then
            wGeoJson := wGeoJson + ',' + LineEnding;
        end;
      end;
      FormatSettings.DecimalSeparator := wOldDS;
      wGeoJson := wGeoJson + '}' + LineEnding;
      wGeoJson := wGeoJson + '}' + LineEnding;
      //-----
      wGeoJson := wGeoJson + ']' + LineEnding;
      wGeoJson := wGeoJson + '}' + LineEnding;

      Clipboard.AsText := wGeoJson;
      //ShowMessage(wGeoJson);

    finally
      fstream.Free;
      fdata.Free;
    end;
  end;
end;

procedure TForm1.cbTablesChange(Sender: TObject);
begin
  edQuery.Text := 'select * from ' + cbTables.Text;
  btnQueryExecClick(Self);
end;

procedure TForm1.edBackgroundColorEditingDone(Sender: TObject);
begin
  btnBackgroundColor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edBackgroundColor.Text));
end;

procedure TForm1.btnBackgroundColorColorChanged(Sender: TObject);
begin
  edBackgroundColor.Text := BGRAPixelToHTml(btnBackgroundColor.ButtonColor);
end;

procedure TForm1.edFillColorEditingDone(Sender: TObject);
begin
  btnFillcolor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edFillColor.Text));
end;

procedure TForm1.btnFillColorColorChanged(Sender: TObject);
begin
  edFillColor.Text := BGRAPixelToHTml(btnFillColor.ButtonColor);
end;

procedure TForm1.btnLineColorColorChanged(Sender: TObject);
begin
  edLineColor.Text := BGRAPixelToHTml(btnLineColor.ButtonColor);
end;

procedure TForm1.btnOceanColorColorChanged(Sender: TObject);
begin
  edOceanColor.Text := BGRAPixelToHTml(btnOceanColor.ButtonColor);
end;

procedure TForm1.edLineColorEditingDone(Sender: TObject);
begin
  btnLineColor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edLineColor.Text));
end;

procedure TForm1.edOceanColorEditingDone(Sender: TObject);
begin
  btnOceanColor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edOceanColor.Text));
end;

procedure TForm1.FileNameEdit1AcceptFileName(Sender: TObject; var Value: string);
begin
  Open(Value);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Drawer := nil;
  pnlStyle.Visible := False;
  LoadSettings;
  graphic.Pos0.Y := 0;
  graphic.Pos0.X := 0;
  graphic.X0 := -1;
  graphic.Y0 := -1;
  graphic.ZoomLevel := -1;
  BitmapMapa := TBGRABitmap.Create(seBitmapWidth.Value, seBitmapHeight.Value, clWhite);
  ScrollBox1.VertScrollBar.Range := BitmapMapa.Height - ScrollBox1.Height;
  ScrollBox1.HorzScrollBar.Range := BitmapMapa.Width - ScrollBox1.Width;
  ScrollBox1.VertScrollBar.Increment := 60;
  ScrollBox1.VertScrollBar.Page := 240;
  ScrollBox1.HorzScrollBar.Increment := 60;
  ScrollBox1.HorzScrollBar.Page := 240;
  geopackagewkb := TGeoPackage.Create;
  {$IFDEF USE_PROJ7}
  ProjInit(projDefaultPath);  //default path in windows.
  {$ELSE}
     lbProjection.visible:=false;
     cbProjection.visible:=false;
     btnDrawMapProj.Visible:=false;
  {$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Drawer.Free;
  geopackagewkb.Free;
  BitmapMapa.Free;
  {$IFDEF USE_PROJ7}
  ProjFinalize;
  {$ENDIF}
end;

procedure TForm1.lbProjLibClick(Sender: TObject);
begin
  OpenUrl('https://proj.org/');
end;

procedure TForm1.lbBgraBitmapClick(Sender: TObject);
begin
  OpenUrl('https://wiki.freepascal.org/BGRABitmap');
end;

procedure TForm1.lbRepoUrlClick(Sender: TObject);
begin
  OpenUrl(TLabel(Sender).Caption);
end;

procedure TForm1.lbSQLiteClick(Sender: TObject);
begin
  OpenUrl('https://www.sqlite.org/index.html');
end;

procedure TForm1.lbStyleClick(Sender: TObject);
begin
  pnlStyle.Visible := not pnlStyle.Visible;
end;

procedure TForm1.miCopyToGeoJsonClick(Sender: TObject);
begin
  btnToGeoJsonClick(Self);
end;

procedure TForm1.miDrawClick(Sender: TObject);
var
  fstream: TStream;
  fdata: TMemoryStream;
begin
  if SQLQuerySlave.BOF and SQLQuerySlave.EOF then
    Exit;
  if SQLQuery1.FieldByName('data_type').AsString <> 'features' then
    Exit;
  if Drawer = nil then
    Exit;
  fstream := nil;
  fdata := nil;
  if SQLQuerySlave.FindField('geom')=nil then
    Exit;
  if not (tblobfield(SQLQuerySlave.FieldByName('geom')).IsNull) then
  begin
    fstream := SQLQuerySlave.CreateBlobStream(SQLQuerySlave.FieldByName('geom'), bmread);
    fdata := TMemoryStream.Create;
    try
      fdata.LoadFromStream(fstream);
      SetupDrawer(Drawer);
      Drawer.Draw(fdata.Memory, fdata.Size);
      if cbLabelField.Text <> '' then
      begin
        Drawer.ClearLabels;
        Drawer.AddLabel(SQLQuerySlave.FieldByName(cbLabelField.Text).AsString,
          0, Drawer.FontHeigth);
        Drawer.DrawLabels;
      end;
    finally
      fstream.Free;
      fdata.Free;
    end;
    tbsGeo.ActivePageIndex := 1;
  end;
end;

procedure TForm1.miSetOriginClick(Sender: TObject);
var
  wLat, wLon: double;
  X,Y:integer;
  wPoint:TPoint;
begin
  if graphic.X0 <> -1 then
  begin
    if Drawer <> nil then
    begin
      wPoint:=ScrollBox1.ScreenToClient(PopupMenuGraphic.PopupPoint);
      if Drawer.PixelXYToCoord(graphic.X0 + wPoint.X, graphic.Y0 + wPoint.Y, wLat, wLon) then
      begin
        //ShowMessage('Lat: ' + FormatFloat('0.00000', wLat) + ' Lon: ' + FormatFloat('0.00000', wLon))
        seLat.Value:=wLat;
        seLon.Value:=wLon;
      end
      else
      begin
        //ShowMessage('Lat: inf Lon: inf');
      end;
    end;
  end;
end;


procedure TForm1.ScrollBox1Paint(Sender: TObject);
begin
  BitmapMapa.Draw(ScrollBox1.Canvas, 0, 0);
end;

procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);
begin
  edFilter.Text := '';
  SQLQuerySlave.Active := False;
  SQLQuerySlave.SQL.Clear;
  SQLQuerySlave.SQL.Add('select * from ' + SQLQuery1.FieldByName('table_name').AsString);
  SQLQuerySlave.Active := True;
  FillLabelFieldNames;
end;

procedure TForm1.FillTablasComboBox;
var
  fSltb: TSQLiteTable;
begin
  cbTables.Items.Clear;
  fSltb := geopackagewkb.Database.GetTable(
    'SELECT name FROM sqlite_master WHERE type =''table'' AND name NOT LIKE ''sqlite_%''');
  try
    cbTables.Items.Clear;
    while fSltb.EOF = False do
    begin
      cbTables.Items.Add(fSltb.FieldAsString(0));
      fSltb.Next;
    end;
  finally
    fSltb.Free;
  end;
  cbTables.ItemIndex := 0;
end;

procedure TForm1.FillLabelFieldNames;
var
  wI: integer;
  wDefault: integer;
  wFK: TFieldType;
begin
  cbLabelField.Items.Clear;
  cbLabelField.Items.Add('');
  wDefault := 0;
  for wI := 0 to Pred(SQLQuerySlave.Fields.Count) do
  begin
    wFK := SQLQuerySlave.Fields[wI].DataType;
    if wFK in [ftString, ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,
      ftTime, ftDateTime, ftAutoInc, ftMemo, ftFmtMemo, ftFixedChar,
      ftWideString, ftLargeint, ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo] then
      cbLabelField.Items.Add(SQLQuerySlave.Fields[wI].FieldName);
  end;
  cbLabelField.ItemIndex := wDefault;
end;

procedure TForm1.PaintBox1MouseLeave(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  wLat, wLon: double;
begin
  if graphic.X0 <> -1 then
  begin
    if Drawer <> nil then
    begin
      if Drawer.PixelXYToCoord(graphic.X0 + X, graphic.Y0 + Y, wLat, wLon) then
        StatusBar1.Panels[0].Text := 'Lat: ' + FormatFloat('0.00000', wLat) + ' Lon: ' + FormatFloat('0.00000', wLon)
      else
        StatusBar1.Panels[0].Text := 'Lat: inf Lon: inf';
    end;
  end;
end;

const
  CONFIGURATION_FILE = 'geopackageviewer.xml';
  DEF_BACKGROUND_COLOR = '#FFFFFFFF';
  DEF_LINE_COLOR = '#FF0000FF';
  DEF_FILL_COLOR = '#00FF00FF';
  DEF_OCEAN_COLOR = '#B4E1FAFF';

procedure TForm1.SaveSettings;
var
  Config: TXMLConfig;
const
  Version = 1;
begin
  try
    Config := TXMLConfig.Create(CONFIGURATION_FILE);
    try
      // store the version number so future extensions can handle old config files
      Config.SetDeleteValue('Version', Version, 0);
      // store string variable "SomeValue"
      // if SomeValue has the default value the entry is not stored,
      // so only the differences to the default are stored.
      // This way the xml is kept short and defaults may change in future.
      Config.SetDeleteValue('ProjDefaultPath', projDefaultPath, '');
      Config.SetDeleteValue('DefaultFile', FilenameEdit1.Text, '');
      Config.SetDeleteValue('Filter', edFilter.Text, '');
      Config.SetDeleteValue('ZoomLevel', seZoom.Value, 2);
      Config.SetDeleteExtendedValue('Lat', seLat.Value, 85);
      Config.SetDeleteExtendedValue('Lon', seLon.Value, -180.0);
      Config.SetDeleteValue('FontHeight', seFontHeight.Value, 16);
      Config.SetDeleteValue('FillColor', edFillColor.Text, DEF_FILL_COLOR);
      Config.SetDeleteValue('LineColor', edLineColor.Text, DEF_LINE_COLOR);
      Config.SetDeleteValue('BackgroundColor', edBackgroundColor.Text, DEF_FILL_COLOR);
      Config.SetDeleteValue('PreFillOceans', cbPreFillOceans.Checked, False);
      Config.SetDeleteValue('OceanColor', edOceanColor.Text, DEF_OCEAN_COLOR);
      Config.SetDeleteValue('TilesOpacity', seOpacity.Value, 255);
      Config.SetDeleteExtendedValue('LineWidth', seLineWidth.Value, 2);
      Config.SetDeleteExtendedValue('PointRadius', sePointRadius.Value, 3);
      Config.SetDeleteValue('BitmapWidth', seBitmapWidth.Value, 1220);
      Config.SetDeleteValue('BitmapHeight', seBitmapHeight.Value, 550);
      Config.SetDeleteValue('GridItemIndex', cbGrid.ItemIndex, 0);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
    end;
  end;
end;

procedure TForm1.LoadSettings;
var
  Config: TXMLConfig;
  Version: integer;
begin
  try
    Config := TXMLConfig.Create(CONFIGURATION_FILE);
    try
      Version := Config.GetValue('Version', 1);
      projDefaultPath := Config.GetValue('ProjDefaultPath', 'C:\OSGeo4W\share\proj');
      FileNameEdit1.Text := Config.GetValue('DefaultFile', '');
      edFilter.Text := Config.GetValue('Filter', '');
      seZoom.Value := Config.GetValue('ZoomLevel', 2);
      seLat.Value := Config.GetExtendedValue('Lat', 85);
      seLon.Value := Config.GetExtendedValue('Lon', -180);
      seFontHeight.Value := Config.GetValue('FontHeight', 16);
      edFillColor.Text := Config.GetValue('FillColor', DEF_FILL_COLOR);
      btnFillColor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edFillColor.Text));
      edLineColor.Text := Config.GetValue('LineColor', DEF_LINE_COLOR);
      btnLineColor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edLineColor.Text));
      edBackgroundColor.Text := Config.GetValue('BackgroundColor', DEF_BACKGROUND_COLOR);
      btnBackgroundColor.ButtonColor :=
        HTmlColorToTColor(Spaces2Zeros(edBackgroundColor.Text));
      cbPreFillOceans.Checked := Config.GetValue('PreFillOceans', True);
      edOceanColor.Text := Config.GetValue('OceanColor', DEF_OCEAN_COLOR);
      btnBackgroundColor.ButtonColor := HTmlColorToTColor(Spaces2Zeros(edBackgroundColor.Text));
      seOpacity.Value := Config.GetValue('TilesOpacity', 255);
      seLineWidth.Value := Config.GetExtendedValue('LineWidth', 2);
      sePointRadius.Value := Config.GetExtendedValue('PointRadius', 3);
      seBitmapWidth.Value := Config.GetValue('BitmapWidth', 1220);
      seBitmapHeight.Value := Config.GetValue('BitmapHeight', 550);
      cbGrid.ItemIndex := Config.GetValue('GridItemIndex', 0);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
    end;
  end;
end;

end.
