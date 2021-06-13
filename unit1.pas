unit Unit1;

{
Data used in this test downloaded from.
https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_ESP_gpkg.zip

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  SQLiteWrap, geopackagewkb, geopackagedrawer, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnOpen: TButton;
    btnDrawer: TButton;
    btnHighLevel: TButton;
    cbTablas: TComboBox;
    cbCampos: TComboBox;
    cbShowLabels: TCheckBox;
    edFiltro: TEdit;
    edFields: TEdit;
    edGeopackage: TEdit;
    seLat: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    seLon: TFloatSpinEdit;
    seZoom: TSpinEdit;
    procedure btnDrawerClick(Sender: TObject);
    procedure btnHighLevelClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure cbTablasSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    geoPackage: TGeoPackage;
    BitmapMapa: TBGRABitmap;
  end;

const
  FILENAME = 'C:\0Temporal\gadm36_ESP_gpkg\gadm36_ESP.gpkg';
  FILENAME2 = 'C:\0Temporal\natural_earth_vector.gpkg';

var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.btnOpenClick(Sender: TObject);
var
  fSltb: TSQLiteTable;
  wcActual: string;
  wcTableName: string;
begin
  geoPackage.Open(edGeopackage.Text);
  //MOSTRAR TABLAS DATOS.
  fSltb := geoPackage.Database.GetTable(
    'select table_name,data_type,identifier,description,last_change,min_x,min_y,max_x,max_y,srs_id from gpkg_contents');
  try
    cbTablas.Items.Clear;
    while fSltb.EOF = False do
    begin
      wcTableName := fSltb.FieldAsString(0);
      cbTablas.Items.Add(wcTableName);
      wCActual := fSltb.FieldAsString(2);
      Memo1.Lines.Add(wCActual);
      fSltb.Next;
    end;
    cbTablas.ItemIndex := 1;//0;
  finally
    fSltb.Free;
  end;
  cbTablasSelect(Self);

end;

//Low level drawing
procedure TForm1.btnDrawerClick(Sender: TObject);
var
  wDrawer: TGeoPackageDrawer;
  fSltb: TSQLiteTable;
  wcTableName: string;
  wPtr: pbyte;
  wSize: integer;
  wQueryString: string;
  wId: integer;
begin
  wDrawer := TGeoPackageDrawer.Create(BitmapMapa, seZoom.Value, seLat.Value, seLon.Value);

  BitmapMapa.Fill(clWhite);

  wQuerySTring := 'select ' + edFields.Text + ' from ' + cbTablas.Text;
  if Trim(edFiltro.Text) <> '' then
    wQueryString := wQueryString + ' where ' + edFiltro.Text;
  fSltb := geoPackage.Database.GetTable(wQueryString);
  try
    Memo1.Lines.Add('------------------------');
    while fSltb.EOF = False do
    begin
      wcTableName := fSltb.FieldAsString(2);
      wId := fSltb.FieldAsInteger(0);
      Memo1.Lines.Add(fSltb.FieldAsString(0) + '- ' + wcTableName);

      wPtr := fSltb.FieldAsBlobPtr(1, wSize);
      if wId = 13 then
      begin
        wDrawer.FillColor := BGRA($FF, 0, 0);
      end
      else
        wDrawer.FillColor := BGRA(0, $FF, 0);
      if wId = 11 then
        wDrawer.FillColor := BGRA(0, 0, 0, 50);
      wDrawer.Draw(wPtr, wSize);
      if cbShowLabels.Checked then
      begin
        //wDrawer.AddLabel((wDrawer.MinEnvelope.y + wDrawer.MaxEnvelope.y) / 2,
        //  (wDrawer.MinEnvelope.x + wDrawer.MaxEnvelope.x) / 2,
        //  wcTableName, 0, 16);
        wDrawer.AddLabel(wcTableName, 0, 16);
      end;
      fSltb.Next;
    end;
  finally
    fSltb.Free;
  end;
  wDrawer.DrawLabels;
  BitmapMapa.Draw(Canvas, 380, 10);
  wDrawer.Free;
end;

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
    wDrawer.DrawLabels;    //draw labels at end.
    BitmapMapa.Draw(Canvas, 380, 10);
  finally
    wGeoPackage.Free;
    wDrawer.Free;
	//BitmapMapa.Free;
  end;
end;


procedure TForm1.cbTablasSelect(Sender: TObject);
var
  fSltb: TSQLiteTable;
begin
  fSltb := geoPackage.Database.GetTable('PRAGMA table_info(''' + cbTablas.Text + ''')');
  try
    cbCampos.Items.Clear;
    while fSltb.EOF = False do
    begin
      cbCampos.Items.Add(fSltb.FieldAsString(1) + ' --- ' + fSltb.FieldAsString(2));
      fSltb.Next;
    end;
  finally
    fSltb.Free;
  end;
  cbCampos.ItemIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  geoPackage := TGeopackage.Create(FILENAME);
  BitmapMapa := TBGRABitmap.Create(740, 550);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  geoPackage.Free;
  BitmapMapa.Free;
end;


end.
