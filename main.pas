unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SortTypes, DynLibs, LazFileUtils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRandomize: TButton;
    btnStartLibAlgorithm: TButton;
    cbSortingAlgorithms: TComboBox;
    PaintBox: TPaintBox;
    procedure btnRandomizeClick(Sender: TObject);
    procedure btnStartLibAlgorithmClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FDataArray: TDataArray;
    FSortingAlgorithmFilenames: array of String;

    procedure DrawArray;
    procedure EnumerateAlgorithmsFromSharedLibraries;
    function GetSortingAlgorithmName(Filename: String;
      var AlgorithmName: String): Boolean;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnRandomizeClick(Sender: TObject);
var
  intLoop: Integer;
begin
  for intLoop := 0 to High(FDataArray) do
    FDataArray[intLoop] := Random(300);

  DrawArray;
end;

procedure TfrmMain.btnStartLibAlgorithmClick(Sender: TObject);
var
  LibHandle: TLibHandle = NilHandle;
  DoSort: TDoSort = nil;
begin
  if cbSortingAlgorithms.ItemIndex > -1 then
  begin
    LibHandle := LoadLibrary(
      FSortingAlgorithmFilenames[cbSortingAlgorithms.ItemIndex]);
    if LibHandle <> NilHandle then
    begin
      Pointer(DoSort) := GetProcAddress(LibHandle, 'DoSort');
      if @DoSort <> nil then
        DoSort(@FDataArray, Length(FDataArray));
      UnloadLibrary(LibHandle);
    end;
    PaintBox.Update;
  end
  else
    MessageDlg('No algorithm selected', mtError, [mbOk], 0);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(FDataArray, 1000);
  Randomize;
  EnumerateAlgorithmsFromSharedLibraries;
end;

procedure TfrmMain.PaintBoxPaint(Sender: TObject);
begin
  DrawArray;
end;

procedure TfrmMain.DrawArray;
var
  intLoop: Integer;
begin
  PaintBox.Canvas.Pen.Color := clBlack;
  PaintBox.Canvas.Rectangle(1, 1, 1004, 305);
  PaintBox.Canvas.Pen.Color := clBlue;
  for intLoop := 0 to High(FDataArray) do
  begin
    PaintBox.Canvas.Line(intLoop+2, 303, intLoop+2, 303-FDataArray[intLoop]);
  end;
end;

procedure TfrmMain.EnumerateAlgorithmsFromSharedLibraries;
var
  SR: TSearchRec;
  AlgorithmName: String;
begin
  if FindFirstUTF8('algorithms/*.' + SharedSuffix, faAnyFile, SR) = 0 then
  repeat
    if (SR.Name = '.') or (SR.Name = '..') then
      Continue;

    if GetSortingAlgorithmName('algorithms/' + SR.Name, AlgorithmName) then
    begin
      SetLength(FSortingAlgorithmFilenames,
        Length(FSortingAlgorithmFilenames)+1);
      FSortingAlgorithmFilenames[High(FSortingAlgorithmFilenames)] :=
        'algorithms/' + SR.Name;
      cbSortingAlgorithms.Items.Add(AlgorithmName);
    end;
  until FindNextUTF8(SR) <> 0;
  FindClose(SR);
end;

function TfrmMain.GetSortingAlgorithmName(Filename: String;
  var AlgorithmName: String): Boolean;
var
  LibHandle: TLibHandle = NilHandle;
  GetAlgorithmName: TGetSortingAlgorithmName = nil;
begin
  Result := False;
  LibHandle := LoadLibrary(Filename);
  if LibHandle <> NilHandle then
  begin
    Pointer(GetAlgorithmName) := GetProcAddress(LibHandle,
      'GetSortingAlgorithmName');
    if @GetAlgorithmName <> nil then
    begin
      AlgorithmName := GetAlgorithmName();
      Result := True;
    end;
    UnloadLibrary(LibHandle);
  end;
end;

end.

