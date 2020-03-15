unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SortTypes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRandomize: TButton;
    btnStartBubbleSort: TButton;
    btnStartInsertionSort: TButton;
    PaintBox: TPaintBox;
    procedure btnRandomizeClick(Sender: TObject);
    procedure btnStartBubbleSortClick(Sender: TObject);
    procedure btnStartInsertionSortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FDataArray: TDataArray;

    procedure DrawArray;
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

procedure TfrmMain.btnStartBubbleSortClick(Sender: TObject);
var
  intLoop, intHigh: Integer;
  intTemp: Integer;
begin
  intHigh := High(FDataArray)-1;
  while intHigh > 0 do
  begin
    for intLoop := 0 to intHigh do
    begin
      if FDataArray[intLoop] > FDataArray[intLoop+1] then
      begin
        intTemp := FDataArray[intLoop];
        FDataArray[intLoop] := FDataArray[intLoop+1];
        FDataArray[intLoop+1] := intTemp;
      end;
      if intLoop mod 700 = 0 then
        PaintBox.Update;
    end;
    Dec(intHigh);
  end;
  PaintBox.Update;
end;

procedure TfrmMain.btnStartInsertionSortClick(Sender: TObject);
var
  intLoop, intInsert, intInnerLoop: Integer;
begin
  for intLoop := 1 to High(FDataArray)-1 do
  begin
    intInsert := FDataArray[intLoop];

    intInnerLoop := intLoop;

    while (intInnerLoop > 0) and (FDataArray[intInnerLoop-1] > intInsert) do
    begin
      FDataArray[intInnerLoop] := FDataArray[intInnerLoop-1];
      Dec(intInnerLoop);
    end;

    FDataArray[intInnerLoop] := intInsert;

    PaintBox.Update;
  end;
  PaintBox.Update;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(FDataArray, 1000);
  Randomize;
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

end.

