library BubbleSort;

{$mode objfpc}{$H+}

uses
  Classes, SortTypes in '../../sorttypes.pas';

function GetSortingAlgorithmName: PChar; stdcall;
begin
  Result := PChar('Bubblesort');
end;

procedure DoSort(DataArray: PDataArray; Size: TDataArraySize); stdcall;
var
  intLoop, intHigh: Integer;
  intTemp: Integer;
begin
  intHigh := Size-2;
  while intHigh > 0 do
  begin
    for intLoop := 0 to intHigh do
    begin
      if DataArray^[intLoop] > DataArray^[intLoop+1] then
      begin
        intTemp := DataArray^[intLoop];
        DataArray^[intLoop] := DataArray^[intLoop+1];
        DataArray^[intLoop+1] := intTemp;
      end;
    end;
    Dec(intHigh);
  end;
end;

exports
  GetSortingAlgorithmName, DoSort;
end.

