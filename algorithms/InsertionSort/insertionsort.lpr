library insertionsort;

{$mode objfpc}{$H+}

uses
  Classes, SortTypes in '../../sorttypes.pas';

function GetSortingAlgorithmName: PChar; stdcall;
begin
  Result := PChar('Insertionsort');
end;

procedure DoSort(DataArray: PDataArray; Size: TDataArraySize); stdcall;
var
  intLoop, intInsert, intInnerLoop: Integer;
begin
  for intLoop := 1 to Size-2 do
  begin
    intInsert := DataArray^[intLoop];

    intInnerLoop := intLoop;

    while (intInnerLoop > 0) and (DataArray^[intInnerLoop-1] > intInsert) do
    begin
      DataArray^[intInnerLoop] := DataArray^[intInnerLoop-1];
      Dec(intInnerLoop);
    end;

    DataArray^[intInnerLoop] := intInsert;
  end;
end;

exports
  GetSortingAlgorithmName, DoSort;
end.

