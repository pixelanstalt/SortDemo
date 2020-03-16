unit SortTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDataArray = array of Integer;
  PDataArray = ^TDataArray;
  TDataArraySize = Integer;

  TGetSortingAlgorithmName = function(): PChar;
  TDoSort = procedure(DataArray: PDataArray; DataArraySize: TDataArraySize) of
    object;

implementation

end.

