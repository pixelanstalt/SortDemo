unit SortTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDataArray = array of Integer;
  PDataArray = ^TDataArray;
  TDataArraySize = Integer;

  TGetSortingAlgorithmName = function(): PChar; stdcall;
  TDoSort = procedure(DataArray: PDataArray; DataArraySize: TDataArraySize); stdcall;

implementation

end.

