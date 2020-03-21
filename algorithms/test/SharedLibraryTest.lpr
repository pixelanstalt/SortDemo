program SharedLibraryTest;

{$mode objfpc}{$H+}

uses
  Classes, TestFramework, TextTestRunner, DynLibs, LazFileUtils,
  SortTypes in '../../SortTypes.pas', SysUtils;

type

  { TLibraryTest }

  TLibraryTest = class(TTestCase)
  private
    FLibHandle: TLibHandle;
    FDataArray, FReferenceArray: TDataArray;

    procedure SetupArrays(Size: Integer);
    procedure CheckForAscendingValues;
    procedure CheckArrays;
    procedure ExecuteSort;
  published
    procedure SetUpOnce; override;
    procedure CheckMethods;
    procedure CheckArray1000;
    procedure CheckArray100000;
    procedure TearDownOnce; override;
  end;

{ TLibraryTest }

procedure TLibraryTest.SetupArrays(Size: Integer);
var
  intLoop: Integer;
begin
  SetLength(FDataArray, Size);
  SetLength(FReferenceArray, Size);

  for intLoop := 0 to High(FDataArray) do
  begin
    FDataArray[intLoop] := Int64(Low(Integer)) + Random(-Low(Integer) +
      Int64(High(Integer)) + 1);
    FReferenceArray[intLoop] := FDataArray[intLoop];
  end;
end;

procedure TLibraryTest.CheckForAscendingValues;
var
  intLoop: Integer;
begin
  for intLoop := 1 to High(FDataArray) do
    Check(FDataArray[intLoop-1] <= FDataArray[intLoop],
    Format('Failed check for ascending values at indexes %d and %d ' +
      '(Values %d and %d)', [intLoop-1, intLoop, FDataArray[intLoop-1],
      FDataArray[intLoop]]));
end;

procedure TLibraryTest.CheckArrays;
var
  FCheckArray: array of Boolean;
  boolFoundThisLoop: Boolean;
  intDataArrayLoop, intReferenceArrayLoop: Integer;
begin
  SetLength(FCheckArray, Length(FDataArray));
  FillByte(FCheckArray[0], Length(FCheckArray), Byte(False));

  for intDataArrayLoop := 0 to High(FDataArray) do
  begin
    boolFoundThisLoop := False;
    for intReferenceArrayLoop := 0 to High(FReferenceArray) do
    begin
      if FDataArray[intDataArrayLoop] =
        FReferenceArray[intReferenceArrayLoop] then
      begin
        if FCheckArray[intDataArrayLoop] then
          Continue
        else
        begin
          FCheckArray[intDataArrayLoop] := True;
          boolFoundThisLoop := True;
          Break;
        end;
      end;
    end;
    Check(boolFoundThisLoop);
  end;

  for intDataArrayLoop := 0 to High(FCheckArray) do
    Check(FCheckArray[intDataArrayLoop]);
end;

procedure TLibraryTest.ExecuteSort;
var
  DoSort: TDoSort;
begin
  Pointer(DoSort) := GetProcAddress(FLibHandle, 'DoSort');
  Check(Pointer(DoSort) <> nil);
  DoSort(@FDataArray, Length(FDataArray));
end;

procedure TLibraryTest.SetUpOnce;
begin
  Randomize;

  if not FileExistsUTF8(ParamStr(1)) then
  begin
    Fail('Shared Library not found');
    Halt;
  end;

  FLibHandle := LoadLibrary(ParamStr(1));

  Check(FLibHandle <> NilHandle);
end;

procedure TLibraryTest.CheckMethods;
begin
  Check(GetProcAddress(FLibHandle, 'GetSortingAlgorithmName') <> nil);
  Check(GetProcAddress(FLibHandle, 'DoSort') <> nil);
  Check(GetProcAddress(FLibHandle, '__DieseFunktionWirdNichtExistieren') = nil);
end;

procedure TLibraryTest.CheckArray1000;
begin
  SetupArrays(1000);
  Check(Length(FDataArray) = 1000);
  Check(Length(FReferenceArray) = 1000);
  ExecuteSort;
  CheckForAscendingValues;
  CheckArrays;
end;

procedure TLibraryTest.CheckArray100000;
begin
  SetupArrays(100000);
  Check(Length(FDataArray) = 100000);
  Check(Length(FReferenceArray) = 100000);
  ExecuteSort;
  CheckForAscendingValues;
  CheckArrays;
end;

procedure TLibraryTest.TearDownOnce;
begin
  Check(UnloadLibrary(FLibHandle));
end;

begin
  TestFramework.RegisterTest(TLibraryTest.Suite);
  RunRegisteredTests;
end.

