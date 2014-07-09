unit TripCMP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants;

var
  NothingVariable: pointer;

function TripCompare(AChanged, AStoraged, AResult: TObject): boolean;
function TripCompareStr(AChanged: AnsiString; AStoraged: AnsiString; var AResult: variant): boolean;
function TripCompare(AChanged: variant; AStoraged: variant; var AResult: variant): boolean;
function TripCompareDateTime(AChanged: TDateTime; AStoraged: TDateTime; var AResult: variant): boolean;

function IsNothing(var AVar): boolean;
procedure SetNothing(var AVar);

implementation

function TripCompare(AChanged, AStoraged, AResult: TObject): boolean;
begin
  if pointer(AChanged) = pointer(AStoraged) then
  begin
    SetNothing(AResult);
    Result := False;
  end

  else
  begin
    pointer(AResult) := pointer(AChanged);
    Result := True;
  end;
end;

function TripCompareStr(AChanged: AnsiString; AStoraged: AnsiString; var AResult: variant): boolean;
begin
  if pointer(AChanged) = pointer(AStoraged) then
  begin
    SetNothing(AResult);
    Result := False;
  end
  else
  begin
    AResult := AChanged;
    Result := True;
  end;
end;

function TripCompare(AChanged: variant; AStoraged: variant; var AResult: variant): boolean;
begin
  if VarIsStr(AChanged) then
    Result := TripCompareStr(AChanged, AStoraged, AResult)
  else if (VarType(AChanged) = 7) or (VarType(AChanged) = 5) then
    Result := TripCompareDateTime(VarToDateTime(AChanged), VarToDateTime(AStoraged), AResult)
  else
    Result := TripCompare(AChanged, AStoraged, AResult);
end;

function TripCompareDateTime(AChanged: TDateTime; AStoraged: TDateTime; var AResult: variant): boolean;
begin
  if AChanged = AStoraged then
  begin
    SetNothing(AResult);
    Result := False;
  end
  else
  begin
    AResult := AChanged;
    Result := True;
  end;
end;

function IsNothing(var AVar): boolean;
begin
  Result := pointer(AVar) = NothingVariable;
end;

procedure SetNothing(var AVar);
begin
  pointer(AVar) := NothingVariable;
end;

initialization
  NothingVariable := AllocMem(1024);

end.
