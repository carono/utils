unit ArrayManager;

{$mode objfpc}{$H+}
interface

uses Classes;

type

  { TArrayManager }

  TArrayManager = class(TObject)
  protected
    function GetItem(index: Integer): TObject; virtual;
  private
    Items1: array of TObject;
    Position: Integer;
    FOwner1: TObject;
    function GetCount: Integer;
  public
    property Owner: TObject read FOwner1 write FOwner1;
    procedure Clear; virtual;
    property Count: Integer read GetCount;
    property Item[index: Integer]: TObject read GetItem;
    function Add(AObject: TObject): TObject; virtual;
    function GetItemIndex(AObject: TObject): Integer;
    function Extract(AObject: TObject): TObject;
    procedure Assign(ASource: TObject); virtual;
    function Current(): TObject; virtual;
    function Next(): TObject;
    procedure Reset();
    function Prev(): TObject;
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; virtual;
  end;


implementation

{ TRCArrayManager }

function TArrayManager.GetCount: Integer;
begin
  Result := Length(Items1);
end;

function TArrayManager.GetItem(index: Integer): TObject;
begin
  Result := Items1[index];
end;

procedure TArrayManager.Clear;
begin
  Position := -1;
  SetLength(Items1, 0);
end;

function TArrayManager.Add(AObject: TObject): TObject;
var
  AIndex1: Integer;
begin
  AIndex1 := GetItemIndex(AObject);
  if AIndex1 = -1 then
  begin
    SetLength(Items1, Count + 1);
    Items1[Count - 1] := AObject;
    Result := Items1[Count - 1];
  end
  else
    Result := AObject;
end;

function TArrayManager.GetItemIndex(AObject: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Item[i] = AObject then
    begin
      Result := i;
      break;
    end;
end;

function TArrayManager.Extract(AObject: TObject): TObject;
var
  i, j, c: Integer;
begin
  i := GetItemIndex(AObject);
  c := 0;
  if (i <> -1) then
    for j := 0 to Count - 1 do
    begin
      Items1[j] := Items1[c];
      if j <> i then
        Inc(c);
    end;
  SetLength(Items1, c);
end;

procedure TArrayManager.Assign(ASource: TObject);
begin
  raise TExceptionClass.Create('Assign method not implemented');
end;

function TArrayManager.Current: TObject;
begin
  if (Position = -1) then
    if (Count > 0) then
      Result := GetItem(0)
    else
      Result := nil
  else
    Result := GetItem(Position);
end;

function TArrayManager.Next: TObject;
begin
  Result := nil;
  if Count > 0 then
  begin
    Inc(Position);
    if Position < Count then
      Result := GetItem(Position);
  end;
end;

procedure TArrayManager.Reset;
begin
  Position := -1;
end;

function TArrayManager.Prev: TObject;
begin
  Result := nil;
end;

constructor TArrayManager.Create(AOwner: TObject);
begin
  FOwner1 := AOwner;
  Clear;
end;

destructor TArrayManager.Destroy;
begin
  Clear;
end;


end.
