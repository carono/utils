unit UntypedDatagram;

{$mode objfpc}{$H+}

interface

uses Classes, CustomUtils, IdGlobal, variants, arraymanager, SysUtils;

type
  { TUntypedDatagram }
  TUntypedDatagram = class
  private
    A: array of variant;
    N: array of string[16];
    Count1: integer;
    Position1: integer;
    Buffer1: integer;
    TBuffer1: integer;
    HNames: boolean;
    function GetIndex(AName: string): integer;
    function GetCount: integer;
    function GetEof: boolean;
    procedure GetIndexes;
    function GetSize: integer;
    function Read: variant;
    procedure SetBuffer(AValue: integer);
    procedure AppendStr(var ABytes: TIdBytes; AString: string);
    procedure AppendVar(var ABytes: TIdBytes; AVar: variant);
  public
    property Buffer: integer read Buffer1 write SetBuffer;
    property EOF: boolean read GetEof;
    property Position: integer read Position1;
    property Count: integer read GetCount;
    property Size: integer read GetSize;
    procedure Add(Data: variant); overload;
    procedure Add(AName: string; Data: variant); overload;
    procedure Clear;
    procedure Seek(index: integer);
    function Extract(index: integer): variant;
    function Get(index: integer): variant; overload;
    function Get(AName: string): variant; overload;
    function GetNext(): variant;
    function AsInteger(index: integer): integer;
    function AsString(index: integer): string;
    function AsByteArray: TIdBytes;
    procedure SaveToFile(const AFileName: string);
    procedure AddByteArray(ByteArray: TIdBytes);
    constructor Create(ByteArray: TIdBytes = nil);
    destructor Destroy; override;
  end;

implementation

{ TUntypedDatagram }

function TUntypedDatagram.GetIndex(AName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if N[i] = AName then
    begin
      Result := i;
      break;
    end;
end;

function TUntypedDatagram.GetCount: integer;
begin
  Result := Count1;
end;

function TUntypedDatagram.GetEof: boolean;
begin
  if Position1 >= Count1 then
    Result := True
  else
    Result := False;
end;

procedure TUntypedDatagram.GetIndexes;
begin

end;

function TUntypedDatagram.GetSize: integer;
begin
  Result := Count * SizeOf(variant);
end;

function TUntypedDatagram.Read: variant;
begin
  Result := A[Position1];
end;

procedure TUntypedDatagram.SetBuffer(AValue: integer);
begin
  if Buffer1 = AValue then
    Exit;
  Buffer1 := AValue;
end;

procedure TUntypedDatagram.AppendStr(var ABytes: TIdBytes; AString: string);
var
  size1: integer;
begin
  size1 := Length(AString);

  AppendBytes(ABytes, RawToBytes(VarType(AString), sizeOf(tvartype)));
  AppendBytes(ABytes, RawToBytes(size1, SizeOf(integer)));
  AppendBytes(ABytes, RawToBytes(AString[1], size1));
end;

procedure TUntypedDatagram.AppendVar(var ABytes: TIdBytes; AVar: variant);
var
  B: TIdBytes;
begin
  AppendBytes(ABytes, RawToBytes(VarType(AVar), sizeOf(tvartype)));
  B := RawToBytes(AVar, sizeOf(AVar));
  AppendBytes(ABytes, B);
end;

function TUntypedDatagram.AsByteArray: TIdBytes;
var
  i, p, size1: integer;
  m: smallint;
  B: TIdBytes;
  S: string;
  S1: ansistring;
begin
  Result := nil;
  for i := 0 to Count1 - 1 do
  begin
    if (VarIsStr(A[i])) then
      AppendStr(Result, varToStr(A[i]))
    else
      AppendVar(Result, A[i]);
  end;
  if (HNames) then
  begin
    AppendStr(Result, '{NAMES}');
    S := '';
    SetLength(S, Count * 16);
    for i := 0 to Count - 1 do
      move(N[i], S[(i * 16)], 16);
    AppendStr(Result, S);
  end;
end;

procedure TUntypedDatagram.SaveToFile(const AFileName: string);
var
  FS: TFileStream;
  B: TIdBytes;
begin
  if FileExists(AFileName) then
    FS := TFileStream.Create(AFileName, fmOpenWrite)
  else
    FS := TFileStream.Create(AFileName, fmCreate);
  FS.Size := 0;
  B := AsByteArray;
  FS.Write(B[0], Length(B));
  FS.Free;
end;

procedure TUntypedDatagram.AddByteArray(ByteArray: TIdBytes);
var
  MS: TMemoryStream;
  VType1: tvartype;
  Value1, Value2: variant;
  size1: integer;
  B: TIdBytes;
  i, x, k: integer;
  S, S1: ansistring;
begin
  if assigned(ByteArray) then
  begin
    x := 0;
    MS := TMemoryStream.Create;
    MS.WriteBuffer(ByteArray[0], Length(ByteArray));
    MS.Position := 0;
    size1 := length(ByteArray);
    while MS.Position <> MS.Size do
    begin
      MS.Read(VType1, sizeOf(VType1));
      Value1 := 0;
      Value1 := VarAsType(Value1, VType1);
      if VarIsStr(Value1) then
      begin
        MS.Read(size1, sizeOf(integer));
        setLength(S, size1);
        MS.ReadBuffer(S[1], size1);
        Add(S);
      end
      else
      begin
        i := SizeOf(Value2);
        MS.Read(Value2, i);
        Add(Value2);
      end;
      Inc(x);
    end;
    MS.Free;
    if (Count1 > 2) then
    begin
      S := Get(Count - 2);
      if (S = '{NAMES}') then
      begin
        HNames := True;
        Inc(x, -2);
        k := x;
        x := Count - 2 - k;
        S := Get(Count - 1);
        for i := 0 to k - 1 do
        begin
          S1 := Copy(S, (i * 16) + 1, 16);
          size1 := length(S1);
          N[x + i] := copy(S1, 1, Pos(#0, S1) - 1);
        end;
        Extract(Count - 1);
        Extract(Count - 1);
      end;
    end;
  end;
end;


procedure TUntypedDatagram.Add(Data: variant);
var
  Str1, s: string;
  Bytes1: TIdBytes;
  B: TIdBytes absolute Str1;
  i: integer;
  Len1: integer;
  x: byte;
  Type1: tvartype;
  zz: TVarRec;
begin
  if TBuffer1 = 0 then
  begin
    SetLength(A, Count1 + Buffer);
    SetLength(N, Count1 + Buffer);
    TBuffer1 := Buffer;
  end;
  A[Count1] := Data;
  N[Count1] := '';
  Inc(Count1);
  Inc(TBuffer1, -1);
end;

procedure TUntypedDatagram.Add(AName: string; Data: variant);
var
  i: integer;
begin
  if not HNames then
    HNames := True;

  i := GetIndex(AName);
  if i = -1 then
  begin
    Add(Data);
    N[Count1 - 1] := AName;
  end
  else
    A[i] := Data;
end;

procedure TUntypedDatagram.Clear;
begin
  Position1 := 0;
  Count1 := 0;
end;

procedure TUntypedDatagram.Seek(index: integer);
begin
  if Index < Count then
    Position1 := Index
  else
    Position1 := Count;
end;

function TUntypedDatagram.Extract(index: integer): variant;
var
  h: integer;
begin
  Result := Get(index);
  h := High(A);
  if index <= h then
  begin
    {$HINTS OFF}
    Finalize(A[index]);
{$HINTS ON}
    Move(A[index + 1], A[index], (h - index) * SizeOf(A[0]));
    Move(N[index + 1], N[index], (h - index) * SizeOf(N[0]));
    FillChar(A[h], SizeOf(A[0]), 0);
    FillChar(N[h], SizeOf(N[0]), 0);
    SetLength(A, h);
    SetLength(N, h);
    count1 := h;
  end;
end;

function TUntypedDatagram.Get(index: integer): variant;
begin
  Position1 := index;
  Result := Read;
end;

function TUntypedDatagram.Get(AName: string): variant;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if N[i] = AName then
    begin
      Result := A[i];
      break;
    end;
end;

function TUntypedDatagram.GetNext: variant;
begin
  Result := Read;
  Inc(Position1);
end;

function TUntypedDatagram.AsInteger(index: integer): integer;
begin
  Result := integer(Get(index));
end;

function TUntypedDatagram.AsString(index: integer): string;
begin
  Result := VarToStr(Get(index));
end;

constructor TUntypedDatagram.Create(ByteArray: TIdBytes);
begin
  HNames := False;
  Buffer1 := 1;
  TBuffer1 := 0;
  Clear;
  Position1 := 0;
  AddByteArray(ByteArray);
end;

destructor TUntypedDatagram.Destroy;
begin

end;

end.
