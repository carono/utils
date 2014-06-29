unit RMPacket;

{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, syncobjs;

type

  { TRMPacket }

  TRMPacket = class
  private
    Raw1: ansistring;
    LastAppended1: ansistring;
    TerminatorString1: string;
    function GetReady: boolean;
    function GetCount: integer;
    function GetPacket: ansistring;
  public
    property Packet: ansistring read GetPacket;
    property Raw: ansistring read Raw1;
    property TerminatorString: string read TerminatorString1 write TerminatorString1;
    function Extract(del: boolean = True): ansistring;
    property Ready: boolean read GetReady;
    property Count: integer read GetCount;
    procedure Append(part: ansistring);
    procedure Clear;
    property LastAppended: ansistring read LastAppended1;
    constructor Create(Delimeter: string = '');
    destructor Destroy; override;
  end;

implementation

{ TRMPacket }

procedure TRMPacket.Append(part: ansistring);
begin
  LastAppended1 := Part;
  Raw1 := Raw1 + part;
end;

procedure TRMPacket.Clear;
begin
  Raw1 := '';
end;

constructor TRMPacket.Create(Delimeter: string);
begin
  Raw1 := '';
  LastAppended1 := '';
  if (Delimeter = '') then
    TerminatorString1 := #0
  else
    TerminatorString1 := Delimeter;
end;

destructor TRMPacket.Destroy;
begin
  inherited Destroy;
end;

function TRMPacket.Extract(del: boolean): ansistring;
begin
  if Ready then
  begin
    Result := Copy(Raw1, 1, Pos(TerminatorString, Raw1) - 1);
    if del then
      Delete(Raw1, 1, Pos(TerminatorString, Raw1) + 1);
  end;
end;

function TRMPacket.GetReady: boolean;
var
  i: integer;
  st: ansistring;
begin
  if (Pos(TerminatorString, Raw) > 0) then
    Result := True
  else
    Result := False;
end;

function TRMPacket.GetCount: integer;
begin
  Result := -1;
end;

function TRMPacket.GetPacket: ansistring;
begin
  if Ready then
    Result := Copy(Raw, 1, Pos(TerminatorString, Raw) - 1);
end;

end.
