unit CryptRDP;

interface


function CryptRDPPassword(sPassword: string): string;

implementation

uses Windows, SysUtils;

const
  crypt32 = 'crypt32.dll';
  CRYPTPROTECT_UI_FORBIDDEN = $01;

type
  DATA_BLOB = record
    cbData: DWORD;
    pbData: Windows.PBYTE;
  end;
  PDATA_BLOB = ^DATA_BLOB;
  LPLPWSTR = ^LPWSTR;

function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: LPCWSTR; pOptionalEntropy: PDATA_BLOB;
  pvReserved: Pointer; pPromptStruct: Pointer; dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall;
  external crypt32 Name 'CryptProtectData';

function BlobDataToHexStr(P: PByte; I: Integer): string;
var HexStr: string;
begin
  HexStr := '';
  while (I > 0) do begin
    Dec(I);
    HexStr := HexStr + IntToHex(P^, 2);
    Inc(P);
  end;
  Result := HexStr;
end;

function CryptRDPPassword(sPassword: string): string;
var DataIn: DATA_BLOB;
    DataOut: DATA_BLOB;
    pwDescription: PWideChar;
    PwdHash: string;
begin
  PwdHash := '';

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  // RDP uses UniCode
  DataIn.pbData := Pointer(WideString(sPassword));
  DataIn.cbData := Length(sPassword) * SizeOf(WChar);

  // RDP always sets description to psw
  pwDescription := WideString('psw');

  if CryptProtectData(@DataIn,
                      pwDescription,
                      nil,
                      nil,
                      nil,
                      CRYPTPROTECT_UI_FORBIDDEN,  // Never show interface
                      @DataOut) then
  begin
    PwdHash := BlobDataToHexStr(DataOut.pbData, DataOut.cbData);
  end;
  Result := PwdHash;

  // Cleanup
  LocalFree(Cardinal(DataOut.pbData));
  LocalFree(Cardinal(DataIn.pbData));

end;
end.
