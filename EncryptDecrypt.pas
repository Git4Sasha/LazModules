unit EncryptDecrypt;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  ;

type

  { TCryptData }

  TCryptData=class
  private
    FC1:Cardinal;     // 1-й не передаваемый ключ для шифровки/расшифровки (это значение нельзя передавать в зашифрованном сообщении)
    FC2:Cardinal;     // 2-й не передаваемый ключ для шифровки/расшифровки (это значение нельзя передавать в зашифрованном сообщении)
    FKey:Cardinal;    // Этот ключ можно передавать вместе с зашифрованным сообщением

    procedure SetKey(AValue: Cardinal);

  public
    constructor Create(c1,c2,key:Cardinal);       // При создании объекта создаются все 3 константы для шифрования в дальнейшем менять и читать можно только ключ

    procedure Encrypt(data:Pointer; size:Integer);
    procedure Decrypt(data:Pointer; size:Integer);

    property CryptKey:Cardinal read FKey write SetKey;   // Ключ можно свободно менять, а FC1 и FC2 нет
  end;



implementation

{ TCryptData }

procedure TCryptData.Encrypt(data: Pointer; size: Integer);
var
  i: integer;
  pb: PByte;
  b: byte;
  key:Cardinal;
begin
  key:=FKey;
  pb:=PByte(data);
	for i:=0 to size-1 do begin
	  b := pb^ xor (key shr 8);
	  key := (b + key) * FC1 + FC2;
    pb^:=b;
    Inc(pb);
	end;
end;

procedure TCryptData.Decrypt(data: Pointer; size: Integer);
var
  i: integer;
  pb:PByte;
  b: byte;
  key:Cardinal;
begin
  key:=FKey;
  pb:=PByte(data);
	for i:=0 to size-1 do begin
	  b := pb^ xor (key shr 8);
    key := (pb^ + key) * FC1 + FC2;
    pb^ := b;
    Inc(pb);
	end;
end;

procedure TCryptData.SetKey(AValue: Cardinal);
begin
  if AValue=0 then
    FKey:=146853782     // Если ключ задан нулевым, то он выставляется ключём по умолчанию
  else
    FKey:=AValue;
end;

constructor TCryptData.Create(c1, c2, key: Cardinal);
begin
  if c1=0 then FC1 := 528431968;    // Значения по умолчанию
  if c2=0 then FC2 := 227195894;
  if key=0 then FKey:=146853782;
end;


end.

