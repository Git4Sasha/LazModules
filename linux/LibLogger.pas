unit LibLogger;

{$mode objfpc}{$H+}

interface

uses
  dynlibs, sysutils;

type
  TInputData=record
    Col:Cardinal; // цвет, которым будет закрашен прямоугольник (цвет можно рассматривать как уровень сигнала)
    tag:Integer;   // любое число
    time:Double;  // Время
  end;
  PInputData=^TInputData;
  TArrayOfInputData = array of TInputData;
  T2DArrayOfInputData = array of TArrayOfInputData;

  TInitLogger = function(log:Boolean=False):Integer; cdecl;
  TDeinitLogger = procedure; cdecl;
  TSendDigitLog = procedure (id:Integer; style:Integer; tag: Integer=0); cdecl; // id-идентификатор графика может принимать значения от 0 до 9;  style - если в пределах 0..7 то интерпритируется как стандартный цвет, иначе как цвет заданный пользователем; tag - пока не изпользуется
  TResetLogger = procedure; cdecl;
  TAddrSMControl = function:Pointer; cdecl;
  TAddrSMData = function(id:Integer):Pointer; cdecl;
  TGetLastError = function:Integer; cdecl;

const
  LL_BLACK_COL=0;     // Чёрный
  LL_RED_COL=1;       // Красный
  LL_GREEN_COL=2;     // Зелёный
  LL_YELLOW_COL=3;    // Жёлтый
  LL_BLUE_COL=4;      // Синий
  LL_VIOLET_COL=5;    // Фиолетовый
  LL_TURQUOISE_COL=6; // Бирюзовый
  LL_WHITE_COL=7;     // Белый

const
  SMDATA_COUNT = 10; // Количество циклограмма записываемых параллельно

var
  SendDigitLog :TSendDigitLog;
  ResetLogger  :TResetLogger;
  AddrSMControl:TAddrSMControl;
  AddrSMData   :TAddrSMData;
  GetLastError :TGetLastError;


implementation

const
  libloggername='liblogger.so';

var
  libhandle: TLibHandle;
  InitLogger   :TInitLogger;
  DeinitLogger :TDeinitLogger;

procedure Initlib;
var
  err:Integer;
begin
  libhandle:=LoadLibrary(PChar(libloggername));
  if libhandle=NilHandle then begin
    WriteLn('Библиотека LibLogger не загружается');
    Exit;
  end;

  InitLogger   :=TInitLogger     (GetProcAddress(libhandle, 'InitLogger'));
  DeinitLogger :=TDeinitLogger   (GetProcAddress(libhandle, 'DeinitLogger'));
  SendDigitLog :=TSendDigitLog   (GetProcAddress(libhandle, 'SendDigitLog'));
  ResetLogger  :=TResetLogger    (GetProcAddress(libhandle, 'ResetLogger'));
  AddrSMControl:=TAddrSMControl  (GetProcAddress(libhandle, 'AddrSMControl'));
  AddrSMData   :=TAddrSMData     (GetProcAddress(libhandle, 'AddrSMData'));
  GetLastError :=TGetLastError   (GetProcAddress(libhandle, 'GetLastError'));

  err:=InitLogger(True);
  if err<>0 then
    raise Exception.Create(Format('Ошибка при инициализации библиотеки Logger %d',[err]));
end;

procedure DeinitLib;
begin
  if libhandle=NilHandle then Exit;
  DeinitLogger;
  FreeLibrary(libhandle);
end;

initialization;
  {$Warning ВНИМАНИЕ ИНИЦИАЛИЗАЦИЯ БИБЛИОТЕКИ, КОТОРОЙ МОЖЕТ НЕ БЫТЬ}
  Initlib;

finalization;
  DeinitLib;

end.

