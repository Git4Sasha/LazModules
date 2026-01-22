unit LibLogger;

{$mode objfpc}{$H+}

interface

uses
  dynlibs;

type
  TInputData=record
    Col:Cardinal; // цвет, которым будет закрашен прямоугольник (цвет можно рассматривать как уровень сигнала)
    tag:Integer;   // любое число
    time:Double;  // Время
  end;
  PInputData=^TInputData;
  TArrayOfInputData = array of TInputData;
  T2DArrayOfInputData = array of TArrayOfInputData;

  TInitLogger = procedure; stdcall;
  TDeinitLogger = procedure; stdcall;
  TSendDigitLog = procedure (id:Integer; style:Integer; tag: Integer=0); stdcall;
  TResetLogger = procedure; stdcall;
  TAddrSMControl = function:Pointer; stdcall;
  TAddrSMData = function(id:Integer):Pointer; stdcall;
  TGetLastError = function:Integer; stdcall;

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
  libloggername='liblogger.dll';

var
  libhandle: TLibHandle;
  InitLogger   :TInitLogger;
  DeinitLogger :TDeinitLogger;

procedure Initlib;
begin
  libhandle:=LoadLibrary(PChar(libloggername));

  InitLogger   :=TInitLogger     (GetProcAddress(libhandle, 'InitLogger'));
  DeinitLogger :=TDeinitLogger   (GetProcAddress(libhandle, 'DeinitLogger'));
  SendDigitLog :=TSendDigitLog   (GetProcAddress(libhandle, 'SendDigitLog'));
  ResetLogger  :=TResetLogger    (GetProcAddress(libhandle, 'ResetLogger'));
  AddrSMControl:=TAddrSMControl  (GetProcAddress(libhandle, 'AddrSMControl'));
  AddrSMData   :=TAddrSMData     (GetProcAddress(libhandle, 'AddrSMData'));
  GetLastError :=TGetLastError   (GetProcAddress(libhandle, 'GetLastError'));

  InitLogger;
end;

procedure DeinitLib;
begin
  DeinitLogger;
  FreeLibrary(libhandle);
end;

initialization;
  {$Warning ВНИМАНИЕ ИНИЦИАЛИЗАЦИЯ БИБЛИОТЕКИ, КОТОРОЙ МОЖЕТ НЕ БЫТЬ}
  Initlib;

finalization;
  DeinitLib;

end.

