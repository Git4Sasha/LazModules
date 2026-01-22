unit LocalTimeWork;

interface

uses
  SysUtils;

function GetTickCountCPU:QWord; // Функция возвращает кол-во тактов от начала запуска процессора
function GetCPUFreq:QWord; // Определение частоты процессора
function TimeToStr(sec:Double):string; // Преобразование времени, заданного в секундах, в строку
function DateTimeToStr(dt:Double):string; // Перевод паскалевского формата времени в строку YYYY.MM.DD_HH.MM.SS.zzz
function GetClockTimeValue:QWord; // Функция аналогична GetTickCountCPU (сделана для совместимости)
function GetClockRealTime:Double;  // Функция возвращает текущее локальное время

implementation

{$ASMMODE INTEL}

function GetTickCountCPU:QWORD; register;  // Функция возвращает кол-во тактов от начала запуска процессора
begin
asm
  rdtsc // db $f, $31
  mov dword ptr Result,eax
  mov dword ptr Result+4,edx
end;
end;

function GetCPUFreq:QWord; // Определение частоты процессора ( В модуле Windows есть похожая процедура - QueryPerformanceFrequency )
begin
  Result:=GetTickCountCPU;
  Sleep(500);
  Result:=(GetTickCountCPU-Result)*2;
end;


function TimeToStr(sec:Double):string; // Преобразование времени, заданного в секундах, в строку
var
  h,m,secint:Cardinal;
begin
	// Переводит секунды в ЧЧ:ММ:СС.доли секунд
  secint:=Trunc(sec);
  sec:=Frac(sec)*1000;
  h:=secint div 3600;
  m:=(secint mod 3600) div 60;
  secint:=(secint mod 3600) mod 60;
  Result:=Format('%.2d:%.2d:%.2d.%.3d',[h,m,secint,Trunc(sec)]);
end;

function DateTimeToStr(dt:Double):string; // Перевод паскалевского формата времени в строку YYYY.MM.DD_HH.MM.SS.zzz
begin
  Result:=FormatDateTime('YYYY.MM.DD_HH.MM.SS.zzz',dt);
end;

function GetClockTimeValue: QWord; register;
begin
  asm
    rdtsc // db $f, $31
    mov dword ptr Result,eax
    mov dword ptr Result+4,edx
  end;
end;

function GetClockRealTime:Double;  // Функция возвращает текущее локальное время
begin
  // В целой части, возвращаемого вещественного числа, хранится кол-во дней с 30 декабря 1899 года.
  // В дробной части, возвращаемого вещественного числа, хранится дробная часть дня
  Result:=now;
end;

end.
