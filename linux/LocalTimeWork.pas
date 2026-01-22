unit LocalTimeWork;

interface

uses
  dynlibs
  , SysUtils
  , Linux
  , ArrayOfTypes
  ;



function GetClockTimeValue:Double;                          // Получение значения локального времени (в секундах)
function GetClockTimeValue:TValue8b;                        // Получение значения локального времени
function GetClockRealTime:Double;                           // Получение значения времени от часов реального времени
function GetClockRealTime:TValue8b;                         // Получение значения времени от часов реального времени
function TimeToStr(sec:Double):string;                      // Преобразование времени, заданного в секундах, в строку
function TimeToStrShort(sec:Double):string;                 // Преобразование времени, заданного в секундах, в строку

function DateTimeToStr(dt,hourshift:Double):string;         // Перевод секунд и наносекунд в строку год, месяц, число, часы, минуты, секунды, долисекунд (для функции GetClockRealTime)
function DateTimeToStrInt(dt,hourshift:Double):string;      // Аналогично DateTimeToStr но без дробных значений секунд
function DateTimeToStrTime(dt, hourshift: Double): string;  // Функция работает аналогично функции DateTimeToStr, но выводит только время без даты

function DateTimeToStr(dt:Double):string;                   // Перевод секунд и наносекунд в строку год, месяц, число, часы, минуты, секунды, долисекунд (для функции Now)
function DateTimeToStrTime(dt:Double):string;               // Функция работает аналогично функции DateTimeToStr, но выводит только время без даты


implementation

function GetClockTimeValue: Double;
var
  tm:array [0..1] of PtrUInt;
begin
  clock_gettime(CLOCK_MONOTONIC, @tm);
  GetClockTimeValue:=tm[0]+tm[1]*1e-9;
end;

function GetClockTimeValue:TValue8b;                        // Получение значения локального времени
var
  tm:array [0..1] of PtrUInt;
begin
  clock_gettime(CLOCK_MONOTONIC, @tm);
  Result.u0_32:=tm[0];
  Result.u1_32:=tm[1];
end;

function GetClockRealTime: Double;
var
  tm:array [0..1] of PtrUInt;
begin
  clock_gettime(CLOCK_REALTIME, @tm);
  GetClockRealTime:=tm[0]+tm[1]*1e-9;
end;

function GetClockRealTime:TValue8b;                         // Получение значения времени от часов реального времени
var
  tm:array [0..1] of PtrUInt;
begin
  clock_gettime(CLOCK_REALTIME, @tm);
  Result.u0_32:=tm[0];
  Result.u1_32:=tm[1];
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
  TimeToStr:=Format('%.2d:%.2d:%.2d.%.3d',[h,m,secint,Trunc(sec)]);
end;

function TimeToStrShort(sec: Double): string; // Переводит секунды в ЧЧ:ММ:СС
var
  h,m,secint:Cardinal;
begin
	// Переводит секунды в ЧЧ:ММ:СС
  secint:=Trunc(sec);
  sec:=Frac(sec)*1000;
  h:=secint div 3600;
  m:=(secint mod 3600) div 60;
  secint:=(secint mod 3600) mod 60;
  TimeToStrShort:=Format('%.2d:%.2d:%.2d',[h,m,secint]);
end;

function DateTimeToStr(dt, hourshift: Double): string;
const
  dayinmonth:array [0..11] of Byte=(31, 28, 31,  30,  31,   30,  31,   31,   30,   31,   30,   31);

  secinyearmid:QWord=31557600; // 31557600 = 365.25*24*3600 - это кол-во секунд в году, если предполагать, что год не целый, а есть ещё 0.25 от дня
  secinyearmin:QWord=31536000; // 31536000 = 365*24*3600    - это кол-во секунд в не високосном году
  secinyearmax:QWord=31622400; // 31622400 = 366*24*3600    - это кол-во секунд в високосном году

  secinday:QWord=86400;     // 24*3600
  secinhour:QWord=3600;     // 60*60
  secinminutes:QWord=60;
var
  y,m,d,h,mn:Integer;
  ynv:Integer;
  sec:QWord;
begin
  // Эта функция создана для перевода даты и времени в строку времени, которое возвращает функция GetClockRealTime
  // dt - в целой части кол-во секунд прошедших с 01.01.1970 в дробной части кол-во наносекунд
  // hourshift - Сдвиг в часах из-за часового пояса

  sec:=Trunc(dt+hourshift*secinhour);   // Целое кол-во секунд прошедших с 01.01.1970
  y:=sec div secinyearmid { 31557600};  // Получаем целое кол-во лет с 1970 года

  // Чтобы правильно разсчитать кол-во високосных годов прошедших с 1970-го года необходимо к году прибавить 2, т.к. первым високосным годом после 1970-го года (сам 1970 год не високосный) является 1972 год
  // разница между 1972 и 1970-м годами равна 2, при целочисленном делении на 4 получим 0 високосных годов, хотя на самом деле один уже есть это 1972, поэтому прибавляется 2, разница увеличивается на 2
  // и получается 1 високосный год.
  ynv:=((y+2) div 4);                   // Кол-во високосных годов

  sec:=sec - (y-ynv)*secinyearmin - ynv*secinyearmax; // // Кол-во секунд с начала года

  d:=sec div secinday;                  // Кол-во дней с начала года (из этого значения определяется номер месяца)
  sec:=sec - d*secinday;                // Кол-во секунд от начала дня
  h:=sec div secinhour;                 // Кол-во часов от начала дня
  sec:=sec - h*secinhour;               // Кол-во секунд от начала часа
  mn:=sec div secinminutes;             // Кол-во минут от начала часа
  sec:=sec - mn*secinminutes;           // Кол-во секунд от начала минуты

  y:=y+1970;

  Inc(d);
  if y mod 4=0 then dayinmonth[1]:=29 else dayinmonth[1]:=28;
  for m:=0 to 11 do begin
    if d<=dayinmonth[m] then Break;
    d:=d-dayinmonth[m];
  end;
  Inc(m);

  Result:=Format('%0.4d.%0.2d.%0.2d_%0.2d.%0.2d.%0.2d.%0.3d',[y,m,d,h,mn,sec, Trunc(Frac(dt)*1000)]);
end;

function DateTimeToStrInt(dt, hourshift: Double): string;
const
  dayinmonth:array [0..11] of Byte=(31, 28, 31,  30,  31,   30,  31,   31,   30,   31,   30,   31);

  secinyearmid:QWord=31557600; // 31557600 = 365.25*24*3600 - это кол-во секунд в году, если предполагать, что год не целый, а есть ещё 0.25 от дня
  secinyearmin:QWord=31536000; // 31536000 = 365*24*3600    - это кол-во секунд в не високосном году
  secinyearmax:QWord=31622400; // 31622400 = 366*24*3600    - это кол-во секунд в високосном году

  secinday:QWord=86400;     // 24*3600
  secinhour:QWord=3600;     // 60*60
  secinminutes:QWord=60;
var
  y,m,d,h,mn:Integer;
  ynv:Integer;
  sec:QWord;
begin
  // Эта функция создана для перевода даты и времени в строку времени, которое возвращает функция GetClockRealTime
  // dt - в целой части кол-во секунд прошедших с 01.01.1970 в дробной части кол-во наносекунд
  // hourshift - Сдвиг в часах из-за часового пояса

  sec:=Trunc(dt+hourshift*secinhour);   // Целое кол-во секунд прошедших с 01.01.1970
  y:=sec div secinyearmid { 31557600};  // Получаем целое кол-во лет с 1970 года

  // Чтобы правильно разсчитать кол-во високосных годов прошедших с 1970-го года необходимо к году прибавить 2, т.к. первым високосным годом после 1970-го года (сам 1970 год не високосный) является 1972 год
  // разница между 1972 и 1970-м годами равна 2, при целочисленном делении на 4 получим 0 високосных годов, хотя на самом деле один уже есть это 1972, поэтому прибавляется 2, разница увеличивается на 2
  // и получается 1 високосный год.
  ynv:=((y+2) div 4);                   // Кол-во високосных годов

  sec:=sec - (y-ynv)*secinyearmin - ynv*secinyearmax; // Кол-во секунд с начала года

  d:=sec div secinday;                  // Кол-во дней с начала года (из этого значения определяется номер месяца)
  sec:=sec - d*secinday;                // Кол-во секунд от начала дня
  h:=sec div secinhour;                 // Кол-во часов от начала дня
  sec:=sec - h*secinhour;               // Кол-во секунд от начала часа
  mn:=sec div secinminutes;             // Кол-во минут от начала часа
  sec:=sec - mn*secinminutes;           // Кол-во секунд от начала минуты

  y:=y+1970;

  Inc(d);
  if y mod 4=0 then dayinmonth[1]:=29 else dayinmonth[1]:=28;
  for m:=0 to 11 do begin
    if d<=dayinmonth[m] then Break;
    d:=d-dayinmonth[m];
  end;
  Inc(m);

  Result:=Format('%0.4d.%0.2d.%0.2d_%0.2d.%0.2d.%0.2d',[y,m,d,h,mn,sec]);
end;

function DateTimeToStrTime(dt, hourshift: Double): string;
const
  dayinmonth:array [0..11] of Byte=(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

  secinyearmid:QWord=31557600; // 31557600 = 365.25*24*3600 - это кол-во секунд в году, если предполагать, что год не целый, а есть ещё 0.25 от дня
  secinyearmin:QWord=31536000; // 31536000 = 365*24*3600    - это кол-во секунд в не високосном году
  secinyearmax:QWord=31622400; // 31622400 = 366*24*3600    - это кол-во секунд в високосном году

  secinday:QWord=86400;     // 24*3600
  secinhour:QWord=3600;     // 60*60
  secinminutes:QWord=60;
var
  y,m,d,h,mn:Integer;
  ynv:Integer;
  sec:QWord;
begin
  // Эта функция создана для перевода даты и времени в строку времени, которое возвращает функция GetClockRealTime
  // dt - в целой части кол-во секунд прошедших с 01.01.1970 в дробной части кол-во наносекунд
  // hourshift - Сдвиг в часах из-за часового пояса

  sec:=Trunc(dt+hourshift*secinhour);   // Целое кол-во секунд прошедших с 01.01.1970
  y:=sec div secinyearmid { 31557600};  // Получаем целое кол-во лет с 1970 года

  // Чтобы правильно разсчитать кол-во високосных годов прошедших с 1970-го года необходимо к году прибавить 2, т.к. первым високосным годом после 1970-го года (сам 1970 год не високосный) является 1972 год
  // разница между 1972 и 1970-м годами равна 2, при целочисленном делении на 4 получим 0 високосных годов, хотя на самом деле один уже есть это 1972, поэтому прибавляется 2, разница увеличивается на 2
  // и получается 1 високосный год.
  ynv:=((y+2) div 4);                   // Кол-во високосных годов

  sec:=sec - (y-ynv)*secinyearmin - ynv*secinyearmax; // Кол-во секунд с начала года

  d:=sec div secinday;                  // Кол-во дней с начала года (из этого значения определяется номер месяца)
  sec:=sec - d*secinday;                // Кол-во секунд от начала дня
  h:=sec div secinhour;                 // Кол-во часов от начала дня
  sec:=sec - h*secinhour;               // Кол-во секунд от начала часа
  mn:=sec div secinminutes;             // Кол-во минут от начала часа
  sec:=sec - mn*secinminutes;           // Кол-во секунд от начала минуты

  y:=y+1970;

  Inc(d);
  if y mod 4=0 then dayinmonth[1]:=29 else dayinmonth[1]:=28;
  for m:=0 to 11 do begin
    if d<=dayinmonth[m] then Break;
    d:=d-dayinmonth[m];
  end;
  Inc(m);

  Result:=Format('%0.2d.%0.2d.%0.2d.%0.3d',[h,mn,sec, Trunc(Frac(dt)*1000)]);
end;

function DateTimeToStr(dt: Double): string;
begin
  Result:=FormatDateTime('yyyy.mm.dd_hh.nn.ss.zzz', dt);
end;

function DateTimeToStrTime(dt: Double): string;
begin
  Result:=FormatDateTime('hh.nn.ss.zzz', dt);
end;



end.
