unit DUAnglesCalc;

// В этом модуле собраны функции для работы с углами представленными в виде дирекционных углов
// Дирекционные углы используются в артиллерии.
// Дирекционный угол это угло выраженный двумя положительными целыми числами через тире:
// - первое число это большие диления угломера, одно деление это 6 градусов
// - второе число это малые деления угломера, одно деление это 1/100 от большого деления угломера, т.е. от 6-ти градусов
// пример: дерекционный угол = 12-08
//         угло в градусах   = (12 + 0.08)*6 = 72.48 градусов
// Дирекционный угол всегда записывается 2-х знакомыми числами, т.е. при необходимости нужно добавлять нули перед числами



{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils

  ;

function StrIsDU(const str:string):Boolean;                           // Функция возвращает True, если переданная строка похожа на дирекционные углы
function DegToDUStr(deg:Single):string;                               // Перевод углов в дирекционные углы и преобразование в строку
function DUStrToDeg(const du:string):Single;                          // Перевод дирекционных углов в строковом виде в углы в градусах
function DUorAngStrToDeg(const duang:string):Single;                  // Перевод угла в виде дирекционного угла или в виде градусов в градусы
function DUOrAngStrToStr(const duang:string; ang:Single):string;      // Функция которая возвращает строку, которая хранит градусы или ДУ в зависимости от входной строки



implementation

function StrIsDU(const str: string): Boolean;
var
  p:Integer;
  val:Single;
begin
  // Выполняется попытка перевести переданную строку в угол (если перевод будет успешен, значит угол передан в формате обычных углов)
  if TryStrToFloat(str, val) then Exit(False);     // Возвращаем False т.к. не дирекционные углы

  p:=Pos('-', str);                               // Поиск знака тире в строке
  if (p=2)or(p=3) then Exit(True);                // Если в строке найден знак "-" и его положение в строке на 2-й или на 3-й позиции, то считается, что передан дирекционный угол (возвращается True)
end;

function DegToDUStr(deg: Single): string;
begin
  // ВНИМАНИЕ!!! передаваемый угол должен быть в диапазоне от 0 до 360
  deg:=deg/6;                                                           // Перевод углов в градусах в дирекционные углы
  Result:=Format('%2.2d-%2.2d',[Trunc(deg), Trunc(Frac(deg)*100)]);     // Особый способ вывода дирекционных углов
end;

function DUStrToDeg(const du: string): Single;
var
  ds:Char;
begin
  ds:=DefaultFormatSettings.DecimalSeparator;         // Сохранение текущего разделителя целой и дробной части
  DefaultFormatSettings.DecimalSeparator:='-';        // тире будет знаком для разделения целой и дробной части
  if not TryStrToFloat(du, Result) then begin
    DefaultFormatSettings.DecimalSeparator:='.';      // точка будет знаком для разделения целой и дробной части
    if not TryStrToFloat(du, Result) then begin
      DefaultFormatSettings.DecimalSeparator:=',';    // запятая будет знаком для разделения целой и дробной части
      if not TryStrToFloat(du, Result) then Result:=0;
    end;
  end;
  Result:=Result*6;                                   // Умножение на 6, это перевод из дирекционных углов в градусы
  DefaultFormatSettings.DecimalSeparator:=ds;         // Возврат сохранённого разделителя целой и дробной части
end;

function DUorAngStrToDeg(const duang: string): Single;
var
  p:Integer;
begin
  if not TryStrToFloat(duang, Result) then begin    // Выполняется попытка перевести переданную строку в угол (если перевод будет успешен, значит угол передан не в формате дирекционных углов, а в виде обычного значения)
    p:=Pos('-', duang);                             // Поиск знака тире в строке
    if (p=2)or(p=3) then Exit(DUStrToDeg(duang));   // Если в строке найден знак "-" и его положение в строке на 2-й или на 3-й позиции, то считается, что передан дирекционный угол
    if p<>0 then Exit(10000);                       // Если в строке найден знак "-" и его положение отличается от 2-й или 3-й позиции, то возвращается ошибка в виде большого значения угла
  end;
end;

function DUOrAngStrToStr(const duang: string; ang: Single): string;
begin
  // duang - строка по которой будет определяться в каком виде нужно выдавать выходную строку в ДУ или в градусах
  // ang   - угол, который будет переводиться в ДУ или в градусы (задаётся в градусах)

  if StrIsDU(duang) then                                    // Если по строке определено ДУ (дирекционные углы), то
    Result:=DegToDUStr(ang)                                 // В результате будет возвращена строка в ДУ
  else
    Result:=Format('%.2f',[ang]);            // В результате будет возвращена строка в градусах
end;

end.

