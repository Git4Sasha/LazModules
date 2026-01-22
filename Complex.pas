// Модуль для работы с комплексными числами
unit Complex;

interface

uses  // Используемые модули
	Math, ArrayOfTypes;

const
	KRad:Double=pi/180;
	KDeg:Double=180/pi;
  ZeroComplex:TValue2f=(A:0;B:0);
  pi2=pi*2;

function GetLength(x,y:Single):Single;
function GetAngle(x,y:Single):Single; // угол в радианах в пределах: -0.5pi..1.5*pi
function AngleBetween(a,b:TValue2f):Single; // Возвращает минимальный угол между векторами в градусах
function Multipli(a:TValue2f; b:Single):TValue2f;  // умножение комплексного числа на скаляр
function MulComplex(a,b:TValue2f):TValue2f; // комплексное перемножение
function DivComplex(a,b:TValue2f):TValue2f; // "a" делится на "b"
function AddComplex(a,b:TValue2f):TValue2f;
function SubComplex(a,b:TValue2f):TValue2f; // Из "a" вычитается "b"
function MathComplex(a,b:Single):TValue2f; // Создание комплексного числа из значений Re и Im
function PowerComplex(R,F:Single):TValue2f;	// Создание комплексного числа из значений модуля комплексного числа и его фазы
function RotComplex(a,b:TValue2f):TValue2f; // Возвращает повёрнутый на угол ang вектор а, b.A - Cos(ang) b.B - Sin(ang)
function GetLengthComplex(A:TValue2f):Single; // Определение модуля комплексного числа
function GetAngleComplex(A:TValue2f):Single; // Возвращает Фазу комплексного числа 0..360
function GetMidComplex(mass:TArrayOfValue2f):TValue2f; overload; // Находит среднее значение в массиве комплексных чисел
function GetMidComplex(mass:PValue2f; len:Integer):TValue2f; overload; // Аналогично  function GetMidComplex(mass:TArrayOfValue2f):TValue2f;

procedure GetMidSKOComplex(mass:TArrayOfValue2f; var Mid,SKO:TValue2f); // Находит среднее значение и среднее квадратическое отклонение


implementation

function CreateComplex(Re,Im:Single):TValue2f;
begin
  Result.A:=Re;
  Result.B:=Im;
end;

function GetAngleComplex(A:TValue2f):Single; // 0..360
begin
	if (A.x=0)and(A.y=0) then Exit(0);

  if A.x=0 then begin
		if A.y>0 then Result:=90 else Result:=270;
	end else begin
		Result:=ArcTan(A.y/A.x)*KDeg;
  	if A.x<0 then Result:=Result+180;
  end;
end;

function GetAngle(x,y:Single):Single; // -0.5pi..1.5*pi Возвращает угол в радианах относительно оси X
begin
	if (x=0)and(y=0) then Exit(0);

  if x=0 then
    begin
      if y>0 then
        Result:=0.5*pi
      else
        Result:=1.5*pi
    end
  else
    begin
      Result:=ArcTan(y/x);
      if x<0 then
        Result:=pi+Result;
    end;
end;

function GetLengthComplex(A:TValue2f):Single;
begin
	Result:=Sqrt(Sqr(A.A)+Sqr(A.B));
end;

function GetLength(x,y:Single):Single;
begin
	Result:=Sqrt(Sqr(x)+Sqr(y));
end;

function AngleBetween(a,b:TValue2f):Single;
begin
  Result:=a.A*b.A+a.B*b.B;
  Result:=Result/(GetLengthComplex(a)*GetLengthComplex(b));
  Result:=ArcCos(Result)*KDeg;
end;

function Multipli(a:TValue2f; b:Single):TValue2f;
begin
  Result.A:=a.A*b;
  Result.B:=a.B*b;
end;

// Аргументы задаются в координатах на комплексной плоскости
function MulComplex(a,b:TValue2f):TValue2f; // Выдаёт результат в координатах на комплексной плоскости
begin
	Result.Re:=a.Re*b.Re - a.Im*b.Im;
	Result.Im:=a.Re*b.Im + a.Im*b.Re;
end;

// Аргументы задаются в координатах на комплексной плоскости
function DivComplex(a,b:TValue2f):TValue2f; // Выдаёт результат в координатах на комплексной плоскости
var
	r:Single;
begin
  r:=b.Re*b.Re + b.Im*b.Im;
	Result.Re:=(a.Re*b.Re + a.Im*b.Im)/r;
	Result.Im:=(a.Im*b.Re - a.Re*b.Im)/r;
end;

function MathComplex(a,b:Single):TValue2f;  // Комплексное число задаётся в координатах на комплексной области
begin
	Result.A:=a;
	Result.B:=b;
end;

function PowerComplex(R,F:Single):TValue2f;
begin
	Result.A:=Cos(F)*R;
	Result.B:=Sin(F)*R;
end;

function AddComplex(a,b:TValue2f):TValue2f;
begin
	Result.A:=a.A+b.A;
	Result.B:=a.B+b.B;
end;

function SubComplex(a,b:TValue2f):TValue2f;
begin
	Result.A:=a.A-b.A;
	Result.B:=a.B-b.B;
end;

function RotComplex(a,b:TValue2f):TValue2f;
begin
  Result.A:=b.A*a.A-b.B*a.B;
	Result.B:=b.B*a.A+b.A*a.B;
end;

function GetMidComplex(mass:TArrayOfValue2f):TValue2f; // Находит среднее значение в массиве комплексных чисел
var
  i,c:Integer;
begin
  Result:=ZeroComplex;
  c:=Length(mass);
  for i:=0 to c-1 do
    Result:=AddComplex(Result, mass[i]);
  Result:=Multipli(Result, 1/c);
end;

function GetMidComplex(mass: PValue2f; len: Integer): TValue2f;
var
  i:Integer;
  p:PValue2f;
begin
  Result:=ZeroComplex;
  p:=mass;
  for i:=0 to len-1 do
    begin
      Result.Re+=p^.Re;
      Result.Im+=p^.Im;
      Inc(p);
    end;
  Result.Re/=len;
  Result.Im/=len;
end;

procedure GetMidSKOComplex(mass:TArrayOfValue2f; var Mid,SKO:TValue2f); // Находит среднее значение и среднее квадратическое отклонение
var
  i,c:Integer;
begin
  Mid:=ZeroComplex;
  SKO:=ZeroComplex;
  c:=Length(mass);
  for i:=0 to c-1 do
    begin
      Mid.Re:=Mid.Re+mass[i].Re;
      Mid.Im:=Mid.Im+mass[i].Im;
      SKO.Re:=SKO.Re+mass[i].Re*mass[i].Re;
      SKO.Im:=SKO.Im+mass[i].Im*mass[i].Im;
    end;
  Mid:=Multipli(Mid, 1/c);
  SKO.Re:=sqrt(SKO.Re/c - Mid.Re*Mid.Re);
  SKO.Im:=sqrt(SKO.Im/c - Mid.Im*Mid.Im);
end;

end.
