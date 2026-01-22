// Модуль для работы с комплексными числами
unit ComplexEX;

interface

uses  // Используемые модули
	Math;

const
	MATH_TYPE=1; // Математический вид при этом A - зна-е вещественной части, B - зна-е мнимой части комплексного числа
	EXP_TYPE=2;		// Показательный вид при этом A - Модуль комплексного числа, B - фаза в радианах

type
	TComplexType=Integer;

	TComplex=Record
		A,B:Single; // Координаты на комплексной области
		NumericType:TComplexType;  // Поле показывает в каком виде хранится сомплексное число
																// в математическом виде или показательном
	end;

	function CreateComplex(a,b:Single; CType:TComplexType):TComplex;
	function ModulComplex(A:TComplex):Single;
	function AngleComplex(A:TComplex):Single;
	function MulComplex(a,b:TComplex):TComplex;
	function DivComplex(a,b:TComplex):TComplex;
	function AddComplex(a,b:TComplex):TComplex;
	function SubComplex(a,b:TComplex):TComplex; // Из "a" вычитается "b"
	function MathComplex(a,b:Single):TComplex; // Создание комплексного числа из значений Re и Im
	function PowerComplex(R,F:Single):TComplex;	// Создание комплексного числа из значений модуля комплексного числа и его фазы
	function ChangeType(A:TComplex):TComplex; // Смена вида хранения комплексного числа

const
	grad=pi/180;
	grad1=180/pi;

implementation

function AngleComplex(A:TComplex):Single; // Возвращает фазу комплексного числа в градусах 
begin
	case A.NumericType of
		MATH_TYPE:	begin
									if (A.A=0)and(A.B=0) then
										begin
											Result:=0;
											Exit;
										end;
									if A.A=0 then
										begin
											if A.B>0 then
												Result:=90
											else
												Result:=270;
										end
									else
										Result:=ArcTan(A.B/A.A)*grad1;
									if A.A<0 then
										Result:=180+Result
									else
										begin
											if A.B<0 then
												Result:=360+Result;
										end;
								end;
		EXP_TYPE:		Result:=A.B*grad1;
	end;
end;

function ModulComplex(A:TComplex):Single;
begin
	case A.NumericType of
		MATH_TYPE:	Result:=Sqrt(Sqr(A.A)+Sqr(A.B));
		EXP_TYPE:		Result:=A.A;
	end;
end;

function MulComplex(a,b:TComplex):TComplex; // Перемножение комплексных чисел (в случае не соответствия видов хранения, вид хранения числа b меняется на такой как у a). 
var
	R,F:Single;
begin
	if a.NumericType<>b.NumericType then
		ChangeType(b);
	case a.NumericType of
		MATH_TYPE:	begin
									R:=ModulComplex(a)*ModulComplex(b);
									F:=(AngleComplex(a)+AngleComplex(b))*grad;
									Result.A:=R*Cos(F);
									Result.B:=R*Sin(F);
								end;
		EXP_TYPE:		begin
									Result.A:=a.A*b.A;
									Result.B:=a.B+b.B;
								end;
	end;
end;

function DivComplex(a,b:TComplex):TComplex; // Деление комплексного a, на комплексное b (в случае не соответствия видов хранения, вид хранения числа b меняется на такой как у a).
var
	R,F:Single;
begin
	if a.NumericType<>b.NumericType then
		ChangeType(b);
	case a.NumericType of
		MATH_TYPE:	begin
									R:=ModulComplex(a)/ModulComplex(b);
									F:=(AngleComplex(a)-AngleComplex(b))*grad;
									Result.A:=R*Cos(F);
									Result.B:=R*Sin(F);
								end;
		EXP_TYPE:		begin
									Result.A:=a.A/b.A;
									Result.B:=a.B-b.B;
								end;
	end;
end;

function MathComplex(a,b:Single):TComplex;  // Комплексное число задаётся в координатах на комплексной области
begin
	Result.A:=a;
	Result.B:=b;
	Result.NumericType:=MATH_TYPE;
end;

function PowerComplex(R,F:Single):TComplex; // Комплексное число задаётся в показательной форме
begin
	Result.A:=R;
	Result.B:=F*grad;
	Result.NumericType:=EXP_TYPE;
end;

function AddComplex(a,b:TComplex):TComplex; // Сложение (выдаваемый результат в математическом виде)
begin
	if a.NumericType=EXP_TYPE then
		ChangeType(a);
	if b.NumericType=EXP_TYPE then
		ChangeType(b);
	Result.A:=a.A+b.A;
	Result.B:=a.B+b.B;
end;

function SubComplex(a,b:TComplex):TComplex; // Вычитание из a вычитается b (выдаваемый результат в математическом виде)
begin
	if a.NumericType=EXP_TYPE then
		ChangeType(a);
	if b.NumericType=EXP_TYPE then
		ChangeType(b);
	Result.A:=a.A-b.A;
	Result.B:=a.B-b.B;
end;

function CreateComplex(a,b:Single; CType:TComplexType):TComplex;
begin
	case CType of
		MATH_TYPE:	Result:=MathComplex(a,b);
		EXP_TYPE:		Result:=PowerComplex(a,b);
	end;
end;

function ChangeType(A:TComplex):TComplex;
begin
	case A.NumericType of
		MATH_TYPE:	Result:=CreateComplex(ModulComplex(A), AngleComplex(A), EXP_TYPE);
		EXP_TYPE:	Result:=CreateComplex(A.A*Cos(A.B), A.A*Sin(A.B), MATH_TYPE);
	end;
end;

end.
