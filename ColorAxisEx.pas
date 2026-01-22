unit ColorAxisEx;

interface

uses SysUtils, ArrayOfTypes;

type
	TColorValue=packed record
		Value:Double;
		Color:TValue4b;
	end;
  TArrayOfColorValue=array of TColorValue;

  { TColorAxisEx }

 TColorAxisEx=class
  private
    FPoints:TArrayOfColorValue;             // Массив точек, который хранит изходные величины и цвета, привязанные к этим величинам
    FScaledValue:TArrayOfDouble;            // Массив в котором изходные величины перезчитаны в диапазон от 0 до 1 с учётом минимального и максимального значения
    FNumPoint:Integer;
    FMaxValue: Double;
    FMinValue: Double;
    FSorted:Boolean;

    procedure Sort;
    procedure ScaleValues;                 // Процедура подгоняет значения в парах под диапазон FMinValue FMaxValue и сохраняет значения в FScaledValue
    function GetColorValue(Index: Integer): TColorValue;
    function GetScaledValue(Index: Integer): Double;
    procedure SetMaxValue(const Value: Double);
    procedure SetMinValue(const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;                                                        // Очистка массива значений и цвета
    procedure AddPoint(colval:TColorValue);                                 // Добавление величины привязанной к цвету ( не забудь вызвать UpdateColVal после всех вызовов AddPoint)
    procedure AddPoint(Value:Double; Color:TValue4b);
    procedure AddPoint(Value:Double; r,g,b:Byte);
    procedure AddPoint(Value:Double; r,g,b,a:Byte);
    procedure UpdateColVal;                                                 // Процедуру нужно запускать после того, как были добавлены новые значения и привязанные к ним цвета
    procedure SetMinMax(minmax:TValue2f);                                   // Установка минимального и максимального значений в цветной оси (процедура автоматически запускает процедуру UpdateColVal)
    function ValueToColor(Value:Double):TValue4b;
    procedure ValueToColor(Value:Double; out R,G,B:Byte);
    property ColorPoints[Index:Integer]:TColorValue read GetColorValue;
    property ScaledValue[Index:Integer]:Double read GetScaledValue;         // Массив значений отнормированный к 1
    property Count:Integer read FNumPoint;
    property MinValue:Double read FMinValue write SetMinValue;
    property MaxValue:Double read FMaxValue write SetMaxValue;
  end;


implementation

{ TColorAxis }

procedure TColorAxisEx.AddPoint(colval: TColorValue);
begin
  SetLength(FPoints, FNumPoint+1);
	FPoints[FNumPoint]:=colval;
	Inc(FNumPoint);
  FSorted:=False;
end;

procedure TColorAxisEx.AddPoint(Value: Double; Color: TValue4b);
begin
  SetLength(FPoints, FNumPoint+1);
	FPoints[FNumPoint].Color:=Color;
	FPoints[FNumPoint].Value:=Value;
	Inc(FNumPoint);
  FSorted:=False;
end;

procedure TColorAxisEx.AddPoint(Value: Double; r, g, b: Byte);
begin
  AddPoint(Value, Value4b(r,g,b,255));
end;

procedure TColorAxisEx.AddPoint(Value: Double; r, g, b, a: Byte);
begin
  AddPoint(Value, Value4b(r,g,b,a));
end;

procedure TColorAxisEx.UpdateColVal;
begin
  Sort;         // Сортировка пар "значение-цвет" по возрастанию значения
  ScaleValues;  // Формирование массива FScaledValue (этот массив содержит значения отнормированные к 1)
end;

procedure TColorAxisEx.SetMinMax(minmax: TValue2f);
begin
  if minmax.Min>=minmax.Max then Exit;
  FMaxValue:=minmax.Max;
  FMinValue:=minmax.Min;
  UpdateColVal;
end;

constructor TColorAxisEx.Create;
begin
  FNumPoint:=0;
	FPoints:=nil;
  FMaxValue:=1;
  FMinValue:=0;
  FScaledValue:=nil;
  AddPoint(0, 0,0,0,255);
  AddPoint(1, 255,255,255,255);
  UpdateColVal;
end;

destructor TColorAxisEx.Destroy;
begin
	FNumPoint:=0;
	FPoints:=nil;
  FScaledValue:=nil;
end;

procedure TColorAxisEx.Clear;
begin
	FNumPoint:=0;
	FPoints:=nil;
  FScaledValue:=nil;
  FSorted:=False;
end;

function TColorAxisEx.GetColorValue(Index: Integer): TColorValue;
begin
	if (Index>=0)and(Index<FNumPoint) then Result:=FPoints[Index];
end;

procedure TColorAxisEx.ScaleValues;  // Эта процедура должна запускаться только после сортировки массива пар "значение-цвет"
var
	deluser,delmass,k:Double;
  i:Integer;
begin
  // После этой процедуры в массиве FScaledValue будут значения из массива пар "значение-цвет" отмасштабированные на заданные минимальные и максимальные значения

  SetLength(FScaledValue, FNumPoint);
  delmass:=FPoints[FNumPoint-1].Value-FPoints[0].Value;
  deluser:=FMaxValue-FMinValue;
  k:=deluser/delmass;
  for i:=0 to FNumPoint-1 do
	 	FScaledValue[i]:=FMinValue+(FPoints[i].Value-FPoints[0].Value)*k;
  FScaledValue[0]:=FMinValue;
  FScaledValue[FNumPoint-1]:=FMaxValue;
end;

procedure TColorAxisEx.SetMaxValue(const Value: Double);
begin
  FMaxValue:=Value;
  UpdateColVal;
end;

procedure TColorAxisEx.SetMinValue(const Value: Double);
begin
  FMinValue:=Value;
  UpdateColVal;
end;

procedure TColorAxisEx.Sort; // Процедура сортирует пары "значение-цвет" по возрастанию значения
var
	AllSorted:Boolean;
	i:Integer;

  procedure SwapData(n:Integer);    // Обмен данными между соседними ячейками
  var
    Work:TColorValue;
  begin
    Work:=FPoints[n];
    FPoints[n]:=FPoints[n+1];
    FPoints[n+1]:=Work;
  end;

begin
  if FSorted then Exit;
  // Для сортировки изпользуется алгоритм пузырька
  if FNumPoint=2 then begin                                   // Если добавлено только 2 цвета, то сортировка сводится к одному действию
	  if FPoints[0].Value>FPoints[1].Value then SwapData(0);
  end else
    repeat
		  AllSorted:=True;
		  for i:=0 to FNumPoint-2 do
			  if FPoints[i].Value>FPoints[i+1].Value then begin
				  AllSorted:=False;
          SwapData(i);
			  end;
	  until AllSorted;
  FSorted:=True;
end;

function TColorAxisEx.GetScaledValue(Index: Integer): Double;
begin
	if (Index>=0)and(Index<FNumPoint) then Result:=FScaledValue[Index];
end;

function TColorAxisEx.ValueToColor(Value: Double): TValue4b;
var
  v4b0, v4b:TValue4b;
  NPoint:Integer;
  dRi,dGi,dBi,dAi:Integer;
begin
	if Value>=FMaxValue then begin  // Если значение больше максимального, то берётся цвет, который соответствует максимальному значению
    Result:=FPoints[FNumPoint-1].Color;
		Exit;
	end;
	if Value<=FMinValue then begin  // Если значение меньше минимального, то цвет соответствует минимальному значению
    Result:=FPoints[0].Color;
		Exit;
	end;
  NPoint:=0;
  while FScaledValue[NPoint]<Value do Inc(NPoint);
	Value:=(Value-FScaledValue[NPoint-1])/(FScaledValue[NPoint]-FScaledValue[NPoint-1]);   // Коэффициент для линейной интерполяции между соседними цветами

  v4b0:=FPoints[NPoint-1].Color;
  v4b:=FPoints[NPoint].Color;

  dRi:=v4b.rv-v4b0.rv;
	dGi:=v4b.gv-v4b0.gv;
	dBi:=v4b.bv-v4b0.bv;
  dAi:=v4b.av-v4b0.av;

	Result.rv:=Trunc(v4b0.rv + dRi*Value);
	Result.gv:=Trunc(v4b0.gv + dGi*Value);
	Result.bv:=Trunc(v4b0.bv + dBi*Value);
	Result.av:=Trunc(v4b0.av + dAi*Value);
end;

procedure TColorAxisEx.ValueToColor(Value: Double; out R, G, B: Byte);
var
  v4b0, v4b:TValue4b;
  NPoint:Integer;
  dRi,dGi,dBi:Integer;
begin
	Sort;  // Сортировка значений цветовой оси по возрастанию (если сортировка была выполнена ранее, то опять она выполняться не будет)
	if Value>=FMaxValue then begin  // Если значение больше максимального, то берётся цвет, который соответствует максимальному значению
    v4b:=FPoints[FNumPoint-1].Color;
    R:=v4b.rv;
    G:=v4b.gv;
    B:=v4b.bv;
		Exit;
	end;
	if Value<=FMinValue then begin  // Если значение меньше минимального, то цвет соответствует минимальному значению
    v4b:=FPoints[0].Color;
    R:=v4b.rv;
    G:=v4b.gv;
    B:=v4b.bv;
		Exit;
	end;
  NPoint:=0;
  while FScaledValue[NPoint]<Value do Inc(NPoint);
	Value:=(Value-FScaledValue[NPoint-1])/(FScaledValue[NPoint]-FScaledValue[NPoint-1]);

  v4b0:=FPoints[NPoint-1].Color;
  v4b:=FPoints[NPoint].Color;

  dRi:=v4b.rv-v4b0.rv;
	dGi:=v4b.gv-v4b0.gv;
	dBi:=v4b.bv-v4b0.bv;

	R:=Trunc(v4b0.rv + dRi*Value);
	G:=Trunc(v4b0.gv + dGi*Value);
	B:=Trunc(v4b0.bv + dBi*Value);
end;

end.
