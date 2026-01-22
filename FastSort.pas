unit FastSort;
// Модуль выполняем операцию быстрой сортировки
// Для выполнения процедуры сортировки необходимо
// задать процедуру для сравнения элементов массива - CompareFunc
// задать процедуру для перестановки элементов массива - SwapFunc
// запустить процедуру StartSort, в которую необходимо передать количество элементов массива и адрес на массив

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // a,b - номера элементов с которыми нужно выполнить операцию
  TCompareFunc=function(a,b:Integer; mass:Pointer):Integer of object; // Функция сравнения (a<b=-1; a=b=0; a>b=1)
  TSwapFunc=procedure(a,b:Integer; mass:Pointer) of object;  // Функция для перестановки элементов местами

  { TFastSort }

  TFastSort=class
  private
    FOnCompareFunc:TCompareFunc;
    FOnSwapFunc:TSwapFunc;
    procedure Sort(FirstElem, EndElem:Integer; mass:Pointer);  // Процедура сортировки
  public
    procedure StartSort(Count:Integer; mass:Pointer);  // Процедура сортировки
    property OnCompareFunc:TCompareFunc read FOnCompareFunc write FOnCompareFunc;
    property OnSwapFunc:TSwapfunc read FOnSwapFunc write FOnSwapFunc;
  end;

implementation

{ TFastSort }

procedure TFastSort.Sort(FirstElem, EndElem: Integer; mass: Pointer);
var
  m,k,mid:Integer;
begin
  // FirstElem - первый элемент в массиве, который необходимо отсортировать
  // EndElem - последний элемент в массиве, который необходимо отсортировать;
  k:=FirstElem; // тут будет хранится элемент слева от среднего
  m:=EndElem;   // тут будет хранится элемент справа от среднего
  mid:=(k+m) div 2; // номер среднего элемента
  repeat
    while FOnCompareFunc(mid, m, mass)>0 do Dec(m); // Поиск элемента, который меньше выбранного среднего значения
    while FOnCompareFunc(k, mid, mass)>0 do Inc(k); // Поиск элемента, который больше выбранного среднего значения

    // значение в ячейке mid нельзя терять, поэтому если значение в ячейке mid перемещается в другую ячейку, то
    // значение mid нужно изменить на новую ячейку
    if m=mid then mid:=k else if k=mid then mid:=m;
    if k<=m then
      begin
        FOnSwapFunc(m,k, mass);
        Inc(k); // Перемещаемся на номер элемента, который ближе к среднему (двигаемся слева направо)
        Dec(m); // Перемещаемся на номер элемента, который ближе к среднему (двигаемся справо на лево)
      end;
  until k>m;
  if FirstElem<m then Sort(FirstElem, m, mass);
  if k<EndElem then Sort(k, EndElem, mass);
end;

procedure TFastSort.StartSort(Count: Integer; mass: Pointer);
begin
  // Если не назначены процедуры для сравнения и для перестановки элементов, то сортировку делать нет смысла
  if not Assigned(FOnCompareFunc) then Exit;
  if not Assigned(FOnSwapFunc) then Exit;
  if mass=nil then Exit; // если не задан массив элементы которого необходимо отсортировать, то выходим
  if Count=0 then Exit;
  Sort(0, Count-1, mass);
end;

end.

