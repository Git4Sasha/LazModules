unit DirWork;

interface

uses
	SysUtils;

type
	TDirElement=record // Элемент папки
  	FSize:Int64; // Размер файла
  	Attr:Integer; // Атрибут элемента
    ShortName:String; // Короткое имя ( без пути )
    FullName:String; // Полное имя
  end;
  TArrayOfDirElement=array of TDirElement; // Массив элементов папки

	TDirWork=class
  private
		FDirItems:TArrayOfDirElement; // Для хранения массива элементов

    procedure Clear; // Отчистка массива
    function GetCount: Integer; // Кол-во элементов в массиве
    function GetDirElement(Index: Integer): TDirElement;
  public
		destructor Destroy; override;
		function ReadDir(const Path: String):Integer; // Читаем элементы папки Path - путь к папке

    property Count:Integer read GetCount;
    property Items[Index:Integer]:TDirElement read GetDirElement; 
  end;

implementation

{ TDirWork }

procedure TDirWork.Clear; // Отчистка массива
var
	i:Integer;
begin
	for i:=0 to Length(FDirItems)-1 do
  	begin
			SetLength(FDirItems[i].ShortName, 0);
			SetLength(FDirItems[i].FullName, 0);
    end;
  SetLength(FDirItems, 0);
end;

destructor TDirWork.Destroy;
begin
	Clear;
  inherited;
end;

function TDirWork.GetCount: Integer;
begin
	Result:=Length(FDirItems);
end;

function TDirWork.GetDirElement(Index: Integer): TDirElement;
begin
	if (Index<0)or(Index>=Length(FDirItems)) then
  	raise Exception.Create('Индекс вне зоны обслуживания :) DirWork');
  Result:=FDirItems[Index];
end;

function TDirWork.ReadDir(const Path: String):Integer;
var
	sr:TSearchRec;
  s:String;
  n:Integer;
begin
	// Ф-я возвращает 0 - если папка пуста; 0 бит = 1 - Есть подкаталоги; 1 бит =1 - Есть файлы
	Result:=0;
	Clear; // Сначала отчищаем весь массив ( Если найдено ни чего не будет, то массив будет пуст )
	s:=ExtractFilePath(Path);
  if FindFirst(Path+'*', faAnyFile, sr)=0 then
		repeat
			if (sr.Name<>'.')and(sr.Name<>'..') then
      	begin
          n:=Length(FDirItems);
          SetLength(FDirItems, n+1);
          FDirItems[n].FSize:=sr.Size;
          FDirItems[n].Attr:=sr.Attr;
          FDirItems[n].ShortName:=sr.Name;
          FDirItems[n].FullName:=s+sr.Name;

          if (sr.Attr and faDirectory)<>0 then
            Result:=Result or 1 // Установка бита наличия каталогов
          else
            Result:=Result or 2 // Установка бита наличия файлов
        end;
    until FindNext(sr)<>0;
  FindClose(sr);
end;

end.
