unit FIFOBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , syncobjs

  ;

type

  { TFIFOBuffer }

  TFIFOBuffer=class
  private
    FBuff:array of Byte;
    FSize:Integer;
    FBuffFull:Boolean;                                      // Признак того, что буфер полон
    FHead:Integer;
    FTail:Integer;
    FCurentSize:Integer;
    FCritSec:TCriticalSection;

  public
    constructor Create(size:Integer);
    destructor Destroy; override;

    procedure ClearBuff(size:Integer=0);                    // Очистка и сброс буфера
    function FIFOWrite(buf:Pointer; sz:Integer):Integer;    // Функция записывает данные в буфер FIFO и возвращает код результата
    function FIFORead(buf:Pointer; sz:Integer):Integer;     // Функция читает данные из буфера FIFO и возвращает код результата
    function FIFOCurSize:Integer;                           // Текущий размер данных в буфере

    property FIFOSize:Integer read FSize;                   // Полный размер FIFO буфера
    property FIFOFull:Boolean read FBuffFull;               // Признак того, что буфер полон
  end;


implementation

{ TFIFOBuffer }

constructor TFIFOBuffer.Create(size: Integer);
begin
  // При создании объекта выделяется память для буфера, голова и хвост инициализируются, сохраняется размер выделенный для FIFO буфера
  ClearBuff(size);
  FCritSec:=TCriticalSection.Create;
end;

destructor TFIFOBuffer.Destroy;
begin
  inherited Destroy;
  FBuff:=nil;
  FCritSec.Free;
end;

procedure TFIFOBuffer.ClearBuff(size: Integer);  // Очистка и сброс буфера
begin
  // size - При сбросе буфера можно изменить его размер, если задать не нулевую длину

  if size<>0 then begin         // Если задан не нулевой новый размер, то выделяется память для буфера
    SetLength(FBuff, size);
    FSize:=size;
  end;
  FHead:=0;
  FTail:=0;
  FCurentSize:=0;
  FBuffFull:=False;
end;

function TFIFOBuffer.FIFOWrite(buf: Pointer; sz: Integer): Integer;  // Функция помещает данные в буфер FIFO и возвращает код результата
var
  nhp,fs:Integer;
  fsz:Integer;
begin
  // Если данные не помещаются в буфер, то функция вернёт -1
  // Если данные помещаются в буфер, то функция вернёт 0
  // На время всех операций с буфером и с полями, которые хранят положение головы, хвоста, текущего размера выполняется вход в критическую секцию

  Result:=-1;                       // Изначально предполагаем, что места в буфере нет

  FCritSec.Enter;

  repeat
    if FBuffFull then Break;          // Если буфер полон, то сразу выходим

    fs:=FTail-FHead;                  // Свободное пространство в буфере FIFO
    if fs<=0 then fs:=fs + FSize;     // Если получилось отрицательным, то просто добавляем размер буфера
    if sz>fs then Break;              // Данные не помещаются в буфер, прерываем цикл для выхода
    Result:=0;                        // Если данные помещаются в буфер, то копировать данные можно

    FBuffFull:=fs=sz;                 // Если кол-во записыаемых данных равно кол-ву свободных, то выставляется признак "Буфер полон"

    nhp:=FHead+sz;                    // Позиция головы, после копирования данных
    if nhp>FSize then begin           // Если позиция вылезает за пределы буфера, то нужно скопировать часть данных до конца буфера, а оставшуюся часть копировать с начала буфера
      fsz:=FSize-FHead;               // Кол-во байт до конца буфера
      Move(buf^, FBuff[FHead], fsz);  // Копирование части байт до конца буфера
      Inc(buf, fsz);                  // Сдвигаем указатель на то кол-во байт, которое скопированно
      Move(buf^, FBuff[0], sz-fsz);   // Копирование остальной части байт с начала буфера
      nhp:=nhp - FSize;               // Новая позиция головы
    end else begin                    // Иначе, если новая позиция головы не вылезает за пределы буфера, то можно смело копировать все данные
      Move(buf^, FBuff[FHead], sz);   // Копирование всех данных в буфер
    end;
    if FBuffFull then FHead:=FTail else FHead:=nhp; // Формирование новой позиции головы
    if FHead=FSize then FHead:=0;
  until True;

  FCritSec.Leave;
end;

function TFIFOBuffer.FIFORead(buf: Pointer; sz: Integer): Integer;  // Функция читает данные из буфера FIFO и возвращает код результата
var
  ntp,um:Integer;
  fsz:Integer;
begin
  // Если в буфере нет, запрашиваемого кол-ва данных, то функция вернёт -1
  // Если данные возможно считать в полном объёме, то функция вернёт 0
  // На время всех операций с буфером и с полями, которые хранят положение головы, хвоста, текущего размера, выполняется вход в критическую секцию

  Result:=-1;                       // Изначально предполагаем, что данных в буфере нет

  FCritSec.Enter;

  repeat
    if FBuffFull then
      um:=FSize
    else begin
      um:=FHead-FTail;                  // Кол-во данных, которое есть в FIFO буфере
      if um=0 then Break;               // Если данных в буфере нет, то считывать нечего
      if um<0 then um:=FSize + um;      // Если значение получилось отрицательным, то его нужно скорректировать
    end;

    if um<sz then Break;                // Если запрашиваемый размер больше кол-ва данных, которое есть в буфере, то выходим

    FBuffFull:=False;                   // Если из буфера хоть что-то считали, то он уже не полный
    Result:=0;                          // Если кол-во данных в буфере достаточно, то можно копировать данные из FIFO в переданный буфер

    ntp:=FTail+sz;                      // Позиция хвоста, после копирования данных
    if ntp>FSize then begin             // Если позиция вылезает за пределы буфера, то нужно скопировать часть данных до конца буфера, а оставшуюся часть копировать с начала буфера
      fsz:=FSize-FTail;                 // Кол-во байт до конца буфера
      Move(FBuff[FTail], buf^, fsz);    // Копирование части байт до конца буфера
      Inc(buf, fsz);                    // Сдвигаем указатель на то кол-во байт, которое скопированно
      Move(FBuff[0], buf^, sz-fsz);     // Копирование остальной части байт с начала буфера
      ntp:=ntp - FSize;                 // Новая позиция хвоста
    end else begin                      // Иначе, если новая позиция головы не вылезает за пределы буфера, то можно смело копировать все данные
      Move(FBuff[FTail], buf^, sz);     // Копирование всех данных в буфер
    end;
    FTail:=ntp;                         // Формирование новой позиции хвоста
    if FTail=FSize then FTail:=0;       // Если новое положение хвоста равно размеру буфера, то положение хвоста нужно обнулить
  until True;

  FCritSec.Leave;
end;

function TFIFOBuffer.FIFOCurSize: Integer;   // Функция возвращает текущий размер данных в FIFO буфере
begin
  FCritSec.Enter;

  if FBuffFull then
    Result:=FSize
  else begin
    Result:=FHead-FTail;                          // Кол-во данных, которое есть в FIFO буфере
    if Result<0 then Result:=Result + FSize;      // Если значение получилось отрицательным, то его нужно скорректировать
  end;

  FCritSec.Leave;
end;

end.

