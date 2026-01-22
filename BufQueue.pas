unit BufQueue;

// Модуль для создания очереди буферов памяти.
// Каждый элемент очереди имеет фиксированный размер, который задаётся при создании очереди

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , RingBufHelper
  , ArrayOfTypes
  , SyncObjs

  ;

type

  { TBufQueue }

  TBufQueue=class
  private
    FRB4Buf:TRingBufHelper;       // Помошник для работы с кольцевым буфером
    FBuffers:T2DArrayOfByte;      // Массив, который хранит буферы в которые сохраняются данные, помещаемые в очередь
    FBufLens:TArrayOfInteger;     // Массив длин сообщиний
    FPushEvent:TEvent;            // Объект - событие, устанавливается в том случае, если есть возможность поместить данные в очередь
    FPopEvent:TEvent;             // Объект - событие, устанавливается в том случае, если есть возможность извлечь данные из очередь
    FMaxDataLen:Integer;          // Максимальный размер данных, который можно записать в элемент очереди
  public

    constructor Create(ql,bs:Integer); // ql - размер очереди; bs - размер буфера для каждого элемента очереди
    destructor Destroy; override;
    function PushData(buf:Pointer; len,tmout:Integer):Integer;  // Помещение данных в очередь с ограничением по времени
    function PopData(buf:Pointer; len,tmout:Integer):Integer;   // Извлечение данных из очереди с ограничением по времени
    procedure ResetQuery;                                       // Сброс очереди
  end;


implementation

{ TBufQueue }

constructor TBufQueue.Create(ql, bs: Integer);
begin
  // ql - Длина очереди
  // bs - Размер буфера для каждого элемента очереди
  // nevt - Признак необходимости создавать событие, которое можно изпользовать по своему усмотрению

  FMaxDataLen:=bs;                                    // Фиксация максимального размера данных, помещаемых в очередь
  FRB4Buf:=TRingBufHelper.Create(ql);                 // Создание помошника для работы с очередями
  SetLength(FBuffers, ql, bs);                        // Двумерный массив для хранения данных очереди
  SetLength(FBufLens, ql);                            // Массив, который будет сохранять длины данных в ячейках очереди
  FPushEvent:=TEvent.Create(nil, False, True, '');    // Объект - событие, устанавливается в том случае, если есть возможность поместить данные в очередь
  FPopEvent:=TEvent.Create(nil, False, False, '');    // Объект - событие, устанавливается в том случае, если есть возможность извлечь данные из очередь
end;

destructor TBufQueue.Destroy;
begin
  inherited Destroy;

  FBuffers:=nil;
  FRB4Buf.Free;
  FBufLens:=nil;
  FPushEvent.Free;
  FPopEvent.Free;
end;

function TBufQueue.PushData(buf: Pointer; len, tmout: Integer): Integer;  // Помещение данных в очередь с ограничением по времени
var
  wi:Integer;
begin
  // buf    - это източник данных
  // len    - кол-во данных, которое сохраняется в ячейку очереди
  // tmout  - предел ожидания возможности поместить данные в очередь
  // Возвращает -1, если данные не удалось добавить в очередь за указанное вермя

  Result:=-1;                         // Это будет означать, что данные в очеред помещены не были
  if not FRB4Buf.CanWrite then begin  // Если нет возможности поместить данные, в очередь, то
    if tmout=0 then                   // Если время ожидания равно нулю, то сразу выходим из функции
      Exit
    else
      FPushEvent.WaitFor(tmout);        // запускается ожидание события возможности записи с ограничением по времени, если событие установится раньше указанного лимита, то
  end;
  if not FRB4Buf.CanWrite then Exit;  // новый вызов FRB4Buf.CanWrite должен привести к положительному результату, иначе опять будет False и на этот раз выход из функции
  FPushEvent.ResetEvent;              // Сброс события не зависимо от того было оно установлено или нет
  wi:=FRB4Buf.Index4Write;
  Result:=len;
  if Result>FMaxDataLen then Result:=FMaxDataLen; // Ограничение размера записываемых данных
  Move(buf^, FBuffers[wi,0], Result);             // Копирование переданных данных во внутренний буфер
  FBufLens[wi]:=Result;                           // Сохранение длины данных, которые сохраняются в данной ячейке очереди
  FRB4Buf.EndWrite;                               // Эту функцию необходимо вызывать, если был успешный вызов функции FRB4Buf.CanWrite
  FPopEvent.SetEvent;                             // Установка события, которое означает, что данные в буфере есть, т.е. чтение возможно
end;

function TBufQueue.PopData(buf: Pointer; len, tmout: Integer): Integer; // функция вернёт -1, если данные не были прочитанны, и значение больше нуля, которое означает сколько реально данных было считанно
var
  ri:Integer;
begin
  // buf    - это приёмник данных
  // len    - кол-во данных, которое считывается из очереди
  // tmout  - предел ожидания возможности получить данные из очередь

  Result:=-1;                         // Сначала предполагается, что данных не будет
  if not FRB4Buf.CanRead then         // Если нет возможности чтения данных, то
    FPopEvent.WaitFor(tmout);         // запускается ожидание события возможности чтения с ограничением по времени, если событие установится раньше указанного лимита, то
  if not FRB4Buf.CanRead then Exit;   // новый вызов FRB4Buf.CanRead должен привести к положительному результату, иначе опять будет False и на этот раз выход из функции
  FPopEvent.ResetEvent;               // Сброс события не зависимо от того было оно установлено или нет
  ri:=FRB4Buf.Index4Read;
  Result:=FBufLens[ri];
  if Result>len then Result:=len;     // Если запрошено меньше данных, чем сохранено в очереди, то размер считывания ограничивается
  Move(FBuffers[ri,0], buf^, Result); // Копирование в буфер данных из очереди
  FBufLens[ri]:=0;                    // Размер данных в данной ячейке очереди обнуляется
  FRB4Buf.EndRead;                    // Эту функцию необходимо вызывать после успешного вызова функции FRB4Buf.CanRead
  FPushEvent.SetEvent;                // т.к. из очереди удалили один элемент, то есть возможность поместить в очередь элемент
end;

procedure TBufQueue.ResetQuery;
begin
  FRB4Buf.Clear;                                    // Сброс кольцевого буфера
  FillDWord(FBufLens[0], Length(FBufLens), 0);      // Обнуление массива с длинами
end;

end.

