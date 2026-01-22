unit SimpleEvent;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , pthreads
  , Linux
  ;

type

  { TSimpleEvent }

  TSimpleEvent=class
  private
    FMtx:TPthreadMutex;
    Fcv:TCondVar;
    FState:Boolean;

  public
    constructor Create(BeginState:Boolean=False);
    destructor Destroy; override;

    function GetState:Boolean;
    procedure SetEvent;
    procedure ResetEvent;
    procedure WaitEvent;
    function TimedWaitEvent(sec, msec:Cardinal):Integer;
  end;


implementation

{ TSimpleEvent }

constructor TSimpleEvent.Create(BeginState: Boolean=False);
var
  cattr:Cardinal;
begin
  pthread_mutex_init(@Fmtx, nil);

  cattr:=2; // включаем в атрибутах сигнальной переменной зависимость от таймера CLOCK_MONOTONIC (по умолчанию это CLOCK_REALTIME)
  pthread_cond_init(@Fcv, @cattr);

  FState:=BeginState;
end;

destructor TSimpleEvent.Destroy;
begin
  pthread_mutex_destroy(@Fmtx);
  pthread_cond_destroy(@Fcv);
  inherited Destroy;
end;

function TSimpleEvent.GetState: Boolean;
begin
  pthread_mutex_lock(@Fmtx);
  Result:=FState;
  pthread_mutex_unlock(@Fmtx);
end;

procedure TSimpleEvent.SetEvent;
begin
  pthread_mutex_lock(@Fmtx);
  if not FState then begin
    FState:=True;
    pthread_cond_broadcast(@Fcv);
  end;
  pthread_mutex_unlock(@Fmtx);
end;

procedure TSimpleEvent.ResetEvent;
begin
  pthread_mutex_lock(@Fmtx);
  FState:=False;
  pthread_mutex_unlock(@Fmtx);
end;

procedure TSimpleEvent.WaitEvent;
begin
  pthread_mutex_lock(@Fmtx);
  if not FState then
    pthread_cond_wait(@Fcv, @Fmtx);
  pthread_mutex_unlock(@Fmtx);
end;

function TSimpleEvent.TimedWaitEvent(sec, msec: Cardinal): Integer; // Если функция возвращает не ноль, это означает что ожидание прервалось не по тому, что событие было выставлено в сигнальное состояние, а по какой-то другой причине
var                                                                 // Если функция вернула 0, значит дождались сигнального состояния объекта синхронизации
  tm:array [0..1] of Cardinal;
begin
  pthread_mutex_lock(@Fmtx);
  if not FState then begin
    clock_gettime(CLOCK_MONOTONIC, @tm); // Определяем текущее время, затем к немо будет добавлено указанное время (sec, msec)

    tm[0]+=(sec + msec div 1000);
    msec:=msec mod 1000; // таким образом миллисекунды будут в диапазоне 0-999
    tm[1]+=msec*1000000;
    Result:=pthread_cond_timedwait(@Fcv, @Fmtx, @tm); // парамет tm это абсолютное время до которого будет работать ожидание
  end;
  pthread_mutex_unlock(@Fmtx);
end;

end.




