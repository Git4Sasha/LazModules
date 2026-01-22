unit ShareMemForProcess;

interface

uses
  ipc;

type

  { TShareMemProcess }

  TShareMemProcess=class
  private
    FShmID:Integer;
    FSize:Cardinal;
    FPointer:Pointer;
    FError:Integer;
  public
    constructor Create(key:Cardinal; size:Cardinal);
    destructor Destroy; override;

    property SMSize:Cardinal read FSize; //  Размер общей памяти
    property Addr:Pointer read FPointer; // Адрес начала общей памяти
    property Error:Integer read FError;
  end;

implementation


{ TShareMemProcess }

constructor TShareMemProcess.Create(key: Cardinal; size: Cardinal);
begin
  FError:=0;
  FShmID:=ShmGet(key, size, IPC_CREAT or &600);
  if FShmID=-1 then
    begin
      FError:=-1;
      Exit;
    end;
  FPointer:=ShmAt(FShmID, nil, 0); // Получаем адрес общей памяти
  if FPointer<>nil then FSize:=size else FError:=-2;
end;

destructor TShareMemProcess.Destroy;
var
  ds:TShmid_ds;
begin
  ShmDt(FPointer);
  ShmCtl(FShmID, IPC_STAT, @ds); // получаем состояние общего ресурса (для определения кол-ва процессов подключённых к общему ресурсу)
  if ds.shm_nattch=0 then // Если кол-во подключённых к общему ресурсу процессов стало равным нулю, то удаляем общую память
    ShmCtl(FShmID, IPC_RMID, nil);
  inherited Destroy;
end;


end.
