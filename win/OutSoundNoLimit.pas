unit OutSoundNoLimit;

interface

uses
  Classes, MMSystem, Windows;

type
	TRightLeftVal=Record
  	Rigth:SmallInt;
    Left:SmallInt;
  end;
  PRightLeftVal=^TRightLeftVal;

  TOutSoundNoLimit = class(TThread)
  private
  	FCurentBuffer:Integer; // Номер текущего буфера
    FSampleCount:Cardinal; // Кол-во отсчётов в блоке ( Вывод звука идёт блоками )
		FFrequency:Cardinal; // Частота дискретизации сигнала по времени
    FBufferHead:Array [0..1] of TWaveHdr; // Массив для хранения информации буферах для устройства вывода звука
    FWaveOut:hWaveIn; // Идентификатор открытого устройства воспроизведения звука
    FPlayNow:Boolean; // Признак того, что идёт воспроизведение, пока идёт запись нельза запускать процедуру StartPlay
    FLastError:MMRESULT; // Переменная хранит ошибку работы со звуком ( 0 - нет ошибок перечень значений в MMSystem.pas )
    FOnGetData:TNotifyEvent; // Переменная процедура ( см. св-во OnGetData )

    procedure SetSampleCount(const Value: Cardinal);
    function GetData(Index: Cardinal): TRightLeftVal;
    procedure SetData(Index: Cardinal; const Value: TRightLeftVal);
    function GetBuffer: TWaveHdr;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartPlay;
    procedure StopPlay;

    property Frequency:Cardinal read FFrequency write FFrequency;
    property SampleCount:Cardinal read FSampleCount write SetSampleCount; // Св-во для определения/устаноки объёма блока воспроизводимых данных
    property Data[Index:Cardinal]:TRightLeftVal read GetData write SetData; // Для заполнения буфера, который мы будем воспроизводить ( св-вом можно пользоваться только после установки SampleCount )
    property Buffer:TWaveHdr read GetBuffer; // Для более прямого доступа к буферу ( св-вом можно пользоваться только после установки SampleCount )
    property LastError:Cardinal read FLastError; // Св-во возвращает последнюю возникшую ошибку
    property OnGetData:TNotifyEvent read FOnGetData write FOnGetData; // Вызывается когда необходимо записать очередные данные в буфер
    property PlayOn:Boolean read FPlayNow; // Признак того, что процесс воспроизведения включен
  end;

implementation

{ TOutSoundNoLimit }

constructor TOutSoundNoLimit.Create;
begin
	FBufferHead[0].lpData:=nil;
	FBufferHead[1].lpData:=nil;
  FFrequency:=44100; // Частота дискретизации сиг-ла по времени по умолчанию
  FPlayNow:=False; // На данный момент нет воспроизведения, значт можно начинать воспроизведение
  FreeOnTerminate:=True; // Память под переменную потока освобождается сразу как останавливается поток
  inherited Create(True); // Создаём поток без его запуска
end;

destructor TOutSoundNoLimit.Destroy;
begin
  ReallocMem(FBufferHead[0].lpData, 0);
  ReallocMem(FBufferHead[1].lpData, 0);
  inherited;
end;

procedure TOutSoundNoLimit.Execute;
begin
	Priority:=tpTimeCritical; // Устанавливается максимальный приоритет потоку для того, чтобы максимально быстро заполнять данные, т.к. FOnGetData(Self) будет выполняться в рамках потока
	FCurentBuffer:=0; // Задаём номер текущего буфера
  // Если есть обработка события записи очередных данных в буфер ( данная обработка должна быть, иначе будет воспроизводиться "мусор" )
 	if Assigned(FOnGetData) then
  	FOnGetData(Self); // Заполняем нулевой буфер данными
  waveOutWrite(FWaveOut, @FBufferHead[FCurentBuffer], SizeOf(TWaveHdr)); // Ставим в очередь на воспроизведение нулевой буфер
  repeat
  	FCurentBuffer:=1-FCurentBuffer; // Меняем номер буфера
    // Заполнение пользователем очередного блока ( Заполнить данные необходимо быстрее чем проигрывается текущий блок )
    if Assigned(FOnGetData) then
      FOnGetData(Self);
		if FPlayNow then
    	waveOutWrite(FWaveOut, @FBufferHead[FCurentBuffer], SizeOf(TWaveHdr)); // Ставим в очередь на воспроизведение текущий буфер
		repeat // Ждём окончания воспроизведения
    	Sleep(1); // Это сделанно для того, чтобы не было 100% загрузки процессора
    until (FBufferHead[1-FCurentBuffer].dwFlags and WHDR_DONE<>0) or (not FPlayNow); // Выйдет когда будет установлен флаг готовности
  until not FPlayNow; // Выйти когда признак воспроизведения сброшен ( признак сбрасывается вызовом процедуры StopPlay )
	FLastError:=waveOutReset(FWaveOut); // Сбрасываем все добавленные буферы
	FLastError:=WaveOutUnPrepareHeader(FWaveOut, @FBufferHead[0], SizeOf(TWaveHdr)); // Освобождение первого буфера
	FLastError:=WaveOutUnPrepareHeader(FWaveOut, @FBufferHead[1], SizeOf(TWaveHdr)); // Освобождение второго буфера
	FLastError:=WaveOutClose(FWaveOut); // Закрытие устройства воспроизведения
end;

function TOutSoundNoLimit.GetBuffer: TWaveHdr;
begin
  Result:=FBufferHead[FCurentBuffer];
end;

function TOutSoundNoLimit.GetData(Index: Cardinal): TRightLeftVal;
var
	p:PRightLeftVal;
begin
	if Index<FSampleCount then
  	begin
			p:=Pointer(FBufferHead[FCurentBuffer].lpData);
      Inc(p, Index);
			Result:=p^;
    end;
end;

procedure TOutSoundNoLimit.SetData(Index: Cardinal;
  const Value: TRightLeftVal);
var
	p:PRightLeftVal;
begin
	if Index<FSampleCount then
  	begin
			p:=Pointer(FBufferHead[FCurentBuffer].lpData);
      Inc(p, Index);
			p^:=Value;
    end;
end;

procedure TOutSoundNoLimit.SetSampleCount(const Value: Cardinal);
begin
  // FSampleCount - Кол-во отсчётов
  FSampleCount:=Value;
  // Выделение памяти для первого буфера
  with FBufferHead[0] do
    begin
      dwBufferLength:=FSampleCount*4;
      ReallocMem(lpData, dwBufferLength); // Один отсчёт занимает 4 байта. 2 байта ( 16 Бит ) на канал, 2 канала
    end;
  // Выделение памяти для второго буфера
  with FBufferHead[1] do
    begin
      dwBufferLength:=FBufferHead[0].dwBufferLength;
      ReallocMem(lpData, dwBufferLength); // Один отсчёт занимает 4 байта. 2 байта ( 16 Бит ) на канал, 2 канала
    end;
end;

procedure TOutSoundNoLimit.StartPlay;
var
  WaveHeader:TWaveFormatEx; //Описатель формата захватываемых данных ( кол-во каналов, частота ... )
begin
  FLastError:=0;
  if FPlayNow then // Если идёт воспроизведение, то
    begin
      FLastError:=MMSYSERR_ERROR; // Занят воспроизведением
      Exit; // выходим
    end;
  // Заполнение формата выводимых данных
  with WaveHeader do
    begin
     wFormatTag := WAVE_FORMAT_PCM; // Звук без сжатия
     nChannels:=2;  // количество каналов
     nSamplesPerSec:=Frequency; // частота
     wBitsPerSample:=16; // Кол-во бит в семпле
     nBlockAlign:=4; // Выравнивание блоков
     nAvgBytesPerSec:=nSamplesPerSec*wBitsPerSample*nChannels shr 3; // Среднее кол-во байт в секунду
     cbSize:=0;
    end;
  FLastError:=WaveOutOpen(@FWaveOut, 0, @WaveHeader, 0, 0, CALLBACK_NULL); // Открытие устройства воспроизведения
  FLastError:=FLastError or WaveOutPrepareHeader(FWaveOut, @FBufferHead[0], SizeOf(TWaveHdr)); // Подготовка первого буфера для загрузки данными ( Выполняется один раз перед началом воспроизведения )
  FLastError:=FLastError or WaveOutPrepareHeader(FWaveOut, @FBufferHead[1], SizeOf(TWaveHdr)); // Подготовка второго буфера для загрузки данными ( Выполняется один раз перед началом воспроизведения )
  if FLastError=0 then // Если ошибок нет, то
    begin
      FPlayNow:=True; // Выставляем признак "идёт воспроизведение"
      Resume; // запускаем воспроизведение
    end;
end;

procedure TOutSoundNoLimit.StopPlay;
begin
  FPlayNow:=False;
end;

end.
