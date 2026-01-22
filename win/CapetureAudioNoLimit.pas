unit CapetureAudioNoLimit;

interface

uses
  Classes, MMSystem, Windows, SimpleThread, ArrayOfTypes;

type
  { TCapetureAudioNoLimit }

  TCapetureAudioNoLimit = class
  private
    FThread4Rec:TSimpleThread;
    FCurentBuffer:Integer; // Номер текущего буфера
    FSampleCount:Cardinal; // Кол-во отсчётов, которое мы записывали при запуске процедуры StartRecord
		FFrequency:Cardinal; // Частота дискретизации сигнала по времени
    FBufferHead:array [0..1] of TWaveHdr; // Массив для хранения информации о буферах для устройства захвата звука
    FWaveIn:hWaveIn; // Идентификатор открытого устройства захвата звука
    FRecordNow:Boolean; // Признак того, что идёт запись, пока идёт запись нет смысла запускать процедуру StartRecord
    FLastError:MMRESULT; // Переменная хранит ошибку работы со звуком ( 0 - нет ошибок перечень значений в MMSystem.pas )
    FOnDataIn:TNotifyEvent; // Переменная процедура ( см. св-во OnDataIn )
    FDataFillEvent:THandle; // Объект собитие

    procedure ThFunc4Rec(Sender:TObject);
    function GetData(Index: Cardinal): TValue4b; // Данным свойством можно пользоваться только после прихода события OnDataIn
    function GetBuffer: TWaveHdr; // Данным свойством можно пользоваться только после прихода события OnDataIn

  public
    constructor Create;
    destructor Destroy; override;
    procedure StartRecord(SampleCount:Cardinal);
    procedure StopRecord;

    property Frequency:Cardinal read FFrequency write FFrequency; // Частота, должна задаваться до запуска процедуры StartRecord
    property Data[Index:Cardinal]:TValue4b read GetData; // Данные полученные от устройства записи
		property Buffer:TWaveHdr read GetBuffer; // Прямой указатель на заголовок буфера для удобного копирования его в файл к примеру
    property SampleCount:Cardinal read FSampleCount; // Кол-во записываемых отсчётов задаётся как параметр в процедуре StartRecord
    property LastError:MMRESULT read FLastError; // Св-во возвращает последнюю возникшую ошибку
    property OnDataIn:TNotifyEvent read FOnDataIn write FOnDataIn; // Вызывается когда пришли данные от записывающего устройства
    property RecordOn:Boolean read FRecordNow; // Свойство признак того, что включена запись
  end;

implementation


{ TCapetureAudio }

constructor TCapetureAudioNoLimit.Create;
begin
	FBufferHead[0].lpData:=nil;
	FBufferHead[1].lpData:=nil;
  FFrequency:=44100; // Частота дискретизации сиг-ла по времени по умолчанию
  FRecordNow:=False; // Признак записи сброшен, можно начинать запись
end;

destructor TCapetureAudioNoLimit.Destroy;
begin
  inherited;
  ReallocMem(FBufferHead[0].lpData, 0);
  ReallocMem(FBufferHead[1].lpData, 0);
end;

procedure TCapetureAudioNoLimit.ThFunc4Rec(Sender: TObject);
var
  thobj:TSimpleThread;
begin
  thobj:=TSimpleThread(Sender);
	thobj.Priority:=tpTimeCritical; // Устанавливается максимальный приоритет потоку для того, чтобы максимально быстро обработать данные, т.к. FOnDataIn(Self) будет выполняться в рамках потока

  FCurentBuffer:=0;
	FLastError:=WaveInAddBuffer(FWaveIn, @FBufferHead[0], SizeOf(TWaveHdr)); // Установка буфера в очередь на загрузку данными
	FLastError:=WaveInAddBuffer(FWaveIn, @FBufferHead[1], SizeOf(TWaveHdr)); // Установка буфера в очередь на загрузку данными
  FLastError:=WaveInStart(FWaveIn); // Начало захвата данных с устройства записи
  ResetEvent(FDataFillEvent); // Сброс сигнального события
  repeat
    WaitForSingleObject(FDataFillEvent, 500); // Ожидание готовности данных (событие установится само, когда будет заполнен буфер (см. StartRecord))
    if thobj.CheckTerminated then Break; // Прерываем поток, если необходимо остановиться
    if FBufferHead[FCurentBuffer].dwFlags and WHDR_DONE=0 then // Если данные не заполнены, то
      Continue; // Опять ждём данные
    ResetEvent(FDataFillEvent); // Сброс сигнального события (теперь оно станет сигнальным, когда заполнится следующий буфер, поставленый в очередь)
    // Заполнение первого буфера закончено, заполнение 2-го начнётся сразу после первого т.к. ранее он был установлен в очередь
    // Пока заполняется очередной буфер, мы должны успеть обработать только что заполненый

		if not thobj.CheckTerminated then begin
  		if Assigned(FOnDataIn) then
	  		FOnDataIn(Self); // Обработка полученных данных. Обработка не должна быть дольше набора данных, т.к. будут потери ( В качестве простейшего обработчика можно просто вывести данные на график )
      FLastError:=WaveInAddBuffer(FWaveIn, @FBufferHead[FCurentBuffer], SizeOf(TWaveHdr)); // Установка буфера в очередь на загрузку данными
      FCurentBuffer:=1-FCurentBuffer; // В качестве основного теперь будет выступать другой буфер
		end;
  until thobj.CheckTerminated; // Выйти когда пользователь закрывает поток
	FLastError:=waveInReset(FWaveIn); // Сбрасываем все добавленные буферы
	FLastError:=WaveInUnPrepareHeader(FWaveIn, @FBufferHead[0], SizeOf(TWaveHdr)); // Освобождение первого буфера
	FLastError:=WaveInUnPrepareHeader(FWaveIn, @FBufferHead[1], SizeOf(TWaveHdr)); // Освобождение второго буфера
	FLastError:=WaveInClose(FWaveIn); // Закрытие устройства записи
  CloseHandle(FDataFillEvent);  // Закрываем сигнальный объект
  FRecordNow:=False; // Сообщаем, что запись остановлена
end;

function TCapetureAudioNoLimit.GetBuffer: TWaveHdr;
begin
	Result:=FBufferHead[FCurentBuffer];
end;

function TCapetureAudioNoLimit.GetData(Index: Cardinal): TValue4b;
var
	p:PValue4b;
begin
	if Index<FSampleCount then begin
		p:=PValue4b(FBufferHead[FCurentBuffer].lpData);
    Inc(p, Index);
		Result:=p^;
  end;
end;

procedure TCapetureAudioNoLimit.StartRecord(SampleCount:Cardinal);
var
  WaveHeader:TWaveFormatEx; //Описатель формата захватываемых данных ( кол-во каналов, частота ... )
begin
  FLastError:=0;
  if not FRecordNow then begin // Если не идёт запись, то можно начинать запись
    FSampleCount:=SampleCount; // SampleCount - указывает на то, какими блоками будут поступать данные
    // Заполнение формата принимаемых данных
    with WaveHeader do begin
      wFormatTag := WAVE_FORMAT_PCM; // Звук без сжатия
      nChannels:=2;  // количество каналов
      nSamplesPerSec:=Frequency; // частота
      wBitsPerSample:=16; // Кол-во бит в семпле
      nBlockAlign:=4; // Выравнивание блоков
      nAvgBytesPerSec:=nSamplesPerSec*wBitsPerSample*nChannels div 8; // Среднее кол-во байт в секунду
      cbSize:=0;
	    FBufferHead[0].dwBufferLength:=wBitsPerSample*nChannels*SampleCount div 8; // Длина буфера данных
	    FBufferHead[1].dwBufferLength:=wBitsPerSample*nChannels*SampleCount div 8; // Длина буфера данных
    end;

    FDataFillEvent:=CreateEvent(nil, True, False, nil); // Этот сигнальный объект будет переходить в сигнальное состояние, когда данные будут готовы

    FLastError:=WaveInOpen(@FWaveIn, 0, @WaveHeader, FDataFillEvent, 0, CALLBACK_EVENT); // Открытие устройства захвата
    with FBufferHead[0] do ReallocMem(lpData, dwBufferLength); // Выделение памяти под буфер ( в этот буфер будут скопированны данные после их получения, для их дальнейшего использования )
    with FBufferHead[1] do ReallocMem(lpData, dwBufferLength); // Выделение памяти под буфер ( в этот буфер будут скопированны данные после их получения, для их дальнейшего использования )
    FLastError:=FLastError or WaveInPrepareHeader(FWaveIn, @FBufferHead[0], SizeOf(TWaveHdr)); // Подготовка первого буфера для загрузки данными ( Выполняется один раз перед началом записи )
    FLastError:=FLastError or WaveInPrepareHeader(FWaveIn, @FBufferHead[1], SizeOf(TWaveHdr)); // Подготовка второго буфера для загрузки данными ( Выполняется один раз перед началом записи )
    if FLastError=0 then begin // Если ошибок нет, то
      FThread4Rec:=TSimpleThread.Create(True);
      FThread4Rec.FreeOnTerminate:=True;
      FThread4Rec.OnExec:=@ThFunc4Rec;
      FThread4Rec.Start;
  		FRecordNow:=True; // Выставляем признак "идёт запись"
    end;
  end else
    FLastError:=MMSYSERR_ERROR; // Занят записыванием
end;

procedure TCapetureAudioNoLimit.StopRecord;
begin
  if not FRecordNow then Exit;
  FThread4Rec.Terminate;
  FThread4Rec.WaitFor;
end;

end.

