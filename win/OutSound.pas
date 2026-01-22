unit OutSound;

interface

uses
  MMSystem, Windows, ArrayOfTypes;

type
	// Класс не очень подходит для непрерывного вывода звука, т.к. предпологает кратковременное открытие устройства
  // воспроизведение звука из буфера и закрытие устройства, всё это делает одна процедура StartPlay, в качестве параметра которой
  // даётся кол-во отсчётов которое мы хоти воспроизвести ( Время воспроизводимого звука тогда равно t = SampleCount/FFrequency )
  TOutSound=class
  private
    FSampleCount:Cardinal; // Кол-во отсчётов, которое мы будем воспроизводить при запуске процедуры StartPlay
		FFrequency:Cardinal; // Частота дискретизации сигнала по времени
    FBufferHead:TWaveHdr; // Буфер для хранения данных

    function GetData(Index: Cardinal): T4Bytes;
    procedure SetData(Index: Cardinal; const Value: T4Bytes); // Переменная для хранения информации о буфере для устройства вывода звука
    procedure SetSampleCount(const Value: Cardinal);

  public
  	constructor Create;
    destructor Destroy; override;

		function StartPlay:MMRESULT; // Воспроизводит звук записанный в буфере

    property Frequency:Cardinal read FFrequency write FFrequency;
    property Data[Index:Cardinal]:T4Bytes read GetData write SetData; // Для заполнения буфера, который мы будем воспроизводить
    property SampleCount:Cardinal read FSampleCount write SetSampleCount; // Св-во для определения/устаноки объёма воспроизводимых данных
    property Buffer:TWaveHdr read FBufferHead; // Для более прямого доступа к буферу ( св-вом можно пользоваться только после установки SampleCount )
  end;

implementation

{ TOutSound }

constructor TOutSound.Create;
begin
  FFrequency:=44100;
  FSampleCount:=0;
  FBufferHead.lpData:=nil;
end;

destructor TOutSound.Destroy;
begin
  ReallocMem(FBufferHead.lpData, 0);
  inherited;
end;

function TOutSound.GetData(Index: Cardinal): T4Bytes;
var
	p:P4Bytes;
begin
	if Index<FSampleCount then
  	begin
			p:=Pointer(FBufferHead.lpData);
      inc(p, Index);
			Result:=p^;
    end;
end;

procedure TOutSound.SetData(Index: Cardinal; const Value: T4Bytes);
var
	p:P4Bytes;
begin
	if Index<FSampleCount then
  	begin
			p:=Pointer(FBufferHead.lpData);
      Inc(p, Index);
			p^:=Value;
    end;
end;

procedure TOutSound.SetSampleCount(const Value: Cardinal);
begin
  // FSampleCount - Кол-во отсчётов
  FSampleCount := Value;
  ReallocMem(FBufferHead.lpData, FSampleCount*4); // Один отсчёт занимает 4 байта. 2 байта ( 16 Бит ) на канал, 2 канала
  FBufferHead.dwBufferLength:=FSampleCount*4;
end;

function TOutSound.StartPlay: MMRESULT;
var
  WaveHeader:TWaveFormatEx; //Описатель формата воспроизводимых данных ( кол-во каналов, частота ... )
  WaveOut:hWaveOut; // Идентификатор открытого для воспроизведения звука устройства
begin
  // время звучания равно t = SampleCount/Frequency секунд

	// Заполнение формата воспроизводимых данных
  with WaveHeader do begin
	 wFormatTag:=WAVE_FORMAT_PCM; // Звук без сжатия
	 nChannels:=2;  // количество каналов
	 nSamplesPerSec:=Frequency; // частота
	 wBitsPerSample:=16; // Кол-во бит в отчёте
	 nBlockAlign:=4; // Выравнивание блоков
	 nAvgBytesPerSec:=nSamplesPerSec*wBitsPerSample*nChannels shr 3; // Среднее кол-во байт в секунду
	 cbSize:=0;
  end;
  Result:=WaveOutOpen(@WaveOut, 0, @WaveHeader, 0, 0, CALLBACK_NULL); // Открытие устройства воспроизведения
  Result:=Result or WaveOutPrepareHeader(WaveOut, @FBufferHead, SizeOf(FBufferHead)); // Подготовка буфера для проигрывания данных ( Выполняется один раз перед началом проигрывания )
  Result:=Result or waveOutWrite(WaveOut, @FBufferHead, SizeOf(TWaveHdr)); // Установка буфера в очередь на воспроизведение, если очередь пуста, то воспроизведение начинается немедленно
  repeat // Ждём пока идёт воспроизведение
    Sleep(1); // Это сделанно для того, чтобы не было 100% загрузки процессора
  until FBufferHead.dwFlags and WHDR_DONE<>0; // Выйдет когда будет установлен флаг готовности
	Result:=Result or waveOutReset(WaveOut); // Сброс всех буферов ( на всякий случий )
	Result:=Result or WaveOutUnPrepareHeader(WaveOut, @FBufferHead, SizeOf(FBufferHead)); // Освобождение буфера
	Result:=Result or WaveOutClose(WaveOut); // Закрытие устройства воспроизведения
end;

end.
