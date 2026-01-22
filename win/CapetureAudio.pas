unit CapetureAudio;

interface

uses
	MMSystem, Windows;

type
	TRightLeftVal=record
  	Rigth:SmallInt;
    Left:SmallInt;
  end;
  PRightLeftVal=^TRightLeftVal;
  TArrayOfRightLeftVal=array of TRightLeftVal;

	// Класс не очень подходит для непрерывного захвата звука, т.к. предпологает кратковременное открытие устройства
  // запись звука в буфер и закрытие устройства, всё это делает одна процедура StartRecord, в качестве параметра которой
  // даётся кол-во отсчётов которое мы хоти записать ( Время записываемого звука тогда равно t = SampleCount/FFrequency )
	TCapetureAudio=class
  private
    FSampleCount:Cardinal; // Кол-во отсчётов, которое мы записывали при запуске процедуры StartRecord
		FFrequency:Cardinal; // Частота дискретизации сигнала по времени
    FBufferHead:TWaveHdr; // Переменная для хранения информации о буфере для устройства захвата звука

    function GetData(Index: Cardinal): TRightLeftVal; // см. св-во Data
  public
  	constructor Create;
    destructor Destroy; override;

		function StartRecord(SampleCount:Cardinal):MMRESULT; // SampleCount - Кол-во отсчётов, которое мы хотим записать

    property Frequency:Cardinal read FFrequency write FFrequency;
    property Data[Index:Cardinal]:TRightLeftVal read GetData; // Данным свойством можно пользоваться только после прихода события OnRecordComplite
    property SampleCount:Cardinal read FSampleCount; // Св-во возвращает кол-во записанных блоков    
    property Buffer:TWaveHdr read FBufferHead; // Для более прямого доступа к записанным данным
  end;

implementation

{ TCapetureAudio }

constructor TCapetureAudio.Create;
begin
	FBufferHead.lpData:=nil;
  FFrequency:=44100;
end;

destructor TCapetureAudio.Destroy;
begin
  ReallocMem(FBufferHead.lpData, 0);
  inherited;
end;

function TCapetureAudio.GetData(Index: Cardinal): TRightLeftVal;
var
	p:PRightLeftVal;
begin
	if Index<FSampleCount then
  	begin
			p:=Pointer(FBufferHead.lpData);
      inc(p, Index);
			Result:=p^;
    end;
end;

function TCapetureAudio.StartRecord(SampleCount:Cardinal):MMRESULT;
var
  WaveHeader:TWaveFormatEx; //Описатель формата захватываемых данных ( кол-во каналов, частота ... )
  WaveIn:hWaveIn; // Идентификатор открытого устройства захвата звука
begin
	// SampleCount - Кол-во отсчётов, которое мы хотим записать
  // время записи при этом будет равно t = SampleCount/nSamplesPerSec
  FSampleCount:=SampleCount;
	// Заполнение формата принимаемых данных
  with WaveHeader do begin
	 wFormatTag := WAVE_FORMAT_PCM; // Звук без сжатия
	 nChannels:=2;  // количество каналов
	 nSamplesPerSec:=Frequency; // частота
	 wBitsPerSample:=16; // Кол-во бит в семпле
	 nBlockAlign:=4; // Выравнивание блоков
	 nAvgBytesPerSec:=nSamplesPerSec*wBitsPerSample*nChannels shr 3; // Среднее кол-во байт в секунду
	 cbSize:=0;
  end;
  Result:=WaveInOpen(@WaveIn, 0, @WaveHeader, 0, 0, CALLBACK_NULL); // Открытие устройства захвата
	with WaveHeader do
	  FBufferHead.dwBufferLength:=wBitsPerSample*nChannels*SampleCount shr 3; // Длина буфера данных в байтах
	with FBufferHead do
		ReallocMem(lpData, dwBufferLength); // Выделение памяти под буфер ( в этот буфер будут скопированны данные после их получения, для их дальнейшего использования )
  Result:=Result or WaveInPrepareHeader(WaveIn, @FBufferHead, SizeOf(FBufferHead)); // Подготовка буфера для загрузки данными ( Выполняется один раз перед началом записи )
  Result:=Result or WaveInAddBuffer(WaveIn, @FBufferHead, SizeOf(FBufferHead)); // Установка буфера в очередь на загруску данными
  Result:=Result or WaveInStart(WaveIn); // Начало захвата данных с устройства записи
  repeat // Ожидание готовности данных
  	Sleep(1); // Это сделанно для того, чтобы не было 100% загрузки процессора
  until FBufferHead.dwFlags and WHDR_DONE<>0; // Выйдет когда будет установлен флаг готовности
	Result:=Result or waveInReset(WaveIn); // Сброс всех буферов
	Result:=Result or WaveInUnPrepareHeader(WaveIn, @FBufferHead, SizeOf(FBufferHead)); // Освобождение буфера
	Result:=Result or WaveInClose(WaveIn); // Закрытие устройства записи
end;

end.
