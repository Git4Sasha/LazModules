unit USB_HID_ReadWriteWin;

// Этот модуль работает через библиотеку HID.dll это родная библиотека Windows поэтому, если работать с HID через неё
// то не нужны никакие дополнительные манипуляции HID.dll есть в системных папках Windows, драйвера для HID устройств
// тоже есть в Windows, но есть ограничения при использовании HID.dll, например непонятно как обмениваться данными через
// конкретные конечные точки (а что если хочется принимать данные от нескольких конечных точек), непонятно как передавать
// данные в 0-ю конечную точку (конечная точка для управления устройством), не понятно как получить сведения о конечных точках.
// Чтобы обойти эти ограничения есть вариант модуля USB_HID_ReadWrite, который работает через библиотеку libUSB-1.0,
// в этом случае необходима библиотека libUSB-1.0.dll для 32/64 битной системы библиотека разная. Так же необходимо заменить
// стандартный драйыер Windows для HID устройств на драйвер WinUSB, т.к. через этот драйвер работает библиотека libUSB-1.0
// для замены стандартного драйвера изпользуется программа zadig, которая для выбранного USB устройства может заменить
// драйвер, который Windows будет изпользовать при работе с этим устройством. Windows по PID и VID привязывает драйвер,
// который был указан в zadig, после чего вынимая и вставляя устройство будет подключаться выбранный драйвер.
// Чтобы вернуть родной драйвер, нужно удалить устройство из диспетчера устройств, далее вытащить и вставить устройство,
// после чего Windows привяжет к нему свой стандартный драйвер.

interface

uses
  Windows
  , Commctrl
  , Classes
  , SysUtils
  , SimpleThread
  , ArrayOfTypes
  , RingBufHelper
  ;

type
  PHIDDAttributes = ^THIDDAttributes;
  HIDD_ATTRIBUTES = record
    Size:          ULONG; // Размер структуры (это поле должно быть заполнено до вызова функции в которой учавствует пременная этого типа)
    VendorID:      Word;
    ProductID:     Word;
    VersionNumber: Word;
  end;
  THIDDAttributes = HIDD_ATTRIBUTES;

  PHIDPCaps = ^THIDPCaps;
  HIDP_CAPS = record
    Usage:                     Word;
    UsagePage:                 Word;
    InputReportByteLength:     Word;
    OutputReportByteLength:    Word;
    FeatureReportByteLength:   Word;
    Reserved:                  array [0..16] of Word;

    NumberLinkCollectionNodes: Word;

    NumberInputButtonCaps:     Word;
    NumberInputValueCaps:      Word;
    NumberInputDataIndices:    Word;

    NumberOutputButtonCaps:    Word;
    NumberOutputValueCaps:     Word;
    NumberOutputDataIndices:   Word;

    NumberFeatureButtonCaps:   Word;
    NumberFeatureValueCaps:    Word;
    NumberFeatureDataIndices:  Word;
  end;
  THIDPCaps = HIDP_CAPS;

  TDataIn=procedure(buf:PByte; cnt, epnum:Integer) of object;

  THIDReportData=record
    epn:Integer;
    len:Integer;
    buf:array [0..128] of Byte;
  end;
  TArrayOfHIDReportData=array of THIDReportData;


  { TUSBHIDReadWrite }

  TUSBHIDReadWrite=class
  private
    FHidHandleRead:THandle;                   // Идентификатор устройства для чтения
    FHidHandleWrite:THandle;                  // Идентификатор устройства для записи
    FHidCaps:THIDPCaps;                       // Информация об отчётах (репортах) которые передаёт/принимает USB устройство
    FManufactureString:string;                // Имя производителя
    FProductString:string;                    // Имя продукта
    FSerialNumberString:string;               // Строка с серийным номером
    FReadThread:TSimpleThread;                // Переменная для создания потока чтения
    FReadThreadOn:Boolean;                    // Признак того, что поток для чтения данных запущен (можно и без него обойтись (особенно если USB устройство ничего не возвращает))
    FOnDataIn:TDataIn;                        // Событие, которое возникает при приходе данных от USB устройства
    FOnNoConnect:TNotifyEvent;                // Событие, которое возникает при разрыве соединения с USB устройством
    FHidDeviceOpened:Boolean;                 // Признак того, что HID устройство открыто
    FUseRecvRingBuf:Boolean;                  // Признак того, что будет изпользоваться кольцевой буфер для входящих сообщений
    FRBHelper:TRingBufHelper;                 // Помошник для кольцевого буфера
    FDataRingBuf:TArrayOfHIDReportData;       // Массив, который будет изпользоваться для приёма входящих сообщений для кольцевого буфера


    procedure GetDeviceInfo;                  // Получение информации об открытом устройстве
    function GetUSBDeviceAllNames:TStrings;   // Функция возвращает список имён устройств
    procedure FReadFunc(Sender:TObject);      // Функция реализует поток чтения данных с USB устройства
  public
    Log:TStrings; // журнал

    constructor Create;
    destructor Destroy; override;
    function GetUSBDeviceNames(PID:Word=0; VID:Word=0):TStrings;                    // Функция формирует список имён устройств по их идентификатору
    function OpenUSBDByPID_VID(PID,VID:Word; RunReadThread:Boolean=True):Boolean;   // Открывает устройство по идентификаторам устройства VID и PID
    function OpenUSBDByName(name:string; RunReadThread:Boolean=True):Boolean;       // Открывает устройство по его полному имени, которое даётся системой
    procedure CloseHidDevice;                                                       // Закрытие устройства HID
    function RecvData(epnum:Integer; buf:PByte; tout:Integer=MaxInt):Integer;       // приём данных от USB устройства
    function SendData(epnum:Integer; buf:PByte; tout:Integer=MaxInt):Integer;       // передача данных USB устройству
    procedure SetUseRingBuf(rblen:Integer);                                         // Процедура устанавливает размер кольцевого буфера для входящих сообщений
    function ReadRingBuf(out mes:THIDReportData):Boolean;                           // Функция считывет данные из кольцевого буфера

    property ManufactureString:string read FManufactureString;                      // Имя производителя
    property ProductString:string read FProductString;                              // Имя продукта
    property SerialNumberString:string read FSerialNumberString;                    // Строка с серийным номером
    property HIDCaps:THIDPCaps read FHidCaps;
    property OnDataIn:TDataIn read FOnDataIn write FOnDataIn;                       // Событие, которое возникает при приходе данных от USB устройства
    property OnNoConnect:TNotifyEvent read FOnNoConnect write FOnNoConnect;
    property DeviceOpened:Boolean read FHidDeviceOpened;                            // Признак того что устройство открыто
    property MaxOutPackSize:Word read FHidCaps.OutputReportByteLength;
    property MaxInPackSize:Word read FHidCaps.InputReportByteLength;
  end;

implementation

const
  INVALID_HANDLE_VALUE = Cardinal(-1);


// Начинается секция для констант и функций библиотеки SetupApi.dll
const
  SetupApiModuleName = 'SetupApi.dll'; // имя библиотеки из которой берутся функции
//
// Flags controlling what is included in the device information set built
// by SetupDiGetClassDevs
//
  DIGCF_DEFAULT         = $00000001; // only valid with DIGCF_DEVICEINTERFACE
  {$EXTERNALSYM DIGCF_DEFAULT}
  DIGCF_PRESENT         = $00000002;
  {$EXTERNALSYM DIGCF_PRESENT}
  DIGCF_ALLCLASSES      = $00000004;
  {$EXTERNALSYM DIGCF_ALLCLASSES}
  DIGCF_PROFILE         = $00000008;
  {$EXTERNALSYM DIGCF_PROFILE}
  DIGCF_DEVICEINTERFACE = $00000010;
  {$EXTERNALSYM DIGCF_DEVICEINTERFACE}

type
  PSPDevInfoData = ^TSPDevInfoData;
  SP_DEVINFO_DATA = packed record // Информационная структура об устройстве
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD; // DEVINST handle
    Reserved: Pointer;
  end;
  TSPDevInfoData = SP_DEVINFO_DATA;

  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;
  SP_DEVICE_INTERFACE_DATA = packed record // Структура информации об интерфейсе устройства
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: Pointer;
  end;
  TSPDeviceInterfaceData = SP_DEVICE_INTERFACE_DATA;

  PSPDeviceInterfaceDetailDataA = ^TSPDeviceInterfaceDetailDataA;
  PSPDeviceInterfaceDetailDataW = ^TSPDeviceInterfaceDetailDataW;
  PSPDeviceInterfaceDetailData = PSPDeviceInterfaceDetailDataA;
  SP_DEVICE_INTERFACE_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    DevicePath: array [0..0] of AnsiChar;
  end;
  {$EXTERNALSYM SP_DEVICE_INTERFACE_DETAIL_DATA_A}
  SP_DEVICE_INTERFACE_DETAIL_DATA_W = packed record
    cbSize: DWORD;
    DevicePath: array [0..0] of WideChar;
  end;
  {$EXTERNALSYM SP_DEVICE_INTERFACE_DETAIL_DATA_W}
  TSPDeviceInterfaceDetailDataA = SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  TSPDeviceInterfaceDetailDataW = SP_DEVICE_INTERFACE_DETAIL_DATA_W;
  TSPDeviceInterfaceDetailData = TSPDeviceInterfaceDetailDataA;

function SetupDiGetClassDevs(ClassGuid: PGUID; const Enumerator: PChar; hwndParent: HWND; Flags: DWORD): Pointer; stdcall; external SetupApiModuleName name 'SetupDiGetClassDevsA';
function SetupDiEnumDeviceInterfaces(DeviceInfoSet: Pointer; DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID; MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): LongBool; stdcall; external SetupApiModuleName name 'SetupDiEnumDeviceInterfaces';
function SetupDiGetDeviceInterfaceDetail(DeviceInfoSet: Pointer; DeviceInterfaceData: PSPDeviceInterfaceData; DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA; DeviceInterfaceDetailDataSize: DWORD; RequiredSize: PDWORD; Device: PSPDevInfoData): LongBool; stdcall; external SetupApiModuleName name 'SetupDiGetDeviceInterfaceDetailA';
function SetupDiDestroyDeviceInfoList(DeviceInfoSet: Pointer): LongBool; stdcall; external SetupApiModuleName name 'SetupDiDestroyDeviceInfoList';

// Конец секции SetupApi

// Секция функций библиотеки HID.dll

const
  HidModuleName = 'HID.dll';

procedure HidD_GetHidGuid(var HidGuid: TGUID) stdcall; external HidModuleName name 'HidD_GetHidGuid';
function HidD_GetAttributes(HidDeviceObject: THandle; var HidAttrs: THIDDAttributes): LongBool; stdcall; external HidModuleName name 'HidD_GetAttributes';
function HidD_GetNumInputBuffers(HidDeviceObject: THandle; var NumBufs: Integer): LongBool; stdcall; external HidModuleName name 'HidD_GetNumInputBuffers';
function HidD_GetManufacturerString(HidDeviceObject: THandle; Buffer: PWideChar; BufferLength: Integer): LongBool; stdcall; external HidModuleName name 'HidD_GetManufacturerString';
function HidD_GetProductString(HidDeviceObject: THandle; Buffer: PWideChar; BufferLength: Integer): LongBool; stdcall; external HidModuleName name 'HidD_GetProductString';
function HidD_GetSerialNumberString(HidDeviceObject: THandle; Buffer: PWideChar; BufferLength: Integer): LongBool; stdcall; external HidModuleName name 'HidD_GetSerialNumberString';
function HidD_GetPreparsedData(HidDeviceObject: THandle; var PreparsedData: Pointer): LongBool; stdcall; external HidModuleName name 'HidD_GetPreparsedData';
function HidD_FreePreparsedData(PreparsedData: Pointer): LongBool; stdcall; external HidModuleName name 'HidD_FreePreparsedData';
function HidP_GetCaps(PreparsedData: Pointer; var Capabilities: THIDPCaps): LongInt; stdcall; external HidModuleName name 'HidP_GetCaps';
function HidD_GetInputReport(HidDeviceObject: THandle; Buffer: Pointer; BufferLength: ULONG): LongBool; stdcall; external HidModuleName name 'HidD_GetInputReport';
function HidD_SetOutputReport(HidDeviceObject: THandle; Buffer: Pointer; BufferLength: ULONG): LongBool; stdcall; external HidModuleName name 'HidD_SetOutputReport';

// Конец функций библиотеки HID.dll


{ TUSBHIDReadWrite }

constructor TUSBHIDReadWrite.Create;
begin
  Log:=TStringList.Create; // Для журнала
  FHidDeviceOpened:=False; // HID устройство не открыто
  CloseHidDevice;
end;

destructor TUSBHIDReadWrite.Destroy;
begin
  CloseHidDevice; // Закрываем HID устройство
  Log.Free;
  inherited;

  if FUseRecvRingBuf then begin
    FRBHelper.Free;
    FDataRingBuf:=nil;
  end;

  inherited;
end;

procedure TUSBHIDReadWrite.GetDeviceInfo;
var
  p:Pointer;
  buf:array [0..255] of WideChar;
begin
  FillChar(buf, SizeOf(buf), #0);
  if not HidD_GetManufacturerString(FHidHandleRead, buf, SizeOf(buf)) then
    Log.Add('Неудалось получить строку производителя')
  else begin
    SetLength(FManufactureString, 255);
    UnicodeToUtf8(@FManufactureString[1], 255, buf, 255);
  end;

  FillChar(buf, SizeOf(buf), #0);
  if not HidD_GetProductString(FHidHandleRead, buf, SizeOf(buf)) then
    Log.Add('Неудалось получить строку производителя')
  else
    begin
      SetLength(FProductString, 255);
      UnicodeToUtf8(@FProductString[1], 255, buf, 255);
    end;

  FillChar(buf, SizeOf(buf), #0);
  if not HidD_GetSerialNumberString(FHidHandleRead, buf, SizeOf(buf)) then
    Log.Add('Неудалось получить строку производителя')
  else
    begin
      SetLength(FSerialNumberString, 255);
      UnicodeToUtf8(@FSerialNumberString[1], 255, buf, 255);
    end;

  // Тут получаем информацию о размерах буферов для обмена с USB HID устройством
  if not HidD_GetPreparsedData(FHidHandleRead, p) then
    Log.Add('Не удалось получить параметры репортов устройства')
  else begin
    HidP_GetCaps(p, FHidCaps);
    HidD_FreePreparsedData(p);
  end;
end;

procedure TUSBHIDReadWrite.FReadFunc(Sender: TObject);  // Функция потока для чтения, приходящих данных от USB устройства
var
  tob:TSimpleThread;
  uhrw:TUSBHIDReadWrite;
  br:UInt32;
  i,epnum:Integer;
  buf:TArrayOfByte;
  pbuf:PByte;
begin
  tob:=Sender as TSimpleThread;
  uhrw:=TUSBHIDReadWrite(tob.UserPar);

  buf:=nil;
  SetLength(buf, 512);
  pbuf:=@buf[0];

  repeat
    if ReadFile(FHidHandleRead, buf[0], FHidCaps.InputReportByteLength, br, nil) then begin   // Если чтение удалось выполнить, то

      if FUseRecvRingBuf then begin                        // Если изпользуется кольцевой буфе, то данные помещаются в кольцевой буфер
        if FRBHelper.CanWrite then begin                   // Если в кольцевом буфере есть место, то можно заполнять данные
          i:=FRBHelper.Index4Write;
          FDataRingBuf[i].epn:=0;
          FDataRingBuf[i].len:=br;
          Move(pbuf^, FDataRingBuf[i].buf[0], br);
          FRBHelper.EndWrite;                              // Обязательно сообщаем, что данные записаны
        end;
      end;

      if Assigned(FOnDataIn) then   // Если назначен обработчик этого события, то
        FOnDataIn(pbuf, br, epnum); // запускаем это обработчик
    end;

    if br=-1 then Break;            // Если произошла ошибка чтения, то вероятно устройство было отключено
  until tob.CheckTerminated;        // Проверка того, что поток необходимо остановить

  if Assigned(FOnNoConnect) then FOnNoConnect(uhrw);
end;

procedure TUSBHIDReadWrite.CloseHidDevice;
begin
  if FHidDeviceOpened then begin // Если устройство открыто, то закрываем его
    if FReadThreadOn then begin // Если поток для чтения запускался, то его нужно остановить
      KillThread(FReadThread.Handle); // поток закрывается так, т.к. внутри поточной функции он постоянно висит и нет возможности выйти из функции чтения, если устройство не отправляет данные хосту
      FReadThreadOn:=False;
    end;

    CloseHandle(FHidHandleWrite);
    CloseHandle(FHidHandleRead);

    // Очистка всех остальных параметров
    FHidHandleRead:=0;
    FManufactureString:='';
    FProductString:='';
    FSerialNumberString:='';

    FHidDeviceOpened:=False; // Выставляем признак того, что устройство не открыто
  end;
end;

function TUSBHIDReadWrite.GetUSBDeviceAllNames: TStrings;
var
  HidGuid : TGuid;
  PnPHandle : Pointer;
  DevData: TSPDevInfoData;
  DeviceInterfaceData: TSPDeviceInterfaceData;
  FunctionClassDeviceData: PSPDeviceInterfaceDetailData;
  Success: LongBool;
  DevIndex: DWORD;
  BytesReturned: DWORD;
  HidName : string;
begin
  Result:=TStringList.Create;

  HidD_GetHidGuid(HidGuid); // Получить GUID для класса HID
  PnPHandle:=SetupDiGetClassDevs(@HidGuid, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE); // Получаем дескриптор PnP для HID-класса
  // Если ошибка, то выходим
  if PnPHandle = Pointer(INVALID_HANDLE_VALUE) then Exit;

  Try
    DevIndex:=0; // Индекс текущего устройства

    repeat // Цикл по всем устройствам в HID-классе
     DeviceInterfaceData.cbSize := SizeOf(TSPDeviceInterfaceData);

     // Получить информацию об интерфейсах устройства номер DevIndex
     Success := SetupDiEnumDeviceInterfaces(PnPHandle, nil, HidGuid, DevIndex, DeviceInterfaceData);
     if Success then
       begin
          BytesReturned:=0;

          // Получаем подробности об устройстве с интерфейсом DeviceInterfaceData
          // Сначала вызываем с нулевым размером буфера, получаем размер необходимого
          // буфера, потом вызываем повторно, сформировав правильный буфер
          SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData, nil, 0, @BytesReturned, nil);
          if (BytesReturned <> 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
            begin
              DevData.cbSize:=SizeOf(DevData);
              FunctionClassDeviceData:=AllocMem(BytesReturned); // Создаем буфер
              //FunctionClassDeviceData^.cbSize := 5;  // для 32-х битной ОС должно стоять это число
              FunctionClassDeviceData^.cbSize := 8;  // для 64-х битной ОС должно стоять это число

              if SetupDiGetDeviceInterfaceDetail(PnPHandle, @DeviceInterfaceData, FunctionClassDeviceData, BytesReturned, @BytesReturned, @DevData) then
                begin
                  HidName:=StrPas(PChar(@FunctionClassDeviceData^.DevicePath)); // Отобразить имя PnP-имя устройства
                  Result.Add(HidName);



                end;
              // Получаем информацию
              FreeMem(FunctionClassDeviceData); // Освободить буфер
            end;
       end;
     Inc(DevIndex); // Следующее устройство
    until not Success;
  finally
    SetupDiDestroyDeviceInfoList(PnPHandle);
  end;
end;

function TUSBHIDReadWrite.GetUSBDeviceNames(PID: Word; VID: Word): TStrings;
var
  i:Integer;
  dattr:THIDDAttributes;
  dev:TStrings;
begin
  Log.Clear;
  Result:=TStringList.Create;

  CloseHidDevice; // Если устройство было открыто, то оно будет закрыто
  dev:=GetUSBDeviceAllNames; // прочитаем имена устройств

  for i:=0 to dev.Count-1 do begin // Цикл по всем найденым HID устройствам
    FHidHandleRead:=CreateFile(PChar(dev[i]), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0); // Получаем идентификатор для получения данных от HID USB
    FHidHandleWrite:=CreateFile(PChar(dev[i]), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0); // Получаем идентификатор для передачи данных к HID USB

    if FHidHandleWrite=INVALID_HANDLE_VALUE then
      begin
        Log.Add('Устройство не открывается для записи');
        Continue;
      end;
    if FHidHandleRead=INVALID_HANDLE_VALUE then
      begin
        Log.Add('Устройство не открывается для чтения');
        CloseHandle(FHidHandleWrite);
        Continue; // Переходим к следующему устройству
      end;

    dattr.Size:=SizeOf(dattr);
    if HidD_GetAttributes(FHidHandleRead, dattr) then
      Log.Add(Format('VendorID=%d  ProductID=%d  VersionNumber=%d',[dattr.VendorID, dattr.ProductID, dattr.VersionNumber]))
    else begin
      Log.Add('Неудалось получить аттрибуты устройства');
      CloseHandle(FHidHandleRead);
      CloseHandle(FHidHandleWrite);
      Continue; // Переход в конец процедуры
    end;

    if (PID=0)and(VID=0) then // Если идентификаторы устройства нулевые, то в список добавляются все устройства
      Result.Add(dev[i])
    else begin
      if (dattr.ProductID=PID) and (dattr.VendorID=VID) then // Если PID и VID устройства совпадают с заданным для поиска, то
        Result.Add(dev[i]);
    end;

    CloseHandle(FHidHandleRead);
    CloseHandle(FHidHandleWrite);
  end;
  // Если вышли из цикла (или вообще не заходили в него), значит устройство так и не нашли, поэтому очищаем переменную FAttributes
  FHidHandleRead:=INVAliD_HANDLE_VALUE;
  FHidHandleWrite:=INVAliD_HANDLE_VALUE;
  dev.Free;
end;

function TUSBHIDReadWrite.OpenUSBDByPID_VID(PID, VID: Word; RunReadThread: Boolean): Boolean;
var
  i:Integer;
  dattr:THIDDAttributes;
  dev:TStrings;
begin
  Log.Clear;
  Result:=False;

  CloseHidDevice; // Если устройство было открыто, то оно будет закрыто
  dev:=GetUSBDeviceNames(PID, VID); // прочитаем имена устройств (только те, которые имеют заданные идентификаторы устройства)

  for i:=0 to dev.Count-1 do begin // Цикл по всем найденым HID устройствам
    Log.Add(Format('n=%d  name=%s', [i, dev[i]]));

    FHidHandleRead:=CreateFile(PChar(dev[i]), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0); // Получаем идентификатор для получения данных от HID USB
    FHidHandleWrite:=CreateFile(PChar(dev[i]), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0); // Получаем идентификатор для передачи данных к HID USB

    if FHidHandleWrite=INVALID_HANDLE_VALUE then begin
      Log.Add('Устройство не открывается для записи');
      Continue;
    end;
    if FHidHandleRead=INVALID_HANDLE_VALUE then begin
      Log.Add('Устройство не открывается для чтения');
      CloseHandle(FHidHandleWrite);
      Continue; // Переходим к следующему устройству
    end;

    dattr.Size:=SizeOf(THIDDAttributes);
    if HidD_GetAttributes(FHidHandleRead, dattr) then
      Log.Add(Format('VendorID=%d  ProductID=%d  VersionNumber=%d',[dattr.VendorID, dattr.ProductID, dattr.VersionNumber]))
    else begin
      Log.Add('Неудалось получить аттрибуты устройства');
      CloseHandle(FHidHandleRead);
      CloseHandle(FHidHandleWrite);
      Continue; // Переход в конец процедуры
    end;

    if (dattr.ProductID=PID) and (dattr.VendorID=VID) then begin // Если PID и VID устройства совпадают с заданным для поиска, то
      GetDeviceInfo; // Получение информации об открытом устройстве

      if RunReadThread then begin
        // Запуск потока, который будет принимать данные от USB-HID устройства
        // !!! ВНИМАНИЕ !!! не забудь, что в линуксе при работе с потоками необходимо подключить модуль cthreads самым первым в проекте (см. файл проекта *.lpr)
        FReadThread:=TSimpleThread.CreateSimple;
        FReadThread.StartThread(@FReadFunc, 0, Self);
        FReadThreadOn:=True;  // По этому признаку будет определяться нужно ли останавливать поток когда устройство закрывается
      end else
        FReadThreadOn:=False;  // Поток для чтения данных запущен не был, значит и останавливать его нет необходимости

      FHidDeviceOpened:=True; // Выставляем признак того, что устройство открыто
      Log.Add('Открыт доступ к устройству');
      Result:=True; // Возвращаем положительный результат
      dev.Free;
      Exit; // Считаем, что устройство открыто, поэтому выходим из процедуры (FHidHandleRead устройства остаётся открытым)
    end;
  end;
  // Если вышли из цикла (или вообще не заходили в него), значит устройство так и не нашли, поэтому очищаем переменную FAttributes
  FHidHandleRead:=INVAliD_HANDLE_VALUE;
  FHidHandleWrite:=INVAliD_HANDLE_VALUE;
  dev.Free;
end;

function TUSBHIDReadWrite.OpenUSBDByName(name: string; RunReadThread: Boolean): Boolean;
var
  i:Integer;
  darrt:THIDDAttributes;
  dev:TStrings;
begin
  Log.Clear;
  Result:=False;

  CloseHidDevice; // Если устройство было открыто, то оно будет закрыто
  dev:=GetUSBDeviceAllNames; // прочитаем имена устройств

//  \\?\hid#vid_0483&pid_5704#6&bcb9668&0&0000#{4d1e55b2-f16f-11cf-88cb-001111000030} - Пример полного имени устройства

  i:=dev.IndexOf(name);
  if i<0 then begin
    Log.Add('Заданное имя устройства не найдено');
    Exit;
  end;

  FHidHandleRead:=CreateFile(PChar(dev[i]), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0); // Получаем идентификатор для получения данных от HID USB
  FHidHandleWrite:=CreateFile(PChar(dev[i]), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0); // Получаем идентификатор для передачи данных к HID USB

  if FHidHandleWrite=INVALID_HANDLE_VALUE then begin
    Log.Add('Устройство не открывается для записи');
    dev.Free;
    Exit;
  end;
  if FHidHandleRead=INVALID_HANDLE_VALUE then begin
    Log.Add('Устройство не открывается для чтения');
    CloseHandle(FHidHandleWrite);
    dev.Free;
    Exit;
  end;

  darrt.Size:=SizeOf(THIDDAttributes);
  if HidD_GetAttributes(FHidHandleRead, darrt) then
    Log.Add(Format('VendorID=%d  ProductID=%d  VersionNumber=%d',[darrt.VendorID, darrt.ProductID, darrt.VersionNumber]))
  else begin
    Log.Add('Неудалось получить аттрибуты устройства');
    CloseHandle(FHidHandleWrite);
    CloseHandle(FHidHandleRead);
    dev.Free;
    Exit;
  end;

  GetDeviceInfo; // Получение информации об открытом устройстве, а так же формирование размеров буферов для приёма и передачи

  if RunReadThread then begin
    // Запуск потока, который будет принимать данные от USB-HID устройства
    // !!! ВНИМАНИЕ !!! не забудь, что в линуксе при работе с потоками необходимо подключить модуль cthreads самым первым в проекте (см. файл проекта *.lpr)
    FReadThread:=TSimpleThread.CreateSimple;
    FReadThread.StartThread(@FReadFunc, 0, Self);
    FReadThreadOn:=True;  // По этому признаку будет определяться нужно ли останавливать поток когда устройство закрывается
  end else
    FReadThreadOn:=False;  // Поток для чтения данных запущен не был, значит и останавливать его нет необходимости

  FHidDeviceOpened:=True; // Выставляем признак того, что устройство открыто
  Log.Add('Открыт доступ к устройству');
  Result:=True; // Возвращаем положительный результат
  dev.Free;
end;

function TUSBHIDReadWrite.RecvData(epnum: Integer; buf: PByte; tout: Integer): Integer;
var
  bread:Cardinal;
begin
  bread:=0;
  if ReadFile(FHidHandleRead, buf[0], FHidCaps.InputReportByteLength, bread, nil) then
    Result:=bread // Возвращаем количество прочитанных байт
  else
    Result:=-1;
end;

function TUSBHIDReadWrite.SendData(epnum: Integer; buf: PByte; tout: Integer): Integer;
var
  bwrite:Cardinal;
begin
  // tout - Этот параметр работает только в том случае, если работать через библиотеку libUSB (в Windows этот параметр введён для совместимости по интерфейсу с модулем для Linux)
  bwrite:=0;
  if WriteFile(FHidHandleWrite, buf[0], FHidCaps.OutputReportByteLength, bwrite, nil) then
    Result:=bwrite  // Возвращаем количество записанных байт
  else
    Result:=-1;
end;

procedure TUSBHIDReadWrite.SetUseRingBuf(rblen: Integer);
begin
  if rblen<=0 then Exit;

  FUseRecvRingBuf:=True;                    // Признак того, что будет изпользоваться кольцевой буфер для входящих сообщений
  FRBHelper:=TRingBufHelper.Create(rblen);  // Помошник для кольцевого буфера
  SetLength(FDataRingBuf, rblen);           // Массив, который будет изпользоваться для приёма входящих сообщений для кольцевого буфера
end;

function TUSBHIDReadWrite.ReadRingBuf(out mes: THIDReportData): Boolean;
begin
  Result:=FRBHelper.CanRead;
  if not Result then Exit(False);
  mes:=FDataRingBuf[FRBHelper.Index4Read];
  FRBHelper.EndRead;
end;


end.
