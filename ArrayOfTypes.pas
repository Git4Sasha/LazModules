unit ArrayOfTypes;

interface

type
  // Массивы через функции SetLength, Length ...
  TArrayOfPointer=array of Pointer;
  TArrayOfSmallInt=array of SmallInt;
	T2DArrayOfSmallInt=array of TArrayOfSmallInt;
  TArrayOfByte=array of Byte;
  TArrayOfChar=array of Char;
	T2DArrayOfByte=array of TArrayOfByte;
  TArrayOfInteger=array of Integer;
	T2DArrayOfInteger=array of TArrayOfInteger;
  TArrayOfCardinal=array of Cardinal;
	T2DArrayOfCardinal=array of TArrayOfCardinal;
  TArrayOfSingle=array of Single;
	T2DArrayOfSingle=array of TArrayOfSingle;
  TArrayOfDouble=array of Double;
	T2DArrayOfDouble=array of TArrayOfDouble;
  TArrayOfBoolean=array of Boolean;
  TArrayOfQWord=array of QWord;
  T2DArrayOfQWord=array of TArrayOfQWord;

  TValue2b=packed record
  	case Integer of
    1:(a,b:Byte);
    2:(uw:Word);
    3:(sw:SmallInt);
    4:(Str:array [0..1] of Char);
    5:(b0,b1:Byte);
  end;

  TValue4b=packed record
  	case Integer of
    1:(a,b,c,d:Byte);
    2:(Int:Int32);
    3:(Float:Single);
    4:(Str:array [0..3] of Char);
    5:(r,i:Int16);
    6:(UInt:UInt32);
    7:(rw,iw:UInt16);
    8:(rv,gv,bv,av:Byte); // в этом случае записи $AABBCCDD будет соответствовать rv=$DD; gv=$CC; bv=$BB; av=$AA
    9:(x,y:Int16);
    10:(mb:Array [0..3] of Byte);
    11:(u0_16,u1_16:UInt16);
    12:(i0_16,i1_16:Int16);
    13:(b0,b1,b2,b3:Byte);
    // 14:(w1,w2:UInt16);       // Устаревшие названия полей (можно раскоментировать, если лень менять на u0_16,u1_16)
    // 15:(si1,si2:Int16);      // Устаревшие названия полей (можно раскоментировать, если лень менять на i0_16,i1_16)
  end;

  TValue8b=packed record
    case Integer of
    1:(b0,b1,b2,b3,b4,b5,b6,b7:Byte);
    2:(bts:array [0..7] of Byte);
    3:(l4b,h4b:TValue4b);
    4:(I64:Int64);
    5:(dlb:Double);
    6:(i0_32, i1_32:Int32);
    7:(u0_32, u1_32:UInt32);
    8:(f1,f2:Single);
    9:(u64:UInt64);
    10:(u0_16,u1_16,u2_16,u3_16:UInt16);
    11:(i0_16,i1_16,i2_16,i3_16:Int16);
    12:(v4b1,v4b2:TValue4b);
    13:(chars:array [0..7] of char);
    // 14:(v1_32,v2_32:Integer);                 // Устаревшие названия полей (можно раскоментировать, если лень менять на i0_32, i1_32)
    // 15:(ui1,ui2:Cardinal);                    // Устаревшие названия полей (можно раскоментировать, если лень менять на ui1,ui2)
    // 16:(w0, w1, w2, w3:UInt16);               // Устаревшие названия полей (можно раскоментировать, если лень менять на w0, w1, w2, w3)
    // 17:(si1, si2, si3, si4:Int16);            // Устаревшие названия полей (можно раскоментировать, если лень менять на si1, si2, si3, si4)
  end;

  PValue2b=^TValue2b;
  PValue4b=^TValue4b;
  PValue8b=^TValue8b;

  TArrayOfValue2b=array of TValue2b;
  TArrayOfValue4b=array of TValue4b;
  TArrayOfValue8b=array of TValue8b;

  T2DArrayOfValue2b=array of TArrayOfValue2b;
  T2DArrayOfValue4b=array of TArrayOfValue4b;
  T2DArrayOfValue8b=array of TArrayOfValue8b;

  TArrayOfString=array of string;
  T2DArrayOfString=array of TArrayOfString;

  // Типы представленные ниже восновном изпользуются при работе с OpenGL, тут представлены типы с одинарной точностью (Single)
  PValue1f4b=^TValue1f4b;
  TValue1f4b=packed record
    case Byte of
    0:(v1:Single; v2:TValue4b;);
    1:(val:Single; r,g,b,a:Byte);
  end;
  TArrayOfValue1f4b=array of TValue1f4b;
  T2DArrayOfValue1f4b=array of array of TValue1f4b;

  PValue2f=^TValue2f;
  TValue2f=packed record
    case Byte of
    0:(v1,v2:Single);
    1:(cs,sn:Single);
    2:(Re,Im:Single);  // Координаты на комплексной области
    3:(A,B:Single);
    4:(Min,Max:Single);
    5:(SKO,Midle:Single);
    6:(X,Y:Single);
    7:(ang,dal:Single);
    8:(v1i,v2i:Integer);
    9:(vx,vy:Single);
  end;
  TArrayOfValue2f=array of TValue2f;
  T2DArrayOfValue2f=array of array of TValue2f;

  PValue2f4b=^TValue2f4b;
  TValue2f4b=packed record
    val2f:TValue2f;
    val4b:TValue4b;
  end;
  TArrayOfValue2f4b=array of TValue2f4b;
  T2DArrayOfValue2f4b=array of array of TValue2f4b;

  PValue3f=^TValue3f;
  TValue3f=packed record
    case Byte of
      0:(v1,v2,v3:Single);
      1:(X,Y,Z:Single);
      2:(B,E,Dal:Single);
      2:(fi,lym,h:Single);
      3:(c,s,c1:Single);
      4:(i0_32,i1_32,i2_32:Int32);
      5:(v2f:TValue2f; v1f:Single);
      6:(kurs,kren,tangaj:Single);
      // 7:(v1i,v2i,v3i:Int32);                       // Устаревшие названия полей (можно раскоментировать, если лень менять на i0_32, i1_32, i2_32)
  end;
  TArrayOfValue3f=array of TValue3f;
  T2DArrayOfValue3f=array of array of TValue3f;

  PValue3f4b=^TValue3f4b;
  TValue3f4b=packed record
    case Byte of
      0: (val3f:TValue3f; val4b:TValue4b;);
      1: (v1,v2,v3:Single; rv,gv,bv,av:Byte);
  end;
  TArrayOfValue3f4b=array of TValue3f4b;
  T2DArrayOfValue3f4b=array of array of TValue3f4b;

  PValue4f=^TValue4f;
  TValue4f=packed record
    case Byte of
      1:(v1,v2,v3,v4:Single);
      2:(v2f1,v2f2:TValue2f);
      3:(X,Y,Z,W:Single);
      4:(R,G,B,A:Single);
      5:(xmin,xmax,ymin,ymax:Single);
      6:(v3f:TValue3f;v1f:Single);
      7:(i0_32, i1_32, i2_32, i3_32:Integer);
      // 8:(v1i,v2i,v3i,v4i:Integer);      // Устаревшие названия полей (можно раскоментировать, если лень менять на i0_32, i1_32, i2_32, i3_32)
  end;
  TArrayOfValue4f=array of TValue4f;
  T2DArrayOfValue4f=array of array of TValue4f;

  PValue5f=^TValue5f;
  TValue5f=packed record
    case Byte of
      1:(v3f:TValue3f; v2f:TValue2f);
      2:(v1,v2,v3,v4,v5:Single);
      3:(v2f0,v2f1:TValue2f; v1f:Single);
      4:(v4f:TValue4f; v4b:TValue4b);
  end;
  TArrayOfValue5f=array of TValue5f;
  T2DArrayOfValue5f=array of array of TValue5f;

  PValue6f=^TValue6f;
  TValue6f=packed record
    case Byte of
      1:(v3f1,v3f2:TValue3f);
      2:(v1,v2,v3,v4,v5,v6:Single);
      3:(v2f:TValue2f; v4f:TValue4f);
      4:(c1,s1,c2,s2,c3,s3:Single);
      5:(b41,b42,b43,b44,b45,b46:TValue4b);
      6:(vf1,vf2,vf3,vf4:Single; vd1:Double);
  end;
  TArrayOfValue6f=array of TValue6f;
  T2DArrayOfValue6f=array of array of TValue6f;

  PValue7f=^TValue7f;
  TValue7f=packed record
    case Byte of
      1:(v3f:TValue3f; v4f:TValue4f);
      2:(v1,v2,v3,v4,v5,v6,v7:Single);
      3:(vf1,vf2,vf3,vf4,vf5:Single; vd1:Double);
  end;
  TArrayOfValue7f=array of TValue7f;
  T2DArrayOfValue7f=array of array of TValue7f;

  PValue8f=^TValue8f;
  TValue8f=packed record
    case Byte of
      1:(v3f1,v3f2:TValue3f; v2f:TValue2f);
      2:(v1,v2,v3,v4,v5,v6,v7,v8:Single);
  end;
  TArrayOfValue8f=array of TValue8f;
  T2DArrayOfValue8f=array of array of TValue8f;

  TValue9f=packed record
    case Integer of
      0:(a,b,c,d,e,f,g,h,i:Single);
      1:(m:array [0..8] of Single);
      2:(m2:array [0..2,0..2] of Single); // m2[строка (Y), столбец (X)]   // m2[0,0] = a; m2[0,1]=b; m2[0,2]=c; m2[1,0]=d; m2[1,1]=e; m2[1,2]=f; m2[2,0]=g; m2[2,1]=h; m2[2,2]=i
	end;

  PValue16f=^TValue16f;
  TValue16f=array [0..3, 0..3] of Single; // Двумерный массив
	{№ элементов и их положение, для линейного варианта массива и для двумерного массива}
	{0(0,0)  1(0,1)  2(0,2)  3(0,3)}
	{4(1,0)  5(1,1)  6(1,2)  7(1,3)}
	{8(2,0)  9(2,1)  10(2,2) 11(2,3)}
	{12(3,0) 13(3,1) 14(3,2) 15(3,3)}




  // Типы восновном изпользуются при работе с OpenGL, тут представлены типы с двойной точностью (Double)

  TValue2d=packed record
    case Byte of
    0:(v1,v2:Double);
    1:(cs,sn:Double);
    2:(Re,Im:Double);
    3:(A,B:Double);
    4:(Min,Max:Double);
    5:(SKO,Midle:Double);
    6:(X,Y:Double);
    7:(ang,dal:Double);
    8:(nx,ny:Double);
    9:(fi,lym:Double);
  end;
  PValue2d=^TValue2d;
  TArrayOfValue2d=array of TValue2d;
  T2DArrayOfValue2d=array of array of TValue2d;

  TValue3d=packed record
    case Byte of
      0:(v1,v2,v3:Double);
      1:(X,Y,Z:Double);
      2:(B,E,Dal:Double);
      3:(fi,lym,h:Double);
      4:(c,s,c1:Double);
  end;
  PValue3d=^TValue3d;
  TArrayOfValue3d=array of TValue3d;
  T2DArrayOfValue3d=array of array of TValue3d;

  TValue4d=packed record
    case Byte of
      1:(v1,v2,v3,v4:Double);
      2:(v2d1,v2d2:TValue2d);
      3:(X,Y,Z,w:Double);
      4:(R,G,B,A:Double);
      5:(xmin,xmax,ymin,ymax:Double);
  end;
  PValue4d=^TValue4d;
  TArrayOfValue4d=array of TValue4d;
  T2DArrayOfValue4d=array of array of TValue4d;

  PValue5d=^TValue5d;
  TValue5d=packed record
    case Byte of
      1:(v3d:TValue3d; v2d:TValue2d);
      2:(v1,v2,v3,v4,v5:Double);
  end;
  TArrayOfValue5d=array of TValue5d;
  T2DArrayOfValue5d=array of array of TValue5d;

  TValue9d=packed record
    case Integer of
      0:(a,b,c,d,e,f,g,h,i:Double);
      1:(m:array [0..8] of Double);
      2:(m2:array [0..2,0..2] of Double); // m2[строка (Y), столбец (X)]   // m2[0,0] = a; m2[0,1]=b; m2[0,2]=c; m2[1,0]=d; m2[1,1]=e; m2[1,2]=f; m2[2,0]=g; m2[2,1]=h; m2[2,2]=i
	end;


  // Целочисленные типы-записи

  TValue2i=packed record
    case Byte of
      0:(i1,i2:Integer);
      1:(ui1,ui2:Cardinal);
      3:(v4b1,v4b2:TValue4b);
  end;
  PValue2i=^TValue2i;
  TArrayOfValue2i=array of TValue2i;

  TValue3i=packed record
    case Byte of
      0:(i1,i2,i3:Integer);
      1:(ui1,ui2,ui3:Cardinal);
      3:(v4b1,v4b2,v4b3:TValue4b);
  end;
  PValue3i=^TValue3i;
  TArrayOfValue3i=array of TValue3i;




  // Функции для удобного создания переменных заданного типа
  function Value2b(a,b:Byte):Tvalue2b;

  function Value4b(a,b,c,d:Byte):TValue4b; overload;
  function Value4b(x,y:SmallInt):TValue4b; overload;

  function Value2i(i1,i2:Integer):TValue2i;
  function Value3i(i1,i2,i3:Integer):TValue3i;

  function Value1f4b(val:Single; r,g,b,a:Byte):TValue1f4b;
  function Value2f(v1,v2:Single):TValue2f;  inline;
  function Value2f4b(v1,v2:Single; r,g,b,a:Byte):TValue2f4b;
  function Value2d(v1,v2:Double):TValue2d;
  function Value3f(v1,v2,v3:Single):TValue3f;
  function Value3f4b(v1,v2,v3:Single; vb1,vb2,vb3,vb4:Byte):TValue3f4b;
  function Value3d(v1,v2,v3:Double):TValue3d;
  function Value4f(v1,v2,v3,v4:Single):TValue4f; overload;
  function Value4f(v2f1,v2f2:TValue2f):TValue4f; overload;
  function Value4d(v1,v2,v3,v4:Double):TValue4d; overload;
  function Value4d(v2d1,v2d2:TValue2d):TValue4d; overload;
  function Value5f(v1,v2,v3,v4,v5:Single):TValue5f;
  function Value6f(v1,v2,v3,v4,v5,v6:Single):TValue6f; overload;
  function Value6f(vf1,vf2,vf3,vf4:Single; vd1:Double):TValue6f; overload;
  function Value6f(v3f1,v3f2:TValue3f):TValue6f;  overload;
  function Value6f(v2f:TValue2f; v4f:TValue4f):TValue6f;  overload;
  function Value7f(v1,v2,v3,v4,v5,v6,v7:Single):TValue7f; overload;
  function Value7f(v1,v2,v3,v4,v5:Single; vd1:Double):TValue7f; overload;
  function Value7f(v3f:TValue3f; v4f:TValue4f):TValue7f; overload;
  function Value8f(v1,v2,v3,v4,v5,v6,v7,v8:Single):TValue8f; overload;
  function Value8f(v3f1,v3f2:TValue3f; v2f:TValue2f):TValue8f; overload;

implementation

function Value2i(i1, i2: Integer): TValue2i;
begin
  Value2i.i1:=i1;
  Value2i.i2:=i2;
end;

function Value3i(i1, i2, i3: Integer): TValue3i;
begin
  Value3i.i1:=i1;
  Value3i.i2:=i2;
  Value3i.i3:=i3;
end;

function Value1f4b(val:Single; r,g,b,a:Byte):TValue1f4b;
begin
  Value1f4b.val:=val;
  Value1f4b.r:=r;
  Value1f4b.g:=g;
  Value1f4b.b:=b;
  Value1f4b.a:=a;
end;

function Value2b(a, b: Byte): Tvalue2b;
begin
  Value2b.a:=a;
  Value2b.b:=b;
end;

function Value4b(a,b,c,d:Byte):TValue4b;
begin
  Value4b.a:=a;
  Value4b.b:=b;
  Value4b.c:=c;
  Value4b.d:=d;
end;

function Value4b(x,y:SmallInt):TValue4b;
begin
  Value4b.x:=x;
  Value4b.y:=y;
end;

function Value2f(v1,v2:Single):TValue2f; inline;
begin
  Value2f.v1:=v1;
  Value2f.v2:=v2;
end;

function Value2f4b(v1, v2: Single; r, g, b, a: Byte): TValue2f4b;
begin
  Value2f4b.val2f.v1:=v1; Value2f4b.val2f.v2:=v2; Value2f4b.val4b.a:=r; Value2f4b.val4b.b:=g; Value2f4b.val4b.c:=b; Value2f4b.val4b.d:=a;
end;

function Value2d(v1, v2: Double): TValue2d;
begin
  Value2d.v1:=v1; Value2d.v2:=v2;
end;

function Value3f(v1,v2,v3:Single):TValue3f;
begin
  Value3f.v1:=v1; Value3f.v2:=v2; Value3f.v3:=v3;
end;

function Value3f4b(v1, v2, v3: Single; vb1, vb2, vb3, vb4: Byte): TValue3f4b;
begin
  Value3f4b.val3f.v1:=v1;
  Value3f4b.val3f.v2:=v2;
  Value3f4b.val3f.v3:=v3;
  Value3f4b.val4b.rv:=vb1;
  Value3f4b.val4b.gv:=vb2;
  Value3f4b.val4b.bv:=vb3;
  Value3f4b.val4b.av:=vb4;
end;

function Value3d(v1,v2,v3:Double):TValue3d;
begin
  Value3d.v1:=v1; Value3d.v2:=v2; Value3d.v3:=v3;
end;

function Value4f(v1,v2,v3,v4:Single):TValue4f;
begin
  Value4f.v1:=v1; Value4f.v2:=v2; Value4f.v3:=v3; Value4f.v4:=v4;
end;

function Value4f(v2f1,v2f2:TValue2f):TValue4f; overload;
begin
  Value4f.v2f1:=v2f1; Value4f.v2f2:=v2f2;
end;

function Value4d(v1, v2, v3, v4: Double): TValue4d;
begin
  Value4d.v1:=v1;
  Value4d.v2:=v2;
  Value4d.v3:=v3;
  Value4d.v4:=v4;
end;

function Value4d(v2d1, v2d2: TValue2d): TValue4d;
begin
  Value4d.v2d1:=v2d1; Value4d.v2d2:=v2d2;
end;

function Value5f(v1,v2,v3,v4,v5:Single):TValue5f;
begin
  Value5f.v1:=v1; Value5f.v2:=v2; Value5f.v3:=v3; Value5f.v4:=v4; Value5f.v5:=v5;
end;

function Value6f(v1,v2,v3,v4,v5,v6:Single):TValue6f;
begin
  Value6f.v1:=v1; Value6f.v2:=v2; Value6f.v3:=v3; Value6f.v4:=v4; Value6f.v5:=v5; Value6f.v6:=v6;
end;

function Value6f(vf1, vf2, vf3, vf4: Single; vd1: Double): TValue6f;
begin
  Value6f.vf1:=vf1; Value6f.vf2:=vf2; Value6f.vf3:=vf3; Value6f.vf4:=vf4; Value6f.vd1:=vd1;
end;

function Value6f(v3f1,v3f2:TValue3f):TValue6f;
begin
  Value6f.v3f1:=v3f1; Value6f.v3f2:=v3f2;
end;

function Value6f(v2f:TValue2f; v4f:TValue4f):TValue6f;  overload;
begin
  Value6f.v2f:=v2f; Value6f.v4f:=v4f;
end;

function Value7f(v1,v2,v3,v4,v5,v6,v7:Single):TValue7f;
begin
  Value7f.v1:=v1; Value7f.v2:=v2; Value7f.v3:=v3; Value7f.v4:=v4; Value7f.v5:=v5; Value7f.v6:=v6; Value7f.v7:=v7;
end;

function Value7f(v1, v2, v3, v4, v5: Single; vd1: Double): TValue7f;
begin
  Value7f.v1:=v1; Value7f.v2:=v2; Value7f.v3:=v3; Value7f.v4:=v4; Value7f.v5:=v5; Value7f.vd1:=vd1;
end;

function Value7f(v3f:TValue3f; v4f:TValue4f):TValue7f;
begin
  Value7f.v3f:=v3f; Value7f.v4f:=v4f;
end;

function Value8f(v1,v2,v3,v4,v5,v6,v7,v8:Single):TValue8f; overload;
begin
  Value8f.v1:=v1; Value8f.v2:=v2; Value8f.v3:=v3; Value8f.v4:=v4; Value8f.v5:=v5; Value8f.v6:=v6; Value8f.v7:=v7; Value8f.v8:=v8;
end;

function Value8f(v3f1,v3f2:TValue3f; v2f:TValue2f):TValue8f; overload;
begin
  Value8f.v3f1:=v3f1; Value8f.v3f2:=v3f2; Value8f.v2f:=v2f;
end;


end.
