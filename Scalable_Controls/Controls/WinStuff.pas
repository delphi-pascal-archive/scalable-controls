unit WinStuff;

////////////////////////////////////////////////////////////////////////////////
// Scalable  Controls - Demo                                                  //
// for Delphi Magazine                                                        //
//                                                                            //
// Copyright (c) Martin Humby 2007  Freeware                                  //
////////////////////////////////////////////////////////////////////////////////

interface
uses
  Windows, SysUtils, Types, Graphics;
type
  TBmpPixels = array[0..0] of RGBQuad;
  PPixels = ^TBmpPixels;

function GetFileMapping(Bytes: Integer): THandle;

procedure GetBitmap(WindowDC: HDC;           // Canvas.Handle
                    FileMapping: THandle;    // file mapping object or zero
                    Width,
                    Height: Integer;
                    var BmpInfo: BITMAPINFO;
                    var Bitmap: HBITMAP;
                    var BitmapDC: HDC;       // bitmap device context;
                    var PixelArray: PPixels);

procedure Marker(x, y: Integer; DC: HDC);
procedure DrawShadowedRect(DC: HDC; Left, Top, Width, Height: Integer;
                           HlPen, SdPen: HPEN); overload;

implementation

function GetFileMapping(Bytes: Integer): THandle;
begin
  Result := CreateFileMapping($FFFFFFFF,      // handle to page file
                              nil,            // security
                              PAGE_READWRITE, // protection
                              0,              // high-order DWORD of size
                              Bytes,          // low-order DWORD of size
                              nil);           // object name
end;

procedure InitBitmapInfo(out BmpInfo: BITMAPINFO; Width, Height: Integer);
begin
  FillChar(BmpInfo, SizeOf(BITMAPINFO), 0);
  with BmpInfo do begin
    bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    bmiHeader.biWidth := Width;
    bmiHeader.biHeight := Height;
    bmiHeader.biPlanes := 1;
    bmiHeader.biBitCount := 32;
    bmiHeader.biCompression := BI_RGB;
    bmiHeader.biSizeImage := Width * Height * 4;
  end;
end;

procedure GetBitmap(WindowDC: HDC;           // Canvas.Handle
                    FileMapping: THandle;    // file mapping object or zero
                    Width,
                    Height: Integer;
                    var BmpInfo: BITMAPINFO;
                    var Bitmap: HBITMAP;
                    var BitmapDC: HDC;       // bitmap device context;
                    var PixelArray: PPixels);
begin
  BitmapDC := CreateCompatibleDC(WindowDC);
  InitBitmapInfo(BmpInfo, Width, Height);
  Bitmap := CreateDIBSection(BitmapDC,
                             BmpInfo,
                             DIB_RGB_COLORS,
                             Pointer(PixelArray),
                             FileMapping,
                             0);
  SelectObject(BitmapDC, Bitmap);

end;

procedure Marker(x, y: Integer; DC: HDC);
begin
  MoveToEx(DC, x - 10, y, nil);
  LineTo(DC, x + 10, y);
  MoveToEx(DC, x, y - 10, nil);
  LineTo(DC, x, y + 10);
end;

procedure DrawShadowedRect(DC: HDC; Left, Top, Width, Height: Integer;
                           HlPen, SdPen: HPEN);
type
  TThreePoints = packed record
    P1, P2, P3: TPoint;
  end;
var
  ERect: TRect;
  Pts: TThreePoints;
begin
  ERect := Bounds(Left,Top,Width,Height);
  FillRect(DC,ERect,GetStockObject(LTGRAY_BRUSH));
  with Pts do begin
   P1.X := Left; P1.Y := Pred(Top+Height);
   P2 := ERect.TopLeft;
   P3.X := Left+Width; P3.Y := Top;
  end;
  SelectObject(DC, HlPen);
  PolyLine(DC,Pts,3);
  with Pts do begin
   P1.X := Left+1;
   Dec(P3.X);
   P2.X := P3.X; P2.Y := P1.Y;
  end;
  SelectObject(DC, SdPen);
  PolyLine(DC,Pts,3);
end;

procedure DisposeBitmap(Bitmap: HBITMAP; BitmapDC: HDC);
begin
  DeleteObject(Bitmap);
  DeleteDC(BitmapDC);
end;

end.





