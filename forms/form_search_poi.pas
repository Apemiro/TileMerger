unit form_search_poi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, URIParser;

type

  { TForm_PoiServer }

  TForm_PoiServer = class(TForm)
    Button_Search: TButton;
    Edit_CityParam: TEdit;
    Memo_PoiSearchEntries: TMemo;
    procedure Button_SearchClick(Sender: TObject);
  private

  public
    procedure CheckURI(Sender:TObject;const ASrc:String;var ADest:String);
  end;

var
  Form_PoiServer: TForm_PoiServer;

implementation
uses tile_merger_main, tile_merger_wmts_client, tile_merger_feature, tile_merger_projection, fphttpclient, fpjson;

{$R *.lfm}


function GCJ02_To_WGS84(const P: TGeoPoint): TGeoPoint;
const
  a  = 6378245.0;                 // Krasovsky 1940
  ee = 0.00669342162296594323;    // 偏心率平方
  pi = 3.14159265358979323846;

var
  lng, lat: Double;
  dLat, dLng: Double;
  radLat, magic, sqrtMagic: Double;

  // ---- 以下为局部内联计算（仍属于本函数） ----

  function OutOfChina(const lng, lat: Double): Boolean; inline;
  begin
    Result :=
      (lng < 72.004) or (lng > 137.8347) or
      (lat < 0.8293) or (lat > 55.8271);
  end;

  function TransformLat(x, y: Double): Double; inline;
  begin
    Result := -100.0 + 2.0*x + 3.0*y + 0.2*y*y + 0.1*x*y + 0.2*Sqrt(Abs(x));
    Result := Result
      + (20.0*Sin(6.0*x*pi) + 20.0*Sin(2.0*x*pi)) * 2.0/3.0
      + (20.0*Sin(y*pi) + 40.0*Sin(y/3.0*pi)) * 2.0/3.0
      + (160.0*Sin(y/12.0*pi) + 320.0*Sin(y*pi/30.0)) * 2.0/3.0;
  end;

  function TransformLng(x, y: Double): Double; inline;
  begin
    Result := 300.0 + x + 2.0*y + 0.1*x*x + 0.1*x*y + 0.1*Sqrt(Abs(x));
    Result := Result
      + (20.0*Sin(6.0*x*pi) + 20.0*Sin(2.0*x*pi)) * 2.0/3.0
      + (20.0*Sin(x*pi) + 40.0*Sin(x/3.0*pi)) * 2.0/3.0
      + (150.0*Sin(x/12.0*pi) + 300.0*Sin(x/30.0*pi)) * 2.0/3.0;
  end;

begin
  lng := P.lng;
  lat := P.lat;

  // 中国境外不做偏移
  if OutOfChina(lng, lat) then
  begin
    Result := P;
    Exit;
  end;

  dLat := TransformLat(lng - 105.0, lat - 35.0);
  dLng := TransformLng(lng - 105.0, lat - 35.0);

  radLat := lat / 180.0 * pi;
  magic := Sin(radLat);
  magic := 1 - ee * magic * magic;
  sqrtMagic := Sqrt(magic);

  dLat := (dLat * 180.0) / ((a * (1 - ee)) / (magic * sqrtMagic) * pi);
  dLng := (dLng * 180.0) / (a / sqrtMagic * Cos(radLat) * pi);

  // ---- GCJ02 反推 WGS84（核心一步）----
  Result.lng := lng - dLng;
  Result.lat := lat - dLat;
end;




{ TForm_PoiServer }

//Fixed by @wittbo on Lazarus Forum,
//Source: https://forum.lazarus.freepascal.org/index.php/topic,43553.msg335901.html#msg335901
procedure TForm_PoiServer.CheckURI (Sender: TObject; const ASrc: String; var ADest: String);
var newURI     : TURI;
    OriginalURI: TURI;
begin
   newURI := ParseURI (ADest, False);
   if (newURI.Host = '') then begin                         // NewURI does not contain protocol or host
      OriginalURI          := ParseURI (ASrc, False);       // use the original URI...
      OriginalURI.Path     := newURI.Path;                  // ... with the new subpage (path)...
      OriginalURI.Document := newURI.Document;              // ... and the new document info...
      ADest                := EncodeURI (OriginalURI)       // ... and return the complete redirected URI
   end
end;

procedure TForm_PoiServer.Button_SearchClick(Sender: TObject);
const //poi_url = 'http://api.map.baidu.com/place/v3/region';
      poi_url = 'http://api.map.baidu.com/geocoding/v3/';
      poi_ua  = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
var SearchEntry:string;
    SearchCity:string;
    GetURL:String;
    content:TMemoryStream;
    jData,res,loc:TJSONData;
    search_result:TJSONArray;
    poi:TJSONObject;
    tmpFT:TAGeoPointGeometry;
    tmpGP:TGeoPoint;
    httpclient:TFPHTTPClient;
    idx,len:integer;

begin
  SearchCity:=Edit_CityParam.Text;
  //临时的搜索方法，临时的要素图层
  WMTS_Client.FeatureLayers[0].Features.Clear;
  httpclient:=TFPHTTPClient.Create(nil);
  content:=TMemoryStream.Create;
  try
    httpclient.KeepConnection:=false;
    httpclient.AllowRedirect:=true;
    httpclient.OnRedirect:=@CheckURI;
    httpclient.AddHeader('User-Agent', poi_ua);
    for SearchEntry in Memo_PoiSearchEntries.Lines do begin
      //GetURL:=poi_url+'?query='+EncodeURLElement(SearchEntry)+'&region='+EncodeURLElement(SearchCity)+'&ret_coordtype=gcj02ll&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L';
      GetURL:=poi_url+'?address='+EncodeURLElement(SearchCity+SearchEntry)+'&output=json&ret_coordtype=gcj02ll&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L';
      try
        content.Clear;
        httpclient.Get(GetURL, content);
      except
        //on E:Exception do begin
        //  ShowMessage(Format('Error %s: %s',[E.ClassName, E.Message]));
        //end;
      end;
      if httpclient.ResponseStatusCode<>200 then continue;
      if content.Size=0 then continue;

      content.Position:=0;
      jData:=nil;
      jData:=GetJSON(content);
      //res:=jData.FindPath('results');
      res:=jData.FindPath('result');
      //if (res<>nil) and (res is TJSONArray) then begin
      //  search_result:=TJSONArray(res);
      //  len:=search_result.Count;
      //  for idx:=0 to len-1 do begin
          //if search_result[idx].JSONType<>jtObject then continue;
          //poi:=TJSONObject(search_result[idx]);
          if res.JSONType<>jtObject then continue;
          poi:=TJSONObject(res);
          //poi内部不作异常检测了
          loc:=poi.Find('location');
          tmpFT:=TAGeoPointGeometry.Create(2);
          tmpGP.x:=loc.FindPath('lng').AsFloat;
          tmpGP.y:=loc.FindPath('lat').AsFloat;
          tmpGP:=GCJ02_To_WGS84(tmpGP);
          tmpFT.X:=tmpGP.x;
          tmpFT.Y:=tmpGP.y;
          //tmpFT.LabelText:=Format('%s [%s,%d]',[poi.Find('name').AsString, SearchEntry, idx]);
          tmpFT.LabelText:=SearchEntry;
          WMTS_Client.FeatureLayers[0].Features.AddFeature(tmpFT);
      //  end;
      //end;
    end;
    ShowMessage(Format('共找到%d个POI',[WMTS_Client.FeatureLayers[0].Features.Count]));
    WMTS_Client.FeatureLayers[0].Features.SaveToCSV('POI_Search.csv');
  finally
    httpclient.Free;
    content.Free;
  end;

end;

end.

