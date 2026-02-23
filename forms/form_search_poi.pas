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
const poi_url = 'https://api.map.baidu.com/place/v3/region';
      poi_ua  = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
var SearchEntry:string;
    SearchCity:string;
    content:TMemoryStream;
    jData, res, loc, value:TJSONData;
    search_result:TJSONArray;
    poi:TJSONObject;
    tmpFT:TAGeoPointGeometry;

begin
  SearchCity:=Edit_CityParam.Text;
  //临时的搜索方法，临时的要素图层
  WMTS_Client.FeatureLayers[0].Features.Clear;
  for SearchEntry in Memo_PoiSearchEntries.Lines do begin
    content:=TMemoryStream.Create;
    jData:=nil;
    try
      with TFPHTTPClient.Create(nil) do try try
        AllowRedirect:=true;
        OnRedirect:=@CheckURI;
        AddHeader('User-Agent', poi_ua);
        Get(poi_url+'?query='+SearchEntry+'&region='+SearchCity+'&ak=F3JisKBLlCzkVsF74ZIUmtJLYvHWjD8L', content);
        if ResponseStatusCode<>200 then begin
          exit;
        end;
      except {silent 404} end;
      finally
        Free;
      end;
      content.Position:=0;
      jData:=GetJSON(content);
      res:=jData.FindPath('results');
      if (res<>nil) and (res is TJSONArray) then begin
        search_result:=TJSONArray(res);
        if (search_result.Count>0) and (search_result[0] is TJSONObject) then begin
          poi:=TJSONObject(search_result.Items[0]);
          //poi内部不作异常检测了
          loc:=poi.Find('location');
          tmpFT:=TAGeoPointGeometry.Create(2);
          tmpFT.Y:=loc.FindPath('lat').AsFloat;
          tmpFT.X:=loc.FindPath('lng').AsFloat;
          tmpFT.LabelText:=poi.Find('name').AsString;
          WMTS_Client.FeatureLayers[0].Features.AddFeature(tmpFT);
        end;
      end;
    finally
      if jData<>nil then jData.Free;
      content.Free;
    end;
  end;

end;

end.

