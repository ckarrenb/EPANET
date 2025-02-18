unit restclient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fphttpclient, opensslsockets, dialogs;

function GetImage(aUrl: String; var Image: TPicture): Boolean;
function GetString(aUrl: String; var Str: String): Boolean;

implementation

function GetImage(aUrl: String; var Image: TPicture): Boolean;
var
  LMemoryStream: TMemoryStream;
  Client: TFPHttpClient;
begin
  Result := false;
  LMemoryStream := TMemoryStream.Create;
  Client := TFPHttpClient.Create(Nil);
  try
    try
      try
        Client.AllowRedirect := true;
        Client.SimpleGet(aUrl, LMemoryStream);
      except
        raise;
      end;
    finally
      Client.Free;
    end;
    if LMemoryStream.Size > 0 then
    begin
      LMemoryStream.Position := 0;
      Image.LoadFromStream(LMemoryStream);
      Result := true;
    end;
  finally
    FreeAndNil(LMemoryStream);
  end;
end;

function GetString(aUrl: String; var Str: String): Boolean;
var
  Client: TFPHTTPClient;
  Response: TStringList;
begin
  Result := false;
  Response := TStringList.Create;
  Client := TFPHttpClient.Create(Nil);
  try
    try
      try
        Client.IOTimeout := 4000;
        Client.AllowRedirect := true;
        Client.Get(aUrl, Response);
      except
        raise;
      end;

    finally
      Client.Free;
    end;
    Str := Response.Text;
    Result := true;
  finally
    Response.Free;
  end;
end;

end.

