unit GKPluginCommon;

interface

type
   PPluginData = ^TPluginData;
   TPluginData = packed record
      Count: Integer;
      Instance: Longint;
   end;

const
  itProcName    = 0;
  itName        = 1;
  itHint        = 2;
  itInfo        = 3;
  itResID       = 4;

implementation

end.
