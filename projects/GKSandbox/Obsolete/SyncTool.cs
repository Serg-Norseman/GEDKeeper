using System.Windows.Forms;
using GKCommon;
using GKCommon.GEDCOM;

namespace GKCore
{
    public class SyncTool
    {
        public enum SyncState
        {
            ssUndefined,
            ssHasMaster,
            ssNoMaster
        }

        public class SyncRecord
        {
            public GEDCOMRecord MasterRecord;
            public GEDCOMRecord UpdateRecord;
            public SyncState State;
            public string UpdateOldXRef;
            public string UpdateNewXRef;
        }

        public static void TreeSync(GEDCOMTree mainTree, string fileName, TextBox logText)
        {
            logText.Clear();

            GEDCOMTree extTree = new GEDCOMTree();
            XRefReplacer repMap = new XRefReplacer();
            ExtList<SyncRecord> sync_list = new ExtList<SyncRecord>(true);
            try
            {
                extTree.LoadFromFile(fileName);
                extTree.Header.Clear();

                int num = extTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = extTree[i];
                    sync_list.Add(new SyncRecord
                                  {
                                      MasterRecord = null,
                                      UpdateRecord = rec,
                                      State = SyncState.ssUndefined,
                                      UpdateOldXRef = "",
                                      UpdateNewXRef = ""
                                  });
                }

                int num2 = sync_list.Count;
                for (int i = 0; i < num2; i++)
                {
                    SyncRecord sync_rec = sync_list[i];

                    GEDCOMRecord rec = mainTree.FindUID(sync_rec.UpdateRecord.UID);

                    if (rec != null) {
                        sync_rec.MasterRecord = rec;
                        sync_rec.State = SyncState.ssHasMaster;
                    } else {
                        sync_rec.State = SyncState.ssNoMaster;
                        rec = extTree.Extract(extTree.IndexOf(sync_rec.UpdateRecord));
                        string newXRef = mainTree.XRefIndex_NewXRef(rec);
                        repMap.AddXRef(rec, rec.XRef, newXRef);
                        rec.XRef = newXRef;
                        rec.ResetOwner(mainTree);
                        mainTree.AddRecord(rec);
                    }
                }

                int num3 = repMap.Count;
                for (int i = 0; i < num3; i++)
                {
                    GEDCOMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                }

                int num4 = extTree.RecordsCount;
                for (int i = 0; i < num4; i++)
                {
                    GEDCOMRecord rec = extTree[i];
                    rec.ReplaceXRefs(repMap);
                }

                int num5 = sync_list.Count;
                for (int i = 0; i < num5; i++)
                {
                    SyncRecord sync_rec = sync_list[i] as SyncRecord;
                    if (sync_rec.State == SyncState.ssHasMaster)
                    {
                        GEDCOMRecord rec = extTree.Extract(extTree.IndexOf(sync_rec.UpdateRecord));
                        rec.XRef = mainTree.XRefIndex_NewXRef(rec);
                        rec.ResetOwner(mainTree);
                        mainTree.AddRecord(rec);
                        string backUID = sync_rec.MasterRecord.UID;
                        sync_rec.UpdateRecord.MoveTo(sync_rec.MasterRecord, true);
                        sync_rec.MasterRecord.UID = backUID;
                        mainTree.DeleteRecord(rec);
                    }
                }

                logText.AppendText("Sync finished\r\n");
            }
            finally
            {
                sync_list.Dispose();
                repMap.Dispose();
                extTree.Dispose();
            }
        }
    }
}
