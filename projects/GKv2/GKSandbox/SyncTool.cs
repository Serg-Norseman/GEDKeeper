/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2011 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Windows.Forms;
using BSLib;
using GKCommon.GEDCOM;

namespace GKCommon.GKCore
{
    public static class SyncTool
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
            ExtList<SyncRecord> syncList = new ExtList<SyncRecord>(true);
            try {
                var gedcomProvider = new GEDCOMProvider(extTree);
                gedcomProvider.LoadFromFile(fileName);

                extTree.Header.Clear();

                int num = extTree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GEDCOMRecord rec = extTree[i];
                    syncList.Add(new SyncRecord {
                        MasterRecord = null,
                        UpdateRecord = rec,
                        State = SyncState.ssUndefined,
                        UpdateOldXRef = "",
                        UpdateNewXRef = ""
                    });
                }

                int num2 = syncList.Count;
                for (int i = 0; i < num2; i++) {
                    SyncRecord syncRec = syncList[i];

                    GEDCOMRecord rec = mainTree.FindUID(syncRec.UpdateRecord.UID);

                    if (rec != null) {
                        syncRec.MasterRecord = rec;
                        syncRec.State = SyncState.ssHasMaster;
                    } else {
                        syncRec.State = SyncState.ssNoMaster;
                        rec = extTree.Extract(extTree.IndexOf(syncRec.UpdateRecord));
                        string newXRef = mainTree.XRefIndex_NewXRef(rec);
                        repMap.AddXRef(rec, rec.XRef, newXRef);
                        rec.XRef = newXRef;
                        rec.ResetOwner(mainTree);
                        mainTree.AddRecord(rec);
                    }
                }

                int num3 = repMap.Count;
                for (int i = 0; i < num3; i++) {
                    GEDCOMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                }

                int num4 = extTree.RecordsCount;
                for (int i = 0; i < num4; i++) {
                    GEDCOMRecord rec = extTree[i];
                    rec.ReplaceXRefs(repMap);
                }

                int num5 = syncList.Count;
                for (int i = 0; i < num5; i++) {
                    SyncRecord syncRec = syncList[i];
                    if (syncRec.State == SyncState.ssHasMaster) {
                        GEDCOMRecord rec = extTree.Extract(extTree.IndexOf(syncRec.UpdateRecord));
                        rec.XRef = mainTree.XRefIndex_NewXRef(rec);
                        rec.ResetOwner(mainTree);
                        mainTree.AddRecord(rec);
                        string backUID = syncRec.MasterRecord.UID;
                        syncRec.UpdateRecord.MoveTo(syncRec.MasterRecord, true);
                        syncRec.MasterRecord.UID = backUID;
                        mainTree.DeleteRecord(rec);
                    }
                }

                logText.AppendText("Sync finished\r\n");
            } finally {
                syncList.Dispose();
                repMap.Dispose();
                extTree.Dispose();
            }
        }
    }
}
