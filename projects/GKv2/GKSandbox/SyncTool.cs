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
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKCore
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
            public GDMRecord MasterRecord;
            public GDMRecord UpdateRecord;
            public SyncState State;
            public string UpdateOldXRef;
            public string UpdateNewXRef;
        }


        public static void TreeSync(GDMTree mainTree, string fileName, TextBox logText)
        {
            logText.Clear();

            GDMTree extTree = new GDMTree();
            GDMXRefReplacer repMap = new GDMXRefReplacer();
            ExtList<SyncRecord> syncList = new ExtList<SyncRecord>(true);
            try {
                var gedcomProvider = new GEDCOMProvider(extTree);
                gedcomProvider.LoadFromFile(fileName);

                extTree.Header.Clear();

                for (int i = 0, num = extTree.RecordsCount; i < num; i++) {
                    GDMRecord rec = extTree[i];
                    syncList.Add(new SyncRecord {
                        MasterRecord = null,
                        UpdateRecord = rec,
                        State = SyncState.ssUndefined,
                        UpdateOldXRef = "",
                        UpdateNewXRef = ""
                    });
                }

                for (int i = 0, num = syncList.Count; i < num; i++) {
                    SyncRecord syncRec = syncList[i];

                    GDMRecord rec = mainTree.FindUID(syncRec.UpdateRecord.UID);

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

                for (int i = 0, num = repMap.Count; i < num; i++) {
                    GDMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                }

                for (int i = 0, num = extTree.RecordsCount; i < num; i++) {
                    GDMRecord rec = extTree[i];
                    rec.ReplaceXRefs(repMap);
                }

                for (int i = 0, num = syncList.Count; i < num; i++) {
                    SyncRecord syncRec = syncList[i];
                    if (syncRec.State == SyncState.ssHasMaster) {
                        GDMRecord rec = extTree.Extract(extTree.IndexOf(syncRec.UpdateRecord));
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
