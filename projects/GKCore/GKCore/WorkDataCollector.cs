/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GDModel;

namespace GKCore
{
    public static class WorkDataCollector
    {
        private static void CollectIndividual(BaseContext baseContext, GDMIndividualRecord iRec)
        {
            if (iRec.HasEvents) {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];
                    baseContext.CollectEvent(evt, true);
                }
            }

            for (int i = 0, num = iRec.PersonalNames.Count; i < num; i++) {
                baseContext.CollectNameLangs(iRec.PersonalNames[i]);
            }

            baseContext.ImportNames(iRec);
        }

        private static void CollectFamily(BaseContext baseContext, GDMFamilyRecord fam)
        {
            if (fam.HasEvents) {
                for (int i = 0, num = fam.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = fam.Events[i];
                    baseContext.CollectEvent(evt, false);
                }
            }
        }

        public static async Task Collect(BaseContext baseContext)
        {
            if (baseContext == null)
                throw new ArgumentNullException(nameof(baseContext));

            await Task.Run(() => {
                try {
                    var tree = baseContext.Tree;
                    for (int i = 0, recsCount = tree.RecordsCount; i < recsCount; i++) {
                        GDMRecord rec = tree[i];

                        switch (rec.RecordType) {
                            case GDMRecordType.rtIndividual:
                                CollectIndividual(baseContext, (GDMIndividualRecord)rec);
                                break;

                            case GDMRecordType.rtFamily:
                                CollectFamily(baseContext, (GDMFamilyRecord)rec);
                                break;
                        }
                    }
                } catch (Exception ex) {
                    Logger.WriteError("WorkDataCollector.CheckFormat()", ex);
                }
            });
        }
    }
}
