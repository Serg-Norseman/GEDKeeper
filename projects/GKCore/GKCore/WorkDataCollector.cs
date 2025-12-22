/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
