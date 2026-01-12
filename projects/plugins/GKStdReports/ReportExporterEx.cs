/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Export;

namespace GKStdReports
{
    public abstract class ReportExporterEx : ReportExporter
    {
        public ReportExporterEx(IBaseWindow baseWin, bool albumPage) : base(baseWin, albumPage)
        {
        }

        protected static string GetName(GDMIndividualRecord iRec)
        {
            return GKUtils.GetNameString(iRec, true, false);
        }

        protected static string Localize(PLS lsid, params object[] args)
        {
            return string.Format(SRLangMan.LS(lsid), args);
        }
    }
}
