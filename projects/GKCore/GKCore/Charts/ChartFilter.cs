/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Lists;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ChartFilter : BaseObject
    {
        public enum BranchCutType
        {
            None,
            Years,
            Persons
        }

        private FilterGroupMode fBackSourceMode;
        private string fBackSourceRef;
        private BranchCutType fBackBranchCut;
        private int fBackBranchYear;
        private string fBackBranchPersons;

        public FilterGroupMode SourceMode;
        public string SourceRef;
        public BranchCutType BranchCut;
        public int BranchYear;
        public string BranchPersons;

        public ChartFilter()
        {
            Reset();
        }

        public void Reset()
        {
            SourceMode = FilterGroupMode.All;
            BranchCut = BranchCutType.None;
        }

        public void Backup()
        {
            fBackSourceMode = SourceMode;
            fBackSourceRef = SourceRef;
            fBackBranchCut = BranchCut;
            fBackBranchYear = BranchYear;
            fBackBranchPersons = BranchPersons;
        }

        public void Restore()
        {
            SourceMode = fBackSourceMode;
            SourceRef = fBackSourceRef;
            BranchCut = fBackBranchCut;
            BranchYear = fBackBranchYear;
            BranchPersons = fBackBranchPersons;
        }
    }
}
