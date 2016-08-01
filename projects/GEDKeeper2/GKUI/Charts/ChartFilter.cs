/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using GKCommon;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ChartFilter : BaseObject, ICustomFilter
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
            this.Reset();
        }

        public void Reset()
        {
            this.SourceMode = FilterGroupMode.All;
            this.BranchCut = BranchCutType.None;
        }

        public void Backup()
        {
            this.fBackSourceMode = this.SourceMode;
            this.fBackSourceRef = this.SourceRef;
            this.fBackBranchCut = this.BranchCut;
            this.fBackBranchYear = this.BranchYear;
            this.fBackBranchPersons = this.BranchPersons;
        }

        public void Restore()
        {
            this.SourceMode = this.fBackSourceMode;
            this.SourceRef = this.fBackSourceRef;
            this.BranchCut = this.fBackBranchCut;
            this.BranchYear = this.fBackBranchYear;
            this.BranchPersons = this.fBackBranchPersons;
        }
    }
}
