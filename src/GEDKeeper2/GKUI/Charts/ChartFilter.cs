using System;

using BSLib;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Charts
{
	/// <summary>
	/// 
	/// </summary>
    public class ChartFilter : BaseObject, ICustomFilter
    {
        public enum TBranchCut
        {
            bcNone,
            bcYears,
            bcPersons
        }

        private FilterGroupMode fBackSourceMode;
        private string fBackSourceRef;
        private TBranchCut fBackBranchCut;
        private int fBackBranchYear;
        private string fBackBranchPersons;

        public FilterGroupMode SourceMode;
        public string SourceRef;
        public TBranchCut BranchCut;
        public int BranchYear;
        public string BranchPersons;

        public ChartFilter()
        {
            this.Reset();
        }

        public void Reset()
        {
            this.SourceMode = FilterGroupMode.gmAll;
            this.BranchCut = TBranchCut.bcNone;
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