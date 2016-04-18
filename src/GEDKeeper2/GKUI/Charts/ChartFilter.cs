using GKCommon;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Charts
{
	/// <summary>
	/// 
	/// </summary>
    public class ChartFilter : BaseObject, ICustomFilter
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