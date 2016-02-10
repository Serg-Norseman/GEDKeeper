using BSLib;
using GKCore.Types;

namespace GKCore.Kinships
{
    using RelationSet = EnumSet<RelationKind>;

    public sealed class KinshipRec
    {
        public EnumSet<RelationKind> PrevRels;
        public EnumSet<RelationKind> CurrRels;
        public RelationKind FinRel;
        public sbyte Great;
        public sbyte Level;

        public KinshipRec(EnumSet<RelationKind> prevRels, EnumSet<RelationKind> currRels, RelationKind finRel, sbyte great, sbyte level)
		{
            this.PrevRels = prevRels;
            this.CurrRels = currRels;
            this.FinRel = finRel;
            this.Great = great;
            this.Level = level;
		}
    }
}