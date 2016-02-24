using System.Collections.Generic;

using BSLib;
using GKCommon.GEDCOM;

namespace GKCore.Types
{
    public sealed class PatriarchObj : BaseObject
    {
        public GEDCOMIndividualRecord IRec;
        public int BirthYear;
        public int DescendantsCount;
        public int DescGenerations;
        public List<PatriarchObj> Links = new List<PatriarchObj>();
        public bool HasLinks;
    }
}