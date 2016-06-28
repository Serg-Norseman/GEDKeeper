using System;

using GKCommon.GEDCOM;

namespace GKCore.Commands
{
	public class CmdPersonChangeSex : CustomCommand
	{
		private string fPersonXRef;
		private TGEDCOMSex fOldSex;
		private TGEDCOMSex fNewSex;

		public CmdPersonChangeSex(UndoManager manager, GEDCOMIndividualRecord person, GEDCOMSex newValue) : base(manager)
		{
			this.fPersonXRef = person.XRef;
			this.fOldSex = person.Sex;
			this.fNewSex = newValue;
		}

		public override bool Redo()
		{
			bool Result = true;
			GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
			if (i_rec == null)
			{
				Result = false;
			}
			else
			{
				i_rec.Sex = this.fNewSex;
			}
			return Result;
		}

		public override void Undo()
		{
			GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
			if (i_rec != null)
			{
				i_rec.Sex = this.fOldSex;
			}
		}
	}
}
